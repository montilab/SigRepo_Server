# Build the Rummagene catalog from Rummagene's published GMT.
#
# Pipeline, per gene set:
#   parse GMT line -> PMC id -> MeSH -> organism + assay_type -> symbols ->
#   Ensembl ids -> every id present in transcriptomics_features -> store
#
# A set that fails any step is counted and discarded. Nothing is inferred: see
# the governing rule in specs/2026-08-31-rummagene-catalog-design.md.
#
# Depends on api/lib/rummagene_ingest.R and api/lib/rummagene_catalog.R.

RUMMAGENE_GMT_URL <- base::Sys.getenv(
  "RUMMAGENE_GMT_URL",
  unset = "https://rummagene.com/latest.gmt"
)

# Download the GMT to `dest` if it is not already there. ~700MB.
download_rummagene_gmt <- function(dest, url = RUMMAGENE_GMT_URL, overwrite = FALSE) {
  if (base::file.exists(dest) && !overwrite) {
    return(dest)
  }
  utils::download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
  dest
}

# Every distinct PMC id in the GMT, streamed. Needed up front so MeSH can be
# fetched in batches rather than one request per gene set.
rummagene_gmt_pmcids <- function(gmt_path, chunk_size = 5000) {
  con <- base::file(gmt_path, open = "r")
  on.exit(base::close(con), add = TRUE)

  seen <- base::new.env(parent = base::emptyenv())
  repeat {
    lines <- base::readLines(con, n = chunk_size, warn = FALSE)
    if (base::length(lines) == 0) break
    for (ln in lines) {
      parsed <- rummagene_parse_gmt_line(ln)
      if (!base::is.null(parsed)) {
        base::assign(parsed$pmcid, TRUE, envir = seen)
      }
    }
  }
  base::ls(seen)
}

# `articles_by_pmcid[[pmcid]]`, tolerating either shape callers may supply:
# the hashed environment rummagene_fetch_articles_by_pmcid() returns in
# production (O(1) lookup -- required at ~188k papers, see that function's
# header), or a plain named list, which is what a small hand-built test
# fixture or a cached/serialized pass naturally is. Both return NULL (never
# an error) for a pmcid absent from either shape.
.rummagene_article_lookup <- function(articles_by_pmcid, pmcid) {
  if (base::is.environment(articles_by_pmcid)) {
    base::mget(pmcid, envir = articles_by_pmcid, ifnotfound = base::list(NULL))[[1]]
  } else {
    articles_by_pmcid[[pmcid]]
  }
}

# The build. `articles_by_pmcid` may be supplied (tests, or a cached pass); when
# NULL it is fetched from PubMed. Each record is
# list(pmid, mesh, title, year, doi).
build_rummagene_catalog <- function(conn, gmt_path, gmt_version,
                                    articles_by_pmcid = NULL, chunk_size = 5000,
                                    progress = TRUE) {
  if (base::is.null(articles_by_pmcid)) {
    if (progress) base::message("Collecting PMC ids from the GMT...")
    pmcids <- rummagene_gmt_pmcids(gmt_path, chunk_size = chunk_size)
    if (progress) base::message("  ", base::length(pmcids), " distinct papers; fetching from PubMed...")
    articles_by_pmcid <- rummagene_fetch_articles_by_pmcid(pmcids)
  }

  organism_id <- lookup_id(conn, "organisms", "organism_id", "organism", RUMMAGENE_CATALOG_ORGANISM)
  if (base::is.null(organism_id)) {
    base::stop(
      "Organism '", RUMMAGENE_CATALOG_ORGANISM, "' is not in the organisms table, ",
      "so no Rummagene set can be catalogued. Seed it from mysql/data/organisms.csv first."
    )
  }

  # Invariant: qualified + sum(unlist(rejected)) == examined. `examined`
  # counts lines that rummagene_parse_gmt_line() turned into a candidate --
  # it does NOT count a line that could not even become one (blank, fewer
  # than 3 fields, no PMC id in the term, or zero genes after cleanup).
  # Those are tallied separately as `unparsed`, because they never reached
  # any gate and so cannot be attributed to a specific rejection reason the
  # way a real candidate's rejection can. Counting them at all (rather than
  # letting them vanish between `examined` and every `rejected` bucket)
  # matters because a truncated download or a malformed future release would
  # otherwise under-report with no signal.
  #
  # `unstorable` sits outside this invariant too, the same way `unparsed`
  # does, but at the OTHER end of the pipeline: it is NOT added to `examined`,
  # `qualified`, or any `rejected` bucket. It counts sets that passed every
  # gate below -- a real, mappable, in-scope gene set -- but that
  # rummagene_catalog_upsert() could not actually persist (an over-length
  # term, or a character this table's charset cannot encode; see that
  # function's header). So `qualified` still means exactly "passed the
  # gate", and it does NOT mean "landed in the table" -- the true row count
  # written is `qualified - unstorable`, not `qualified` itself.
  reasons <- c("no_mesh", "organism", "assay_type", "too_few_genes", "unmapped_symbol", "feature_absent")
  rejected <- stats::setNames(base::as.list(base::rep(0L, base::length(reasons))), reasons)
  unparsed <- 0L
  unstorable <- 0L
  examined <- 0L
  qualified <- 0L
  batch <- base::list()

  flush_batch <- function() {
    if (base::length(batch) > 0) {
      rummagene_catalog_upsert(conn, batch, gmt_version = gmt_version,
                               on_unstorable = function(row, message) {
                                 unstorable <<- unstorable + 1L
                               })
      batch <<- base::list()
    }
  }

  con <- base::file(gmt_path, open = "r")
  on.exit(base::close(con), add = TRUE)

  repeat {
    lines <- base::readLines(con, n = chunk_size, warn = FALSE)
    if (base::length(lines) == 0) break

    for (ln in lines) {
      parsed <- rummagene_parse_gmt_line(ln)
      if (base::is.null(parsed)) { unparsed <- unparsed + 1L; next }
      examined <- examined + 1L

      article <- .rummagene_article_lookup(articles_by_pmcid, parsed$pmcid) %||% base::list()
      mesh <- article$mesh %||% base::character(0)
      if (base::length(mesh) == 0) { rejected$no_mesh <- rejected$no_mesh + 1L; next }

      organism <- rummagene_mesh_organism(mesh)
      if (base::is.null(organism) || !base::identical(organism, RUMMAGENE_CATALOG_ORGANISM)) {
        rejected$organism <- rejected$organism + 1L; next
      }
      assay_type <- rummagene_mesh_assay_type(mesh)
      if (base::is.null(assay_type) || !base::identical(assay_type, "transcriptomics")) {
        rejected$assay_type <- rejected$assay_type + 1L; next
      }

      # The spec's floor (Architecture step 5): a set qualifies on
      # "unambiguous organism, unambiguous assay type, >= 2 genes".
      # rummagene_parse_gmt_line() only ever drops a ZERO-gene set (there is
      # no set at all to speak of below that) -- it does not, and cannot,
      # enforce this >= 2 floor itself, since it runs before organism/
      # assay_type are known and has no way to tell a genuinely single-gene
      # published set apart from one this pipeline just hasn't rejected yet.
      # Without this check a one-gene set whose lone symbol happens to map
      # and resolve would reach the catalog. Checked here, cheaply, before
      # the mapIds()/DB round trip in rummagene_gate() below runs on a set
      # that can never qualify regardless of what it finds.
      if (base::length(parsed$genes) < 2L) {
        rejected$too_few_genes <- rejected$too_few_genes + 1L; next
      }

      # KNOWN COST, PARKED (Task 7 review, Finding 4): rummagene_gate() calls
      # AnnotationDbi::mapIds() and does a transcriptomics_features round
      # trip via rummagene_resolve_features(), UNBATCHED, once per gene set
      # that reaches this point -- roughly 155,000 times against the real
      # GMT. Left as is: this is a weekly offline job, batching would mean
      # restructuring the gate's all-or-nothing contract, and that work
      # should be driven by real timing from a production run rather than
      # done speculatively.
      gated <- rummagene_gate(conn, parsed, organism = organism, organism_id = organism_id)
      if (!base::isTRUE(gated$ok)) {
        rejected[[gated$reason]] <- rejected[[gated$reason]] + 1L; next
      }

      batch[[base::length(batch) + 1L]] <- base::list(
        term = parsed$term, pmcid = parsed$pmcid,
        pmid  = article$pmid  %||% NA_character_,
        title = article$title %||% NA_character_,
        year  = article$year  %||% NA_integer_,
        doi   = article$doi   %||% NA_character_,
        description = parsed$description, organism = organism, assay_type = assay_type,
        mesh_evidence = base::paste(base::intersect(mesh, c(
          base::names(RUMMAGENE_MESH_ORGANISM), base::names(RUMMAGENE_MESH_ASSAY)
        )), collapse = ", "),
        gene_symbols = parsed$genes, feature_names = gated$feature_names
      )
      qualified <- qualified + 1L

      if (base::length(batch) >= 500) flush_batch()
    }
    if (progress) base::message("  examined ", examined, ", qualified ", qualified)
  }
  flush_batch()

  rummagene_catalog_prune(conn, gmt_version = gmt_version)
  base::list(examined = examined, qualified = qualified, rejected = rejected,
            unparsed = unparsed, unstorable = unstorable)
}
