# New read-only query logic backing the MCP tools in mcp/lib/tools.R.
# Depends on api/lib/common.R (db_connect_local) and, for compare_two_signatures,
# api/lib/signature.R (fetch_signature_contexts, signature_similarity_summary).

list_vocabulary <- function(conn) {
  organism <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT o.organism FROM organisms o
    INNER JOIN signatures s ON s.organism_id = o.organism_id
    ORDER BY o.organism
  ")$organism

  phenotype <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT p.phenotype FROM phenotypes p
    INNER JOIN signatures s ON s.phenotype_id = p.phenotype_id
    ORDER BY p.phenotype
  ")$phenotype

  sample_type <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT st.sample_type FROM sample_types st
    INNER JOIN signatures s ON s.sample_type_id = st.sample_type_id
    ORDER BY st.sample_type
  ")$sample_type

  platform <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT pl.platform_name FROM platforms pl
    INNER JOIN signatures s ON s.platform_id = pl.platform_id
    ORDER BY pl.platform_name
  ")$platform_name

  assay_type <- DBI::dbGetQuery(conn, "SELECT DISTINCT assay_type FROM signatures ORDER BY assay_type")$assay_type

  base::list(
    organism = organism,
    phenotype = phenotype,
    sample_type = sample_type,
    platform = platform,
    assay_type = assay_type
  )
}

# is_admin controls whether hidden (visibility = 0) signatures are included.
# Search deliberately does NOT consult the per-signature signature_access ACL
# the way fetch_signature_context() does for a single record -- that's an
# expensive join across every candidate row. A non-admin user with explicit
# access to a hidden signature can still retrieve it directly via
# get_signature_context if they already know its hashkey; it just won't
# surface via search. Under-returning is the safe default here.
search_signatures <- function(conn, organism = NULL, phenotype = NULL, assay_type = NULL,
                               keyword = NULL, limit = 20, is_admin = FALSE) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 20
  }
  limit <- base::min(limit, 100)

  query <- "
    SELECT s.signature_hashkey, s.signature_name, o.organism, p.phenotype,
           s.assay_type, s.description,
           (SELECT COUNT(*) FROM signature_feature_set sfs WHERE sfs.signature_id = s.signature_id) AS feature_count
    FROM signatures s
    LEFT JOIN organisms o ON s.organism_id = o.organism_id
    LEFT JOIN phenotypes p ON s.phenotype_id = p.phenotype_id
    WHERE 1=1
  "

  if (!is_admin) {
    query <- base::paste(query, "AND s.visibility = 1")
  }
  if (!is.null(organism) && base::nzchar(base::trimws(organism[1]))) {
    query <- base::paste(query, "AND o.organism =", DBI::dbQuoteLiteral(conn, base::trimws(organism[1])))
  }
  if (!is.null(phenotype) && base::nzchar(base::trimws(phenotype[1]))) {
    query <- base::paste(query, "AND p.phenotype =", DBI::dbQuoteLiteral(conn, base::trimws(phenotype[1])))
  }
  if (!is.null(assay_type) && base::nzchar(base::trimws(assay_type[1]))) {
    query <- base::paste(query, "AND s.assay_type =", DBI::dbQuoteLiteral(conn, base::trimws(assay_type[1])))
  }
  if (!is.null(keyword) && base::nzchar(base::trimws(keyword[1]))) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", base::trimws(keyword[1])))
    query <- base::paste(query, base::sprintf(
      "AND (s.signature_name LIKE %s OR s.description LIKE %s OR s.keywords LIKE %s)",
      like, like, like
    ))
  }

  query <- base::paste(query, "ORDER BY s.signature_name ASC LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

# is_admin controls whether hidden (visibility = 0) collections are included,
# mirroring search_signatures' visibility handling above.
search_collections <- function(conn, collection_name = NULL, user_name = NULL,
                                keyword = NULL, limit = 20, is_admin = FALSE) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 20
  }
  limit <- base::min(limit, 100)

  query <- "
    SELECT c.collection_id, c.collection_name, c.description, c.user_name, c.date_created,
           (SELECT COUNT(*) FROM signature_collection_access sca WHERE sca.collection_id = c.collection_id) AS signature_count
    FROM collection c
    WHERE 1=1
  "

  if (!is_admin) {
    query <- base::paste(query, "AND c.visibility = 1")
  }
  if (!is.null(collection_name) && base::nzchar(base::trimws(collection_name[1]))) {
    query <- base::paste(query, "AND c.collection_name =", DBI::dbQuoteLiteral(conn, base::trimws(collection_name[1])))
  }
  if (!is.null(user_name) && base::nzchar(base::trimws(user_name[1]))) {
    query <- base::paste(query, "AND c.user_name =", DBI::dbQuoteLiteral(conn, base::trimws(user_name[1])))
  }
  if (!is.null(keyword) && base::nzchar(base::trimws(keyword[1]))) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", base::trimws(keyword[1])))
    query <- base::paste(query, base::sprintf(
      "AND (c.collection_name LIKE %s OR c.description LIKE %s)",
      like, like
    ))
  }

  query <- base::paste(query, "ORDER BY c.collection_name ASC LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

# geneset_resources is catalog metadata with no per-row ACL -- any valid
# api_key can browse it. Defaults to current-only rows since superseded
# resource versions aren't useful to an agent picking a geneset to run
# enrichment against.
search_geneset_resources <- function(conn, source = NULL, species = NULL, collection = NULL,
                                      subcollection = NULL, current_only = TRUE, limit = 50) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 50
  }
  limit <- base::min(limit, 200)

  query <- "
    SELECT geneset_resource_id, source, species, collection, subcollection,
           version, source_version, n_genesets, n_features, is_current, notes
    FROM geneset_resources
    WHERE 1=1
  "

  if (base::isTRUE(current_only)) {
    query <- base::paste(query, "AND is_current = 1")
  }
  if (!is.null(source) && base::nzchar(base::trimws(source[1]))) {
    query <- base::paste(query, "AND source =", DBI::dbQuoteLiteral(conn, base::trimws(source[1])))
  }
  if (!is.null(species) && base::nzchar(base::trimws(species[1]))) {
    query <- base::paste(query, "AND species =", DBI::dbQuoteLiteral(conn, base::trimws(species[1])))
  }
  if (!is.null(collection) && base::nzchar(base::trimws(collection[1]))) {
    query <- base::paste(query, "AND collection =", DBI::dbQuoteLiteral(conn, base::trimws(collection[1])))
  }
  if (!is.null(subcollection) && base::nzchar(base::trimws(subcollection[1]))) {
    query <- base::paste(query, "AND subcollection =", DBI::dbQuoteLiteral(conn, base::trimws(subcollection[1])))
  }

  query <- base::paste(query, "ORDER BY source, species, collection, subcollection LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

# Individual gene sets within a resource, e.g. "HALLMARK_APOPTOSIS" inside
# MSigDB's H collection. Joined with the parent resource's identifying
# columns so a result is self-describing without a second lookup.
search_geneset_entries <- function(conn, geneset_resource_id = NULL, source = NULL,
                                    species = NULL, collection = NULL, subcollection = NULL,
                                    keyword = NULL, limit = 20) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 20
  }
  limit <- base::min(limit, 100)

  query <- "
    SELECT ge.geneset_entry_id, ge.geneset_name, ge.description, ge.n_features,
           gr.geneset_resource_id, gr.source, gr.species, gr.collection, gr.subcollection, gr.version
    FROM geneset_entries ge
    INNER JOIN geneset_resources gr ON ge.geneset_resource_id = gr.geneset_resource_id
    WHERE 1=1
  "

  if (!is.null(geneset_resource_id) && base::nzchar(base::trimws(base::as.character(geneset_resource_id[1])))) {
    query <- base::paste(query, "AND ge.geneset_resource_id =", DBI::dbQuoteLiteral(conn, base::as.integer(geneset_resource_id[1])))
  }
  if (!is.null(source) && base::nzchar(base::trimws(source[1]))) {
    query <- base::paste(query, "AND gr.source =", DBI::dbQuoteLiteral(conn, base::trimws(source[1])))
  }
  if (!is.null(species) && base::nzchar(base::trimws(species[1]))) {
    query <- base::paste(query, "AND gr.species =", DBI::dbQuoteLiteral(conn, base::trimws(species[1])))
  }
  if (!is.null(collection) && base::nzchar(base::trimws(collection[1]))) {
    query <- base::paste(query, "AND gr.collection =", DBI::dbQuoteLiteral(conn, base::trimws(collection[1])))
  }
  if (!is.null(subcollection) && base::nzchar(base::trimws(subcollection[1]))) {
    query <- base::paste(query, "AND gr.subcollection =", DBI::dbQuoteLiteral(conn, base::trimws(subcollection[1])))
  }
  if (!is.null(keyword) && base::nzchar(base::trimws(keyword[1]))) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", base::trimws(keyword[1])))
    query <- base::paste(query, base::sprintf(
      "AND (ge.geneset_name LIKE %s OR ge.description LIKE %s)",
      like, like
    ))
  }

  query <- base::paste(query, "ORDER BY ge.geneset_name ASC LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

# Maps the same assay_type vocabulary search_signatures already uses onto
# each omic type's feature reference table. "snps" intentionally maps to
# genetic_variants_features (not the older snps_features table) -- that's
# the table SigRepo::searchGeneticVariantsFeatureSet() actually queries
# today. methylomics has no feature reference table and isn't supported here.
.feature_table_by_assay_type <- base::list(
  transcriptomics = "transcriptomics_features",
  proteomics = "proteomics_features",
  snps = "genetic_variants_features"
)

search_features <- function(conn, assay_type, feature_name = NULL, organism = NULL,
                             feature_database = NULL, limit = 50) {
  assay_type <- base::trimws(base::tolower(assay_type[1]))
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 50
  }
  limit <- base::min(limit, 200)

  if (assay_type == "metabolomics") {
    return(search_metabolomics_features(
      conn,
      feature_database = feature_database,
      feature_name = feature_name,
      limit = limit
    ))
  }

  table_name <- .feature_table_by_assay_type[[assay_type]]
  if (base::is.null(table_name)) {
    stop(base::sprintf(
      "Unsupported assay_type '%s'. Use one of: %s.",
      assay_type,
      base::paste(c(base::names(.feature_table_by_assay_type), "metabolomics"), collapse = ", ")
    ))
  }

  query <- base::sprintf("
    SELECT f.feature_name, o.organism, f.is_current, f.version
    FROM %s f
    LEFT JOIN organisms o ON f.organism_id = o.organism_id
    WHERE f.is_current = 1
  ", table_name)

  if (!is.null(feature_name) && base::nzchar(base::trimws(feature_name[1]))) {
    query <- base::paste(query, "AND f.feature_name =", DBI::dbQuoteLiteral(conn, base::trimws(feature_name[1])))
  }
  if (!is.null(organism) && base::nzchar(base::trimws(organism[1]))) {
    query <- base::paste(query, "AND o.organism =", DBI::dbQuoteLiteral(conn, base::trimws(organism[1])))
  }

  query <- base::paste(query, "ORDER BY f.feature_name ASC LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

# metabolomics has no single feature_name column to match against -- which
# identifier namespace is being searched (refmet/hmdb/smiles/inchikey) has to
# be chosen explicitly, mirroring SigRepo::searchMetabolomicsFeatureSet()'s
# feature_database argument. Only queries metabolite_reference columns
# confirmed in mysql/schema/metabolite_reference.sql (metabolite_id,
# refmet_name, is_current, version) -- if the live DB has since grown
# refmet_id/hmdb_id columns beyond that file, this can be extended to return
# them too.
search_metabolomics_features <- function(conn, feature_database, feature_name = NULL, limit = 50) {
  if (base::is.null(feature_database) || !base::nzchar(base::trimws(feature_database[1]))) {
    stop("feature_database is required for metabolomics feature search (one of: refmet_id, refmet, hmdb, smiles, inchikey).")
  }
  feature_database <- base::trimws(base::tolower(feature_database[1]))

  if (feature_database == "refmet") {
    query <- "
      SELECT metabolite_id, refmet_name AS feature_name, is_current, version
      FROM metabolite_reference
      WHERE is_current = 1
    "
    if (!is.null(feature_name) && base::nzchar(base::trimws(feature_name[1]))) {
      query <- base::paste(query, "AND refmet_name =", DBI::dbQuoteLiteral(conn, base::trimws(feature_name[1])))
    }
  } else {
    source_db <- base::switch(feature_database,
      refmet_id = "refmet_id",
      hmdb = "hmdb",
      smiles = "smiles",
      inchikey = "inchikey",
      stop(base::sprintf(
        "Unsupported feature_database '%s'. Use one of: refmet_id, refmet, hmdb, smiles, inchikey.",
        feature_database
      ))
    )

    query <- base::sprintf("
      SELECT x.metabolite_id, x.source_value AS feature_name, m.is_current, m.version
      FROM metabolite_xref x
      INNER JOIN metabolite_reference m ON x.metabolite_id = m.metabolite_id
      WHERE x.source_db = %s AND m.is_current = 1
    ", DBI::dbQuoteLiteral(conn, source_db))

    if (!is.null(feature_name) && base::nzchar(base::trimws(feature_name[1]))) {
      query <- base::paste(query, "AND x.source_value =", DBI::dbQuoteLiteral(conn, base::trimws(feature_name[1])))
    }
  }

  query <- base::paste(query, "ORDER BY feature_name ASC LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

# Loads a cached geneset .rds via geneset_resources.storage_path (populated
# by scripts/build_msigdb_cache.R + scripts/register_msigdb_cache.R) for a
# given geneset_resource_id. This is the fast path: no network fetch, and it
# reuses the same catalog search_geneset_resources already exposes.
load_cached_genesets <- function(conn, geneset_resource_id) {
  resource_id <- base::suppressWarnings(base::as.integer(geneset_resource_id[1]))
  if (base::is.na(resource_id)) {
    stop("geneset_resource_id must be a valid integer.")
  }

  storage_path <- DBI::dbGetQuery(conn, base::paste(
    "SELECT storage_path FROM geneset_resources WHERE geneset_resource_id =",
    DBI::dbQuoteLiteral(conn, resource_id)
  ))$storage_path

  if (base::length(storage_path) == 0) {
    stop(base::sprintf("No geneset_resources row found for geneset_resource_id = %s.", resource_id))
  }
  if (!base::file.exists(storage_path)) {
    stop(base::sprintf(
      "geneset_resource_id = %s points at '%s', which doesn't exist on this server.",
      resource_id, storage_path
    ))
  }

  base::readRDS(storage_path)
}

# Flattens a hypeR hyp/multihyp R6 result (montilab/hypeR's R/hyp.R and
# R/multihyp.R -- hyp$data is a plain data.frame, multihyp$data is a named
# list of hyp objects) into a plain, JSON-friendly structure: a single
# fdr-ranked list of rows for hyp, or a named list of ranked row-lists for
# multihyp (one per query vector produced by split_by_group/split_by_direction).
flatten_hyp_result <- function(hyp_result, limit) {
  rank_and_cap <- function(df) {
    if (base::is.null(df) || base::nrow(df) == 0) {
      return(base::list())
    }
    if ("fdr" %in% base::colnames(df)) {
      df <- df[base::order(df$fdr), , drop = FALSE]
    }
    compact_table(df, max_rows = limit)
  }

  if (methods::is(hyp_result, "multihyp")) {
    return(base::lapply(hyp_result$as.list(), rank_and_cap))
  }

  rank_and_cap(hyp_result$as.data.frame())
}

# Runs SigRepo::runHypeR() against one signature, resolving genesets from
# exactly one of two places:
#   - geneset_resource_id: reads the pre-cached local .rds via
#     load_cached_genesets() above -- fast, no network call.
#   - msigdb_collection (+ msigdb_species/msigdb_subcollection): resolved via
#     ensure_msigdb_geneset_resource() (api/lib/msigdb_genesets_admin.R) --
#     registers the collection into geneset_resources/geneset_entries on the
#     first call if it isn't already there, then reads the same cache file
#     load_cached_genesets() would. Every run_enrichment call ends up
#     cache-backed this way and grows the shared catalog as agents actually
#     use it, instead of a separate, unregistered hypeR::msigdb_gsets() fetch
#     that left no trace for search_geneset_resources to ever find.
# Unlike the other MCP query functions, this one calls into SigRepo:: directly
# rather than reimplementing query logic -- there's real enrichment statistics
# here (hypeR's hypergeometric/KS/GSEA tests), not a lookup to reshape.
run_enrichment <- function(signature_id, geneset_resource_id = NULL,
                            msigdb_species = NULL, msigdb_collection = NULL, msigdb_subcollection = NULL,
                            method = "hypergeo", background = NULL, fdr = 0.05,
                            split_by_group = FALSE, split_by_direction = FALSE, limit = 20) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 20
  }
  limit <- base::min(limit, 100)

  using_cache <- !base::is.null(geneset_resource_id) &&
    base::nzchar(base::trimws(base::as.character(geneset_resource_id[1])))
  using_msigdb <- !base::is.null(msigdb_collection) && base::nzchar(base::trimws(msigdb_collection[1]))

  if (using_cache && using_msigdb) {
    stop("Provide either geneset_resource_id or msigdb_collection, not both.")
  }
  if (!using_cache && !using_msigdb) {
    stop("Provide either geneset_resource_id (see search_geneset_resources) or msigdb_collection to select which gene sets to test against.")
  }

  # Reuses the module-level `conn_handler` (mcp/run_sigrepo_mcp.R) rather
  # than building a second one here: ensure_msigdb_geneset_resource() calls
  # SigRepo::addGenesetResource()/addGenesetEntry() internally, which
  # require an 'admin'-role connection regardless of which SigRepo user is
  # actually driving this MCP call, and runHypeR() below needs the same
  # connection to round-trip through the REST API for the difexp table --
  # both need the api_host/api_port override that connection already
  # carries, not the package's production-pointing default a fresh
  # newConnHandler() call here would silently fall back to.
  if (using_cache) {
    conn <- db_connect_local()
    genesets <- base::tryCatch(
      load_cached_genesets(conn, geneset_resource_id),
      finally = base::suppressWarnings(DBI::dbDisconnect(conn))
    )
  } else {
    resolved_species <- if (!base::is.null(msigdb_species) && base::nzchar(base::trimws(msigdb_species[1]))) {
      base::trimws(msigdb_species[1])
    } else {
      "Homo sapiens"
    }
    resolved_subcollection <- if (!base::is.null(msigdb_subcollection) && base::nzchar(base::trimws(msigdb_subcollection[1]))) {
      base::trimws(msigdb_subcollection[1])
    } else {
      ""
    }

    ensured <- ensure_msigdb_geneset_resource(
      conn_handler = conn_handler,
      cache_dir = msigdb_cache_dir,
      species = resolved_species,
      collection = base::trimws(msigdb_collection[1]),
      subcollection = resolved_subcollection
    )
    genesets <- base::readRDS(ensured$resource$storage_path[[1]])
  }

  hype_result <- SigRepo::runHypeR(
    conn_handler = conn_handler,
    signature_id = signature_id,
    genesets = genesets,
    method = method,
    background = background,
    fdr = fdr,
    split_by_group = split_by_group,
    split_by_direction = split_by_direction,
    plotting = FALSE,
    quiet = TRUE,
    verbose = FALSE
  )

  base::list(
    results = flatten_hyp_result(hype_result$result, limit),
    query_vector_sizes = stats::setNames(
      base::vapply(hype_result$signatures, base::length, integer(1)),
      base::names(hype_result$signatures)
    )
  )
}

compare_two_signatures <- function(signature_hashkey_1, signature_hashkey_2, auth = NULL, max_features = 200) {
  if (identical(signature_hashkey_1, signature_hashkey_2)) {
    stop("signature_hashkey_1 and signature_hashkey_2 must be different signatures.")
  }

  contexts <- fetch_signature_contexts(
    signature_hashkeys = c(signature_hashkey_1, signature_hashkey_2),
    include_features = TRUE,
    max_features = max_features,
    auth = auth
  )

  if (base::length(contexts) < 2) {
    missing <- base::setdiff(c(signature_hashkey_1, signature_hashkey_2), base::names(contexts))
    stop(base::sprintf("Could not find signature(s): %s", base::paste(missing, collapse = ", ")))
  }

  similarity_tbl <- signature_similarity_summary(contexts)

  base::list(
    signature_hashkey_1 = signature_hashkey_1,
    signature_hashkey_2 = signature_hashkey_2,
    signature_name_1 = contexts[[signature_hashkey_1]]$signature$signature_name,
    signature_name_2 = contexts[[signature_hashkey_2]]$signature$signature_name,
    features_1 = similarity_tbl$features_1[1],
    features_2 = similarity_tbl$features_2[1],
    shared_features = similarity_tbl$shared_features[1],
    jaccard_similarity = similarity_tbl$jaccard_similarity[1]
  )
}
