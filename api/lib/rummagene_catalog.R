# Rummagene catalog: build-time gate, storage, and query.
#
# The build job (api/lib/rummagene_catalog_build.R) streams Rummagene's
# latest.gmt, qualifies each set through api/lib/rummagene_ingest.R, and stores
# the survivors here. See specs/2026-08-31-rummagene-catalog-design.md.
#
# Depends on api/lib/rummagene_ingest.R and the `conn_handler` global in api.R.

if (!base::exists("%||%")) {
  `%||%` <- function(a, b) if (base::is.null(a)) b else a
}

# One GMT line -> list(term, description, genes, pmcid), or NULL when the line
# cannot yield a candidate.
#
# GMT is `term \t description \t gene \t gene \t ...`. Rummagene's terms start
# with the source article's PMC id, which is the only route to MeSH -- a term
# without one can never qualify, so it is dropped here rather than carried
# through the whole pipeline.
rummagene_parse_gmt_line <- function(line) {
  line <- base::as.character(line)[1]
  if (base::is.na(line) || !base::nzchar(base::trimws(line))) {
    return(NULL)
  }

  fields <- base::strsplit(line, "\t", fixed = TRUE)[[1]]
  if (base::length(fields) < 3) {
    return(NULL)
  }

  term <- base::trimws(fields[1])
  pmcid <- rummagene_pmcid_from_term(term)
  if (base::length(pmcid) == 0 || base::is.na(pmcid)) {
    return(NULL)
  }

  genes <- base::trimws(fields[-c(1, 2)])
  genes <- base::unique(genes[base::nzchar(genes)])
  if (base::length(genes) == 0) {
    return(NULL)
  }

  base::list(
    term = term,
    description = base::trimws(fields[2]),
    genes = genes,
    pmcid = pmcid
  )
}

# Scope is human transcriptomics (see the spec). Mouse needs org.Mm.eg.db, and
# separately needs updateTranscriptomicsFeatureSet()'s hgnc_symbol -> mgi_symbol
# bug fixed, so it is refused here rather than half-supported.
RUMMAGENE_CATALOG_ORGANISM <- "Homo sapiens"

# symbol -> lowercased Ensembl gene id, NA where the symbol has none.
#
# Lowercased because updateTranscriptomicsFeatureSet() stores
# feature_name = trimws(tolower(ensembl_gene_id)); matching any other case
# would resolve nothing.
rummagene_map_symbols <- function(symbols, organism) {
  if (!base::identical(base::as.character(organism)[1], RUMMAGENE_CATALOG_ORGANISM)) {
    base::stop(
      "The Rummagene catalog covers only Homo sapiens (asked for '",
      base::as.character(organism)[1], "')."
    )
  }
  symbols <- base::unique(base::as.character(symbols))

  mapped <- base::suppressMessages(AnnotationDbi::mapIds(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = symbols, keytype = "SYMBOL", column = "ENSEMBL", multiVals = "first"
  ))
  out <- base::tolower(base::as.character(mapped))
  out[base::is.na(mapped)] <- NA_character_
  stats::setNames(out, symbols)
}

# Which of `feature_names` actually exist in transcriptomics_features for this
# organism. Resolved by feature_hashkey, the same way
# create_signature.R's resolve_feature_ids() does, so the gate and the eventual
# insert agree by construction.
rummagene_resolve_features <- function(conn, feature_names, organism_id) {
  feature_names <- base::unique(base::as.character(feature_names))
  if (base::length(feature_names) == 0) {
    return(base::character(0))
  }
  hashkeys <- base::vapply(
    feature_names, function(fn) collection_hash(fn, organism_id), base::character(1)
  )
  found <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT feature_hashkey FROM transcriptomics_features WHERE feature_hashkey IN (%s)",
    base::paste(DBI::dbQuoteLiteral(conn, base::unname(hashkeys)), collapse = ",")
  ))
  base::unname(feature_names[hashkeys %in% found$feature_hashkey])
}

# The all-or-nothing gate. Returns list(ok = TRUE, feature_names) only when
# EVERY symbol maps to an Ensembl id AND every one of those ids is present in
# this database's reference table.
#
# Two distinct rejections, kept distinct because they mean different things: a
# dead alias is a property of the source set, while a missing feature is a
# property of THIS database and may change when the reference table is next
# rebuilt from biomaRt.
rummagene_gate <- function(conn, parsed, organism, organism_id) {
  mapped <- rummagene_map_symbols(parsed$genes, organism)
  if (base::any(base::is.na(mapped))) {
    return(base::list(ok = FALSE, reason = "unmapped_symbol"))
  }

  present <- rummagene_resolve_features(conn, base::unname(mapped), organism_id)
  if (base::length(present) != base::length(base::unique(base::unname(mapped)))) {
    return(base::list(ok = FALSE, reason = "feature_absent"))
  }

  # unique(): multiVals = "first" above means two DIFFERENT symbols can
  # legitimately land on the SAME Ensembl id, so the raw mapped vector can
  # carry a duplicate. Collapsing it here is not a violation of "nothing is
  # invented": the verbatim published symbol list is preserved separately as
  # gene_symbols, exact and untouched -- this dedup only ever applies to the
  # DERIVED Ensembl list. Two symbols collapsing onto one gene is a fact
  # about the mapping, not a gene being dropped, so it is expected (not a
  # bug to "fix" later) for length(feature_names) to come out less than
  # length(gene_symbols) when that happens. base::unique() preserves
  # first-occurrence order.
  base::list(ok = TRUE, feature_names = base::unique(base::unname(mapped)))
}

# Gene lists are stored as comma-delimited text. Gene symbols and Ensembl ids
# never contain a comma, so this needs no escaping, and it keeps the column
# greppable from SQL for debugging.
.rummagene_join_genes <- function(x) base::paste(base::as.character(x), collapse = ",")

# Write catalog rows. Keyed on term_hashkey = md5(tolower(term)) -- the same
# formula collection_hash() uses everywhere else in this codebase -- so a
# re-run over an unchanged GMT updates in place rather than duplicating.
rummagene_catalog_upsert <- function(conn, rows, gmt_version) {
  written <- 0L
  for (r in rows %||% base::list()) {
    hk <- collection_hash(r$term, "")
    DBI::dbExecute(conn, base::sprintf(
      "INSERT INTO rummagene_catalog
         (term, pmcid, pmid, title, year, doi, description, organism, assay_type,
          mesh_evidence, n_genes, gene_symbols, feature_names, gmt_version, term_hashkey)
       VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %d, %s, %s, %s, %s)
       ON DUPLICATE KEY UPDATE
         term = VALUES(term), pmcid = VALUES(pmcid), pmid = VALUES(pmid), title = VALUES(title),
         year = VALUES(year), doi = VALUES(doi), description = VALUES(description),
         organism = VALUES(organism), assay_type = VALUES(assay_type),
         mesh_evidence = VALUES(mesh_evidence), n_genes = VALUES(n_genes),
         gene_symbols = VALUES(gene_symbols), feature_names = VALUES(feature_names),
         gmt_version = VALUES(gmt_version), built_at = CURRENT_TIMESTAMP",
      DBI::dbQuoteLiteral(conn, r$term),
      DBI::dbQuoteLiteral(conn, r$pmcid),
      sql_value(conn, r$pmid),
      sql_value(conn, r$title),
      sql_value(conn, r$year),
      sql_value(conn, r$doi),
      sql_value(conn, r$description),
      DBI::dbQuoteLiteral(conn, r$organism),
      DBI::dbQuoteLiteral(conn, r$assay_type),
      DBI::dbQuoteLiteral(conn, r$mesh_evidence),
      base::length(r$gene_symbols),
      DBI::dbQuoteLiteral(conn, .rummagene_join_genes(r$gene_symbols)),
      DBI::dbQuoteLiteral(conn, .rummagene_join_genes(r$feature_names)),
      DBI::dbQuoteLiteral(conn, gmt_version),
      DBI::dbQuoteLiteral(conn, hk)
    ))
    written <- written + 1L
  }
  written
}

# Drop rows the current build did not touch -- i.e. sets Rummagene has withdrawn
# or that no longer pass the gate. A signature someone already pulled is
# unaffected: it lives in `signatures` and there is no FK back to here.
#
# Guarded against the build's own failure modes, not just a hypothetical: a
# truncated GMT download, or an NCBI outage that makes every set fail the
# MeSH check, produces a build that qualifies NOTHING under the new
# gmt_version. `WHERE gmt_version <> gmt_version` is subtractive -- if no row
# anywhere carries the new version yet, EVERY existing row matches, and an
# unconditional prune would read that as "Rummagene withdrew its entire
# catalog" and empty the table. A build that qualified zero rows is a failed
# build, not evidence of mass withdrawal, so: if the count of rows already
# carrying `gmt_version` is zero, refuse to delete, warn, and return 0. Call
# this only AFTER rummagene_catalog_upsert() has written the new build's rows
# under `gmt_version` -- in that order the count is non-zero and this
# proceeds normally.
rummagene_catalog_prune <- function(conn, gmt_version) {
  present <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) n FROM rummagene_catalog WHERE gmt_version = %s",
    DBI::dbQuoteLiteral(conn, gmt_version)
  ))$n[1]
  if (base::as.numeric(present) == 0) {
    base::warning(
      "rummagene_catalog_prune: refusing to delete -- no rows carry gmt_version '",
      gmt_version, "'. This looks like a failed build (nothing qualified under ",
      "the new version), not a genuine withdrawal of the whole catalog. Upsert ",
      "the new build's rows before pruning.",
      call. = FALSE
    )
    return(0L)
  }
  DBI::dbExecute(conn, base::sprintf(
    "DELETE FROM rummagene_catalog WHERE gmt_version <> %s",
    DBI::dbQuoteLiteral(conn, gmt_version)
  ))
}

# sort_by is interpolated into ORDER BY, where dbQuoteLiteral cannot protect it.
# Whitelist, exactly as .signature_sort_columns does in api/lib/signature.R.
.rummagene_catalog_sort_columns <- base::list(
  term       = "term",
  title      = "title",
  year       = "year",
  n_genes    = "n_genes",
  organism   = "organism",
  assay_type = "assay_type"
)

# One page of the catalog plus the TOTAL count of matching rows, so the client
# can render pager controls while holding only one page.
#
# gene_symbols and feature_names are deliberately NOT selected: at ~135k rows of
# ~40 genes each they dwarf everything else, and only a detail view needs them.
search_rummagene_catalog <- function(conn, q = NULL, organism = NULL, assay_type = NULL,
                                     year_min = NULL, year_max = NULL,
                                     n_genes_min = NULL, n_genes_max = NULL,
                                     limit = 25, offset = 0,
                                     sort_by = NULL, sort_dir = "asc") {
  # As in search_signatures() (api/lib/signature.R:41-49): a non-numeric
  # limit/offset must degrade to a default instead of crashing sprintf's %d
  # into the literal text "LIMIT NA", and the upper cap keeps a caller from
  # requesting the whole ~135k-row table in one response and defeating the
  # pagination this function exists to provide. Reusing that function's cap
  # of 100 rather than inventing a different one for the same shape of limit.
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 25
  }
  limit <- base::min(limit, 100)

  offset <- base::suppressWarnings(base::as.integer(offset[1]))
  if (base::is.na(offset) || offset < 0) {
    offset <- 0
  }

  where <- base::character(0)
  add <- function(clause) where <<- c(where, clause)

  q <- base::trimws(base::as.character(q %||% ""))
  if (base::nzchar(q)) {
    like <- DBI::dbQuoteLiteral(conn, base::paste0("%", q, "%"))
    add(base::sprintf("(term LIKE %s OR title LIKE %s OR description LIKE %s)", like, like, like))
  }
  if (base::length(organism) == 1 && base::nzchar(base::as.character(organism))) {
    add(base::sprintf("organism = %s", DBI::dbQuoteLiteral(conn, organism)))
  }
  if (base::length(assay_type) == 1 && base::nzchar(base::as.character(assay_type))) {
    add(base::sprintf("assay_type = %s", DBI::dbQuoteLiteral(conn, assay_type)))
  }
  # NULL, a single NA (any type), or "" all mean "not supplied" -- the route
  # layer's json_scalar() yields "" for a parameter the caller never passed --
  # and must stay silently absent. Anything else that fails to parse as a
  # number is a caller error and must fail loudly: silently dropping it would
  # be indistinguishable, from the caller's side, from "filtered correctly
  # and nothing matched", which is exactly the failure shape the "nothing is
  # invented" rule exists to prevent.
  for (bound in base::list(
    base::list(v = year_min,    name = "year_min",    col = "year",    op = ">="),
    base::list(v = year_max,    name = "year_max",    col = "year",    op = "<="),
    base::list(v = n_genes_min, name = "n_genes_min", col = "n_genes", op = ">="),
    base::list(v = n_genes_max, name = "n_genes_max", col = "n_genes", op = "<=")
  )) {
    v <- bound$v
    not_supplied <- base::is.null(v) ||
      (base::length(v) == 1 && base::is.na(v)) ||
      (base::length(v) == 1 && base::is.character(v) && !base::nzchar(base::trimws(v)))
    if (not_supplied) {
      next
    }
    n <- base::suppressWarnings(base::as.integer(v))
    if (base::is.na(n)) {
      base::stop(base::sprintf(
        "search_rummagene_catalog: %s must be a number, got %s",
        bound$name, base::deparse(v)
      ))
    }
    add(base::sprintf("%s %s %d", bound$col, bound$op, n))
  }

  where_sql <- if (base::length(where) == 0) "" else base::paste("WHERE", base::paste(where, collapse = " AND "))

  count <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) AS n FROM rummagene_catalog %s", where_sql
  ))$n[1]

  sort_key <- base::trimws(base::as.character(sort_by %||% ""))
  sort_expr <- if (base::nzchar(sort_key)) .rummagene_catalog_sort_columns[[sort_key]] else NULL
  if (base::is.null(sort_expr)) {
    sort_expr <- "year"
  }
  sort_dir_sql <- if (base::identical(base::tolower(base::trimws(base::as.character(sort_dir %||% "asc"))), "desc")) "DESC" else "ASC"

  rows <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT rummagene_catalog_id, term, pmcid, pmid, title, year, doi, description,
            organism, assay_type, mesh_evidence, n_genes, gmt_version, built_at, term_hashkey
     FROM rummagene_catalog %s
     ORDER BY %s %s, term ASC
     LIMIT %d OFFSET %d",
    where_sql, sort_expr, sort_dir_sql,
    limit, offset
  ))

  base::list(count = base::as.integer(count), rows = rows)
}

# One catalog row WITH its genes, for a detail view or a pull.
get_rummagene_catalog_entry <- function(conn, term) {
  hk <- collection_hash(term, "")
  row <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT * FROM rummagene_catalog WHERE term_hashkey = %s LIMIT 1",
    DBI::dbQuoteLiteral(conn, hk)
  ))
  if (base::nrow(row) == 0) {
    return(NULL)
  }
  base::list(
    term = row$term[1], pmcid = row$pmcid[1], pmid = row$pmid[1],
    title = row$title[1], year = row$year[1], doi = row$doi[1],
    description = row$description[1], organism = row$organism[1],
    assay_type = row$assay_type[1], mesh_evidence = row$mesh_evidence[1],
    gene_symbols  = base::strsplit(row$gene_symbols[1],  ",", fixed = TRUE)[[1]],
    feature_names = base::strsplit(row$feature_names[1], ",", fixed = TRUE)[[1]]
  )
}
