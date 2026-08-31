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
