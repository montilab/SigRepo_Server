# Gene-symbol resolution shared by the API (api/lib/annotate.R, rummagene.R)
# and the Shiny Annotate module (legacy_app/modules/annotate_module.R).
#
# Deliberately dependency-free beyond DBI so the Shiny app can source it
# without dragging in the rest of api/lib. api.R picks it up with every other
# api/lib/*.R file; legacy_app/app_src/bootstrap.R sources it by name.

# assay_type -> the reference table holding feature_id -> gene_symbol.
# Enrichment against MSigDB needs gene symbols; only these two assay types
# have a features table with a gene_symbol column today.
enrichment_reference_table <- function(assay_type) {
  switch(assay_type,
    "transcriptomics" = "transcriptomics_features",
    "proteomics" = "proteomics_features",
    NULL
  )
}

# Column names a difexp may carry gene symbols under. Depositors are not
# consistent about this: of 297 difexp tables on the production repository, 118
# use `symbol` and only 39 use `gene_symbol`. Checking a single name silently
# ignores symbols that are sitting right there, which is what made enrichment
# fail for signatures whose reference-table symbols are absent.
#
# Kept identical to the list rummagene.R accepts, so the two features cannot
# disagree about whether a signature has usable symbols.
DIFEXP_SYMBOL_COLUMNS <- c("gene_symbol", "symbol", "geneSymbol", "gene", "hgnc_symbol", "mgi_symbol")

# The first symbol-bearing column actually present and populated, or NULL.
difexp_symbol_column <- function(difexp_tbl) {
  for (col in DIFEXP_SYMBOL_COLUMNS) {
    if (col %in% base::colnames(difexp_tbl)) {
      candidate <- base::trimws(base::as.character(difexp_tbl[[col]]))
      if (base::any(!base::is.na(candidate) & base::nzchar(candidate))) {
        return(col)
      }
    }
  }
  NULL
}

# feature_id -> gene_symbol for the given ids, from the appropriate
# reference table. Returns a named character vector (names = feature_id).
lookup_gene_symbols <- function(conn, ref_table, feature_ids) {
  feature_ids <- base::unique(feature_ids[!base::is.na(feature_ids)])
  if (base::length(feature_ids) == 0) {
    return(base::character())
  }

  query <- base::sprintf(
    "SELECT feature_id, gene_symbol FROM %s WHERE feature_id IN (%s)",
    ref_table, base::paste(feature_ids, collapse = ",")
  )
  tbl <- DBI::dbGetQuery(conn, query)
  symbols <- base::trimws(base::as.character(tbl$gene_symbol))
  stats::setNames(symbols, base::as.character(tbl$feature_id))
}

# feature_name -> gene_symbol for the given names within one organism (the
# reference tables' natural key is (feature_name, organism_id) -- see their
# UNIQUE constraint), from the appropriate reference table. Returns a named
# character vector (names = feature_name). This is what lets a kstest run
# use every gene difexp actually measured, not just the ones that happen to
# also be in the signature's own curated feature set (see
# resolve_single_enrichment_query()).
lookup_gene_symbols_by_feature_name <- function(conn, ref_table, feature_names, organism_id) {
  feature_names <- base::unique(feature_names[!base::is.na(feature_names) & base::nzchar(feature_names)])
  if (base::length(feature_names) == 0 || base::is.na(organism_id)) {
    return(base::character())
  }

  query <- base::sprintf(
    "SELECT feature_name, gene_symbol FROM %s WHERE organism_id = %d AND feature_name IN (%s)",
    ref_table, base::as.integer(organism_id), base::paste(DBI::dbQuoteLiteral(conn, feature_names), collapse = ",")
  )
  tbl <- DBI::dbGetQuery(conn, query)
  symbols <- base::trimws(base::as.character(tbl$gene_symbol))
  stats::setNames(symbols, base::as.character(tbl$feature_name))
}
