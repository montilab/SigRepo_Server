# Gene set enrichment analysis backing /annotate/*. Calls hypeR::hypeR()
# directly instead of SigRepo::runHypeR() -- runHypeR() resolves its
# signature via SigRepo::getSignature(), which authorizes through
# SigRepo::checkPermissions() (the *DB connection's own login*, not the
# api_key holder). Since the REST API always connects through one shared
# service-account login (see conn_handler in api.R), that would either leak
# private signatures to callers who shouldn't see them or wrongly hide
# signatures from callers who should -- the same class of bug fixed for
# signature/collection writes, but here on a read path with real disclosure
# risk. Instead: fetch the signature ourselves via fetch_signature_context()
# (which already authorizes against the real api_key caller), build the
# query vector by hand, and call hypeR::hypeR() directly -- SigRepo/
# checkPermissions never enter the picture.
#
# Depends on api/lib/common.R (db_connect_local), api/lib/signature.R
# (fetch_signature_context), and api/lib/difexp.R (load_difexp_rds).

`%||%` <- function(x, y) if (base::is.null(x) || base::is.na(x)) y else x

# The real list of MSigDB collections/subcollections hypeR can fetch.
list_msigdb_collections <- function() {
  as.data.frame(hypeR::msigdb_available())
}

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

# Builds the query hypeR::hypeR() expects for a signature:
#   - "hypergeometric": an unnamed character vector of gene symbols, from
#     the signature's stored (already-curated) feature set.
#   - "kstest" (rank-based/GSEA-style): a named numeric vector, gene symbol
#     -> score, from the signature's *difexp* table -- the full unfiltered
#     ranked results, not the curated signature subset, which is what a
#     rank statistic actually needs. Requires has_difexp = 1. difexp rows
#     are matched back to gene symbols via their shared probe_id with the
#     signature's own feature set (SigRepo enforces this correspondence at
#     upload time -- see createOmicSignature.R's probe_id cross-check).
#
# Returns list(ok = FALSE, reason, message?) or
# list(ok = TRUE, query = <vector>, signature_name = ...).
resolve_enrichment_query <- function(auth, signature_hashkey, test, difexp_dir) {
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = TRUE,
    max_features = 5000,
    auth = auth
  )
  if (base::is.null(context)) {
    return(base::list(ok = FALSE, reason = "not_found"))
  }

  signature_name <- context$signature$signature_name
  assay_type <- context$signature$assay_type
  ref_table <- enrichment_reference_table(assay_type)
  if (base::is.null(ref_table)) {
    return(base::list(
      ok = FALSE, reason = "unsupported_assay_type",
      message = base::sprintf("Enrichment is not supported for assay_type = '%s'.", assay_type)
    ))
  }
  if (base::length(context$features) == 0) {
    return(base::list(ok = FALSE, reason = "no_features"))
  }

  probe_ids <- base::vapply(context$features, function(f) base::as.character(f$probe_id %||% NA), character(1))
  feature_ids <- base::vapply(context$features, function(f) base::suppressWarnings(base::as.integer(f$feature_id %||% NA)), integer(1))

  conn <- NULL
  symbol_by_feature_id <- base::tryCatch({
    conn <- db_connect_local()
    lookup_gene_symbols(conn, ref_table, feature_ids)
  }, finally = {
    if (!base::is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })

  # probe_id -> gene_symbol, for joining difexp rows (which carry probe_id,
  # not feature_id) back to a symbol.
  symbol_by_probe_id <- stats::setNames(
    symbol_by_feature_id[base::as.character(feature_ids)],
    probe_ids
  )

  if (identical(test, "hypergeometric")) {
    query_vector <- base::unique(symbol_by_probe_id[!base::is.na(symbol_by_probe_id) & base::nzchar(symbol_by_probe_id)])
    query_vector <- base::unname(query_vector)
    if (base::length(query_vector) == 0) {
      return(base::list(ok = FALSE, reason = "no_gene_symbols"))
    }
    return(base::list(ok = TRUE, query = query_vector, signature_name = signature_name))
  }

  # kstest
  if (!base::isTRUE(base::as.logical(context$signature$has_difexp))) {
    return(base::list(ok = FALSE, reason = "no_difexp"))
  }

  difexp_tbl <- load_difexp_rds(difexp_dir, signature_hashkey)
  if (base::is.null(difexp_tbl) || !base::is.data.frame(difexp_tbl) || base::nrow(difexp_tbl) == 0) {
    return(base::list(ok = FALSE, reason = "no_difexp"))
  }

  id_col <- if ("probe_id" %in% base::colnames(difexp_tbl)) "probe_id" else NULL
  score_col <- if ("score" %in% base::colnames(difexp_tbl)) "score" else NULL
  if (base::is.null(id_col) || base::is.null(score_col)) {
    return(base::list(
      ok = FALSE, reason = "unsupported_difexp_shape",
      message = base::sprintf(
        "difexp for this signature does not have the expected 'probe_id'/'score' columns (has: %s).",
        base::paste(base::colnames(difexp_tbl), collapse = ", ")
      )
    ))
  }

  difexp_probe_ids <- base::as.character(difexp_tbl[[id_col]])
  difexp_scores <- base::suppressWarnings(base::as.numeric(difexp_tbl[[score_col]]))
  difexp_symbols <- base::unname(symbol_by_probe_id[difexp_probe_ids])

  keep <- !base::is.na(difexp_symbols) & base::nzchar(difexp_symbols) & !base::is.na(difexp_scores)
  ranked <- stats::aggregate(difexp_scores[keep], by = base::list(symbol = difexp_symbols[keep]), FUN = function(x) x[base::which.max(base::abs(x))])
  if (base::nrow(ranked) == 0) {
    return(base::list(ok = FALSE, reason = "no_gene_symbols"))
  }
  ranked <- ranked[base::order(-ranked$x), ]
  query_vector <- stats::setNames(ranked$x, ranked$symbol)

  base::list(ok = TRUE, query = query_vector, signature_name = signature_name)
}

# Runs hypeR::hypeR() and shapes the result table for the API response.
# Returns list(ok = FALSE, reason, message) or
# list(ok = TRUE, signature_name, n_query = <int>, results = <data.frame>).
run_enrichment <- function(auth, signature_hashkey, test = c("hypergeometric", "kstest"),
                            species = "Homo sapiens", collection = "H", subcollection = NULL,
                            fdr = 0.05, difexp_dir) {
  test <- base::match.arg(test)

  resolved <- resolve_enrichment_query(auth, signature_hashkey, test, difexp_dir)
  if (!resolved$ok) {
    return(resolved)
  }

  genesets <- base::tryCatch(
    hypeR::msigdb_gsets(species = species, collection = collection, subcollection = subcollection, clean = FALSE),
    error = function(err) {
      structure(base::list(message = err$message), class = "enrichment_geneset_error")
    }
  )
  if (base::inherits(genesets, "enrichment_geneset_error")) {
    return(base::list(ok = FALSE, reason = "invalid_geneset", message = genesets$message))
  }

  hyp <- base::tryCatch(
    hypeR::hypeR(signature = resolved$query, genesets = genesets, test = test, fdr = fdr, plotting = FALSE, quiet = TRUE),
    error = function(err) {
      structure(base::list(message = err$message), class = "enrichment_run_error")
    }
  )
  if (base::inherits(hyp, "enrichment_run_error")) {
    return(base::list(ok = FALSE, reason = "enrichment_failed", message = hyp$message))
  }

  results_tbl <- hyp$data[base::order(hyp$data$pval), , drop = FALSE]

  base::list(
    ok = TRUE,
    signature_name = resolved$signature_name,
    n_query = base::length(resolved$query),
    results = results_tbl
  )
}
