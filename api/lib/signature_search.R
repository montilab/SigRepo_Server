# LINCS connectivity search, via Bioconductor signatureSearch.
#
# Answers a question nothing else in SigRepo can: given this signature, which
# perturbations produce the same expression pattern, or reverse it? A reversing
# hit is a drug-repurposing hypothesis for the phenotype the signature encodes.
#
# Deliberately narrow for a first pass -- LINCS only, human only. The wider
# package offers five search methods and several reference databases; the ones
# left out are not oversights:
#   * CMAP/gCMAP/Fisher/correlation need either another reference database or
#     the difexp profile rather than the curated set. Adding them later does not
#     change anything here.
#   * LINCS is built from human cell lines and signatureSearch is human-centric
#     (org.Hs.eg.db). Mouse signatures -- 100 of 293 on the current repository --
#     would need ortholog mapping, which is lossy in ways that matter for a
#     connectivity score. Better to refuse clearly than to return a number
#     nobody should trust.
#
# Depends on api/lib/signature.R (fetch_signature_context) and
# api/lib/annotate.R (lookup_gene_symbols, enrichment_reference_table), both
# resolved at call time.

# Path to the LINCS HDF5 reference database. Unset means the feature is simply
# not available on this deployment, which every entry point checks for -- the
# file is several GB and is not shipped with the image.
lincs_refdb_path <- function() {
  base::Sys.getenv("LINCS_REFDB", unset = "")
}

lincs_available <- function() {
  path <- lincs_refdb_path()
  base::nzchar(path) &&
    base::file.exists(path) &&
    base::requireNamespace("signatureSearch", quietly = TRUE)
}

# Why a search cannot run here, as a sentence for the caller, or NULL if it can.
lincs_unavailable_reason <- function() {
  if (!base::requireNamespace("signatureSearch", quietly = TRUE)) {
    return("The signatureSearch package is not installed on this server.")
  }
  path <- lincs_refdb_path()
  if (!base::nzchar(path)) {
    return("No LINCS reference database is configured (set LINCS_REFDB).")
  }
  if (!base::file.exists(path)) {
    return(base::sprintf("The configured LINCS reference database is missing (%s).", path))
  }
  NULL
}

# Minimum genes per direction. LINCS' weighted KS statistic is meaningless on a
# handful of genes; the published guidance is ~100-150 per side, and below ~10
# the score is noise dressed as a result.
LINCS_MIN_GENES_PER_SIDE <- 10L

# Split a signature's curated feature set into up/down gene symbols.
#
# Direction is the sign of `score`, which is the convention the rest of the
# application already uses -- the detail page renders `score >= 0` as Up. It is
# NOT group_label: that column holds the biological contrast ("Older",
# "Younger", "Increasing_with_age") and carries no direction.
#
# Returns list(ok = TRUE, upset, downset, signature_name, ...) or
# list(ok = FALSE, reason, message).
lincs_query_from_signature <- function(auth, signature_hashkey, max_features = 100000) {
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = TRUE,
    max_features = max_features,
    auth = auth
  )

  if (base::is.null(context)) {
    return(base::list(ok = FALSE, reason = "not_found",
                      message = "Signature not found, or you do not have access to it."))
  }

  signature_name <- base::as.character(context$signature$signature_name %||% signature_hashkey)
  organism <- base::trimws(base::as.character(context$signature$organism %||% ""))
  assay_type <- base::trimws(base::tolower(base::as.character(context$signature$assay_type %||% "")))

  if (!base::identical(organism, "Homo sapiens")) {
    return(base::list(
      ok = FALSE, reason = "unsupported_organism", signature_name = signature_name,
      message = base::sprintf(
        "LINCS is built from human cell lines, so this search only supports Homo sapiens signatures (this one is %s).",
        if (base::nzchar(organism)) organism else "of unknown organism"
      )
    ))
  }

  # LINCS profiles are transcriptional. A proteomics or metabolomics signature
  # has no meaningful connectivity score against them -- that is out of scope
  # rather than broken, and saying so beats an empty result that reads as
  # failure.
  if (!base::identical(assay_type, "transcriptomics")) {
    return(base::list(
      ok = FALSE, reason = "unsupported_assay_type", signature_name = signature_name,
      message = base::sprintf(
        "LINCS reference profiles are transcriptional, so a '%s' signature cannot be scored against them.",
        assay_type
      )
    ))
  }

  features <- context$features %||% base::list()
  if (base::length(features) == 0) {
    return(base::list(ok = FALSE, reason = "no_features", signature_name = signature_name,
                      message = "This signature has no features recorded to search with."))
  }

  scores <- base::vapply(features, function(f) base::suppressWarnings(base::as.numeric(f$score %||% NA)), numeric(1))
  feature_ids <- base::vapply(features, function(f) base::suppressWarnings(base::as.integer(f$feature_id %||% NA)), integer(1))

  ref_table <- enrichment_reference_table(assay_type)
  conn <- db_connect_local()
  symbols_by_id <- base::tryCatch(
    lookup_gene_symbols(conn, ref_table, feature_ids),
    finally = base::suppressWarnings(DBI::dbDisconnect(conn))
  )

  symbols <- base::unname(symbols_by_id[base::as.character(feature_ids)])
  keep <- !base::is.na(symbols) & base::nzchar(symbols) & !base::is.na(scores)
  symbols <- base::toupper(symbols[keep])
  scores <- scores[keep]

  if (base::length(symbols) == 0) {
    return(base::list(
      ok = FALSE, reason = "no_gene_symbols", signature_name = signature_name,
      message = paste0(
        "No gene symbols could be resolved for this signature's features, so there is ",
        "nothing to match against LINCS, which is keyed on gene symbols."
      )
    ))
  }

  upset <- base::unique(symbols[scores > 0])
  downset <- base::unique(symbols[scores < 0])

  # A gene landing in both directions is contradictory -- drop it rather than
  # letting it push the score both ways.
  both <- base::intersect(upset, downset)
  if (base::length(both) > 0) {
    upset <- base::setdiff(upset, both)
    downset <- base::setdiff(downset, both)
  }

  if (base::length(upset) < LINCS_MIN_GENES_PER_SIDE || base::length(downset) < LINCS_MIN_GENES_PER_SIDE) {
    return(base::list(
      ok = FALSE, reason = "too_few_genes", signature_name = signature_name,
      message = base::sprintf(
        paste0("LINCS scores an up and a down set together, and this signature resolves to %d up / %d down ",
               "gene symbols -- below the %d per side needed for the statistic to mean anything."),
        base::length(upset), base::length(downset), LINCS_MIN_GENES_PER_SIDE
      )
    ))
  }

  base::list(
    ok = TRUE,
    signature_name = signature_name,
    upset = upset,
    downset = downset,
    n_up = base::length(upset),
    n_down = base::length(downset),
    n_ambiguous = base::length(both)
  )
}

# Run the search. Returns list(ok = TRUE, hits, ...) or list(ok = FALSE, ...).
#
# A LINCS search reads a multi-GB HDF5 file and is not a sub-second operation,
# which is why every caller treats it as an explicit action rather than
# something that runs on page load.
lincs_search <- function(auth, signature_hashkey, limit = 25) {
  blocked <- lincs_unavailable_reason()
  if (!base::is.null(blocked)) {
    return(base::list(ok = FALSE, reason = "unavailable", message = blocked))
  }

  query <- lincs_query_from_signature(auth, signature_hashkey)
  if (!base::isTRUE(query$ok)) {
    return(query)
  }

  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) limit <- 25
  limit <- base::min(limit, 200)

  result <- base::tryCatch({
    qsig <- signatureSearch::qSig(
      query = base::list(upset = query$upset, downset = query$downset),
      gess_method = "LINCS",
      refdb = lincs_refdb_path()
    )
    gess <- signatureSearch::gess_lincs(qsig, sortby = "NCS", tau = FALSE)
    signatureSearch::result(gess)
  }, error = function(e) e)

  if (base::inherits(result, "error")) {
    return(base::list(ok = FALSE, reason = "search_failed",
                      signature_name = query$signature_name,
                      message = base::sprintf("LINCS search failed: %s", base::conditionMessage(result))))
  }

  if (!base::is.data.frame(result) || base::nrow(result) == 0) {
    return(base::list(ok = TRUE, signature_name = query$signature_name,
                      n_up = query$n_up, n_down = query$n_down,
                      total = 0, hits = base::list()))
  }

  # Keep the columns that mean something to a reader and are stable across
  # signatureSearch versions; drop the rest rather than passing through a wide
  # frame the UI would have to guess at.
  wanted <- base::intersect(
    c("pert", "cell", "type", "trend", "WTCS", "WTCS_Pval", "WTCS_FDR", "NCS", "NCSct", "N_upset", "N_downset"),
    base::colnames(result)
  )
  tidy <- result[, wanted, drop = FALSE]
  tidy <- utils::head(tidy, limit)

  base::list(
    ok = TRUE,
    signature_name = query$signature_name,
    n_up = query$n_up,
    n_down = query$n_down,
    n_ambiguous = query$n_ambiguous,
    total = base::nrow(result),
    hits = compact_table(tidy, max_rows = limit)
  )
}
