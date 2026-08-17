# Multi-signature comparison backing POST /signatures/compare.
#
# Reuses SigRepo::createOmicSignature() to build correct OmicSignature objects
# server-side, then runs OmicSignature::compare_omic_signatures(). Two things
# make this safe to do from inside the single-process API:
#   1. Each signature is authorized against the *real* api_key caller via
#      fetch_signature_context(auth=...) before it is built -- createOmicSignature
#      itself authorizes against the DB connection's own login (see the note in
#      api/lib/export.R), which would otherwise let a caller compare signatures
#      they can't see.
#   2. difexp is loaded from disk (load_difexp_rds) and injected with
#      fetch_difexp = FALSE, so createOmicSignature never makes an HTTP call back
#      to this same server (which would deadlock a single-process Plumber).
#
# Depends on api/lib/signature.R (fetch_signature_context), api/lib/difexp.R
# (load_difexp_rds), and the `conn_handler` global defined in api.R.

# Overlap needs no difexp; the rank-based methods (KS and GSEA) use difexp
# when a signature has it. GSEA additionally requires the fgsea package.
COMPARE_METHODS <- c("overlap", "ks_rank", "ks_score", "gsea")

# Build one OmicSignature for a hashkey, or NULL if the caller can't see it
# (or it doesn't exist).
compare_build_signature <- function(auth, signature_hashkey, difexp_dir) {
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = FALSE,
    auth = auth
  )
  if (base::is.null(context)) {
    return(NULL)
  }

  db_row <- base::as.data.frame(context$signature, stringsAsFactors = FALSE)

  difexp <- NULL
  if (base::isTRUE(base::as.logical(db_row$has_difexp[1]))) {
    difexp <- load_difexp_rds(difexp_dir, signature_hashkey)
  }

  # createOmicSignature() is internal (not exported), so it must be reached
  # with ::: rather than ::.
  SigRepo:::createOmicSignature(
    conn_handler = conn_handler,
    db_signature_tbl = db_row,
    difexp = difexp,
    fetch_difexp = FALSE
  )
}

# One labeled similarity matrix -> a JSON-friendly {rows, cols, values} object.
# Uses as.list() throughout so single-row/col matrices still serialize as JSON
# arrays under the API's auto_unbox = TRUE serializer.
compare_matrix_to_json <- function(m) {
  if (base::is.null(m)) {
    return(NULL)
  }
  m <- base::as.matrix(m)
  values <- base::lapply(base::seq_len(base::nrow(m)), function(i) {
    base::as.list(base::unname(m[i, ]))
  })
  base::list(
    rows = base::as.list(base::rownames(m)),
    cols = base::as.list(base::colnames(m)),
    values = values
  )
}

# compare_omic_signatures() output -> a compact JSON payload the web UI can
# render directly. `sig_meta` is a list of {name, hashkey, direction_type}, in
# matrix row/col order, so the UI can map a clicked heatmap cell back to the
# signatures it represents (needed for the GSEA leading-edge drill-down).
compare_serialize_result <- function(res, skipped, sig_meta, sig_meta2 = NULL) {
  comparisons <- res$comparisons

  # All-uni-directional overlap returns jaccard/pvalue/counts directly instead
  # of the per-level nesting; wrap it as a single "overlap" pairing so the
  # shape the UI sees is always {pairing -> {measure -> matrix}}.
  if (base::any(c("jaccard", "score") %in% base::names(comparisons))) {
    comparisons <- base::list(overlap = comparisons)
  }

  measure_keys <- c("jaccard", "pvalue", "counts", "score")
  out <- base::lapply(comparisons, function(cmp) {
    keys <- base::intersect(measure_keys, base::names(cmp))
    serialized <- stats::setNames(
      base::lapply(keys, function(k) {
        m <- cmp[[k]]
        # The overlap `counts` matrix carries an extra "size" row AND column
        # (each signature's own retained set size), making it one bigger than
        # jaccard/pvalue. Split that out so every measure matrix has identical
        # dimensions -- otherwise the heatmap would render a stray row/col --
        # and expose the sizes separately, where they're actually readable.
        if (base::identical(k, "counts") && !base::is.null(m)) {
          m <- base::as.matrix(m)
          keep_r <- base::rownames(m) != "size"
          keep_c <- base::colnames(m) != "size"
          m <- m[keep_r, keep_c, drop = FALSE]
        }
        compare_matrix_to_json(m)
      }),
      keys
    )

    if ("counts" %in% base::names(cmp) && !base::is.null(cmp$counts)) {
      cm <- base::as.matrix(cmp$counts)
      if ("size" %in% base::colnames(cm)) {
        nm <- base::rownames(cm)[base::rownames(cm) != "size"]
        serialized$sizes <- base::lapply(nm, function(n) {
          base::list(name = n, size = base::unname(cm[n, "size"]))
        })
      }
    }
    serialized
  })

  # label_order is a LIST keyed by input list ("sig_list1", and "sig_list2" for a
  # two-list run); each element is a matrix of rows = signatures, cols =
  # level1/level2, holding the actual group_label each "level" resolved to. We
  # flatten it to {list, signature, levels} records so the UI can name levels
  # honestly (and show what a signature was paired on) instead of parsing
  # "level1_vs_level2" strings.
  label_order <- NULL
  if (!base::is.null(res$label_order)) {
    lo_list <- res$label_order
    if (!base::is.list(lo_list)) {
      lo_list <- base::list(sig_list1 = lo_list)
    }
    records <- base::list()
    for (which_list in base::names(lo_list)) {
      lo <- lo_list[[which_list]]
      if (base::is.null(lo)) {
        next
      }
      lo <- base::as.matrix(lo)
      for (i in base::seq_len(base::nrow(lo))) {
        records[[base::length(records) + 1L]] <- base::list(
          list = which_list,
          signature = base::rownames(lo)[i],
          levels = base::as.list(base::unname(lo[i, ]))
        )
      }
    }
    if (base::length(records) > 0) {
      label_order <- records
    }
  }

  # Which measures are actually present, so the UI can offer only real choices.
  # "sizes" is a per-signature vector, not a matrix -- not a heatmap measure.
  measures <- base::unique(base::unlist(base::lapply(out, base::names)))
  measures <- base::setdiff(measures, "sizes")

  base::list(
    method = res$method,
    # Primary measure the heatmap should default to.
    primary_measure = if (base::identical(res$method, "overlap")) "jaccard" else "score",
    measures = base::as.list(measures),
    pairings = base::as.list(base::names(comparisons)),
    comparisons = out,
    signatures = sig_meta,
    # Non-NULL only for a two-list (query vs reference) comparison. When set,
    # matrices are rectangular: rows = query (sig_list1), cols = reference.
    reference_signatures = sig_meta2,
    two_list = !base::is.null(sig_meta2),
    label_order = label_order,
    skipped = base::as.list(skipped)
  )
}

# Build a named list of OmicSignatures from hashkeys, disambiguating duplicate
# names (compare_omic_signatures() keys its matrices by name). `used_names` is
# carried across both lists so a query and reference signature can't collide.
compare_build_list <- function(auth, hashkeys, difexp_dir, used_names = base::character()) {
  sig_list <- base::list()
  sig_meta <- base::list()
  skipped <- base::character()

  for (hk in hashkeys) {
    os <- base::tryCatch(compare_build_signature(auth, hk, difexp_dir), error = function(e) e)
    if (base::inherits(os, "error") || base::is.null(os)) {
      skipped <- c(skipped, hk)
      next
    }

    nm <- base::tryCatch(os$metadata$signature_name, error = function(e) NULL)
    if (base::is.null(nm) || base::is.na(nm[1]) || nm[1] == "") {
      nm <- hk
    }
    nm <- base::as.character(nm[1])

    base_nm <- nm
    k <- 2L
    while (nm %in% used_names) {
      nm <- base::sprintf("%s (%d)", base_nm, k)
      k <- k + 1L
    }
    used_names <- c(used_names, nm)

    sig_list[[nm]] <- os
    sig_meta[[base::length(sig_meta) + 1L]] <- base::list(
      name = nm,
      hashkey = hk,
      direction_type = base::tryCatch(base::as.character(os$metadata$direction_type)[1], error = function(e) NA_character_)
    )
  }

  base::list(sig_list = sig_list, sig_meta = sig_meta, skipped = skipped, used_names = used_names)
}

# label_pairing arrives from the web UI keyed by hashkey (stable) or by name.
# compare_omic_signatures() wants it keyed by the names used in sig_list, so
# translate, dropping entries that don't resolve or lack >= 2 labels.
compare_translate_pairing <- function(pairing, sig_meta) {
  if (base::is.null(pairing) || base::length(pairing) == 0) {
    return(NULL)
  }
  by_hashkey <- stats::setNames(
    base::vapply(sig_meta, function(m) m$name, character(1)),
    base::vapply(sig_meta, function(m) m$hashkey, character(1))
  )
  known_names <- base::unname(by_hashkey)

  out <- base::list()
  for (key in base::names(pairing)) {
    levels <- base::as.character(base::unlist(pairing[[key]]))
    levels <- levels[!base::is.na(levels) & base::nzchar(levels)]
    if (base::length(levels) < 2) {
      next
    }
    nm <- if (!base::is.na(by_hashkey[key])) by_hashkey[[key]] else if (key %in% known_names) key else NA_character_
    if (base::is.na(nm)) {
      next
    }
    out[[nm]] <- levels
  }
  if (base::length(out) == 0) NULL else out
}

# Full pipeline: hashkeys -> authorized OmicSignature objects -> comparison
# -> serializable result. Throws on bad input / too few loadable signatures.
compare_signatures_result <- function(auth, signature_hashkeys, method, difexp_dir,
                                      reference_hashkeys = NULL,
                                      score_cutoff = 0, adj_p_cutoff = 0.05, min_features = 5,
                                      max_feature = 500, label_pairing = NULL, label_pairing2 = NULL,
                                      adjust = FALSE, p_adjust_method = "BH",
                                      gsea_score = "NES", min_size = 1, max_size = Inf) {
  if (!method %in% COMPARE_METHODS) {
    base::stop(base::sprintf("Unsupported method '%s'. Use one of: %s.",
                             method, base::paste(COMPARE_METHODS, collapse = ", ")))
  }

  clean_keys <- function(x) {
    x <- base::unique(base::as.character(x))
    x[!x %in% c("", NA)]
  }
  signature_hashkeys <- clean_keys(signature_hashkeys)
  reference_hashkeys <- clean_keys(reference_hashkeys)
  two_list <- base::length(reference_hashkeys) > 0

  # Self-comparison needs >= 2 signatures; a two-list comparison only needs one
  # on each side (query vs reference is meaningful with a single pair).
  if (!two_list && base::length(signature_hashkeys) < 2) {
    base::stop("Select at least two signatures to compare.")
  }
  if (two_list && base::length(signature_hashkeys) < 1) {
    base::stop("Select at least one query signature to compare against the reference set.")
  }

  built1 <- compare_build_list(auth, signature_hashkeys, difexp_dir)
  sig_list <- built1$sig_list
  sig_meta <- built1$sig_meta
  skipped <- built1$skipped

  sig_list2 <- NULL
  sig_meta2 <- NULL
  if (two_list) {
    built2 <- compare_build_list(auth, reference_hashkeys, difexp_dir, used_names = built1$used_names)
    sig_list2 <- built2$sig_list
    sig_meta2 <- built2$sig_meta
    skipped <- c(skipped, built2$skipped)
    if (base::length(sig_list2) < 1) {
      base::stop("None of the reference signatures could be loaded (check visibility and that they exist).")
    }
  }

  min_needed <- if (two_list) 1L else 2L
  if (base::length(sig_list) < min_needed) {
    base::stop(if (two_list) {
      "None of the query signatures could be loaded (check visibility and that they exist)."
    } else {
      "Fewer than two of the selected signatures could be loaded (check visibility and that they exist)."
    })
  }

  args <- base::list(
    sig_list1 = sig_list,
    method = method,
    score_cutoff = score_cutoff,
    adj_p_cutoff = adj_p_cutoff,
    min_features = min_features,
    max_feature = max_feature,
    adjust = base::isTRUE(adjust),
    p_adjust_method = p_adjust_method
  )
  if (!base::is.null(sig_list2)) {
    args$sig_list2 <- sig_list2
  }
  pairing1 <- compare_translate_pairing(label_pairing, sig_meta)
  if (!base::is.null(pairing1)) {
    args$label_pairing <- pairing1
  }
  if (!base::is.null(sig_meta2)) {
    pairing2 <- compare_translate_pairing(label_pairing2, sig_meta2)
    if (!base::is.null(pairing2)) {
      args$label_pairing2 <- pairing2
    }
  }
  if (base::identical(method, "gsea")) {
    args$gsea_score <- gsea_score
    args$minSize <- min_size
    args$maxSize <- max_size
  }

  res <- base::do.call(OmicSignature::compare_omic_signatures, args)

  compare_serialize_result(res, skipped, sig_meta, sig_meta2)
}

# Leading-edge / enrichment-plot data for ONE GSEA pair (a geneset signature vs
# a ranking signature), for the heatmap drill-down. Reuses compare_omic_signatures'
# own internal ranking/geneset helpers so the plot is consistent with the matrix,
# then hands off to fgsea::plotEnrichmentData() for the running-ES curve.
compare_leading_edge <- function(auth, geneset_hashkey, ranking_hashkey,
                                 geneset_level = 1, ranking_level = 1, difexp_dir,
                                 score_cutoff = 0, adj_p_cutoff = 0.05,
                                 min_features = 5, max_feature = 500) {
  if (!base::requireNamespace("fgsea", quietly = TRUE)) {
    base::stop("Package 'fgsea' is required for the leading-edge plot.")
  }

  geneset_sig <- compare_build_signature(auth, geneset_hashkey, difexp_dir)
  ranking_sig <- compare_build_signature(auth, ranking_hashkey, difexp_dir)
  if (base::is.null(geneset_sig) || base::is.null(ranking_sig)) {
    base::stop("Could not load both signatures (check visibility / that they exist).")
  }
  if (base::is.null(ranking_sig$difexp)) {
    base::stop("The ranking signature has no difexp table, so it cannot be ranked for GSEA.")
  }

  clamp_level <- function(x) base::max(1L, base::min(2L, base::suppressWarnings(base::as.integer(x[1]))))

  # Ranking must be bi-directional; the geneset side may be uni-directional
  # (whole feature set, no level).
  rank_levels <- OmicSignature:::.cos_group_label_levels(ranking_sig, "group_label")
  ranking_label <- rank_levels[clamp_level(ranking_level)]

  geneset_label <- NULL
  if (!OmicSignature:::.cos_is_uni(geneset_sig)) {
    gset_levels <- OmicSignature:::.cos_group_label_levels(geneset_sig, "group_label")
    geneset_label <- gset_levels[clamp_level(geneset_level)]
  }

  stats <- OmicSignature:::.cos_difexp_scores(
    ranking_sig, ranking_label, "feature_name", "score", "p_value", "group_label"
  )
  geneset <- OmicSignature:::.cos_signature_features(
    geneset_sig, geneset_label, "feature_name", "score", "adj_p", "group_label",
    score_cutoff, adj_p_cutoff, min_features, max_feature
  )
  geneset <- base::intersect(base::unique(geneset), base::names(stats))
  if (base::length(geneset) < 1) {
    base::stop("None of the geneset's features are present in the ranking signature's difexp.")
  }

  pd <- fgsea::plotEnrichmentData(pathway = geneset, stats = stats)
  fg <- fgsea::fgsea(pathways = base::list(sig = geneset), stats = stats,
                     minSize = 1, maxSize = Inf, nproc = 0)

  curve_df <- base::as.data.frame(pd$curve)
  ticks_df <- base::as.data.frame(pd$ticks)

  base::list(
    geneset_name = base::as.character(geneset_sig$metadata$signature_name)[1],
    ranking_name = base::as.character(ranking_sig$metadata$signature_name)[1],
    geneset_label = if (base::is.null(geneset_label)) "all features" else geneset_label,
    ranking_label = ranking_label,
    ranking_contrast = base::setdiff(rank_levels, ranking_label)[1],
    n_ranked = base::length(stats),
    n_geneset = base::length(geneset),
    NES = fg$NES[1],
    pvalue = fg$pval[1],
    ES = if (base::abs(pd$posES) >= base::abs(pd$negES)) pd$posES else pd$negES,
    leading_edge = base::as.list(fg$leadingEdge[[1]]),
    curve = base::lapply(base::seq_len(base::nrow(curve_df)), function(i) {
      base::list(rank = curve_df$rank[i], ES = curve_df$ES[i])
    }),
    ticks = base::as.list(ticks_df$rank)
  )
}
