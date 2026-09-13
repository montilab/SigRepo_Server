# Helpers for the Shiny Compare tab (modules/compare_module.R).
#
# The tab is a front end to SigRepo::compareSignatures(), which is itself a
# front end to OmicSignature::compare_omic_signatures(). Nothing here compares
# anything: these helpers turn the tab's inputs into that function's arguments,
# and read its result back for display. Keeping them free of Shiny lets them be
# tested on their own (tests/testthat/test-shiny-compare-helpers.R).

# ---- parsing free text ------------------------------------------------------

# Signature names typed into a text box, one per line or comma separated.
compare_parse_names <- function(text) {
  if (base::is.null(text) || base::length(text) == 0) {
    return(base::character())
  }
  x <- base::trimws(base::unlist(base::strsplit(base::paste(text, collapse = "\n"), "[,\n]+")))
  base::unique(x[!base::is.na(x) & x != ""])
}

# Background features typed into a text box. NULL when blank, so
# compare_omic_signatures() builds its own universe from the signatures.
compare_parse_background <- function(text) {
  if (base::is.null(text) || base::length(text) == 0) {
    return(NULL)
  }
  x <- base::trimws(base::unlist(base::strsplit(base::paste(text, collapse = "\n"), "[,[:space:]]+")))
  x <- base::unique(x[!base::is.na(x) & x != ""])
  if (base::length(x) == 0) NULL else x
}

# ---- uploads ----------------------------------------------------------------

# Read one uploaded .rds into a named list of OmicSignature objects. Accepts
# what compareSignatures()'s omic_signatures argument accepts: an OmicSignature,
# a list of them, or an OmicSignatureCollection. List names are kept; blank ones
# fall back to the metadata signature_name, the same rule compareSignatures()
# applies, so the names previewed in the tab are the names in the result.
compare_read_signature_upload <- function(path, file_name) {
  obj <- base::tryCatch(
    base::readRDS(path),
    error = function(e) {
      base::stop(base::sprintf("'%s' could not be read as an .rds file: %s", file_name, base::conditionMessage(e)), call. = FALSE)
    }
  )

  if (methods::is(obj, "OmicSignatureCollection")) {
    obj <- obj$OmicSigList
  }
  if (methods::is(obj, "OmicSignature")) {
    obj <- base::list(obj)
  }

  not_signature <- function() {
    base::stop(base::sprintf(
      "'%s' must contain an OmicSignature, a list of OmicSignature objects, or an OmicSignatureCollection.",
      file_name
    ), call. = FALSE)
  }
  if (!base::is.list(obj) || base::length(obj) == 0) {
    not_signature()
  }
  if (!base::all(base::vapply(obj, methods::is, base::logical(1), "OmicSignature"))) {
    not_signature()
  }

  list_names <- base::names(obj)
  if (base::is.null(list_names)) {
    list_names <- base::rep("", base::length(obj))
  }
  blank <- base::is.na(list_names) | list_names == ""
  list_names[blank] <- base::vapply(obj[blank], function(s) {
    nm <- s$metadata$signature_name
    if (base::length(nm) > 0 && !base::is.na(nm[1]) && nm[1] != "") base::as.character(nm[1]) else "signature"
  }, base::character(1))
  base::names(obj) <- list_names
  obj
}

# Every file from one fileInput(multiple = TRUE), combined in upload order.
compare_read_signature_uploads <- function(files) {
  if (base::is.null(files) || base::NROW(files) == 0) {
    return(base::list())
  }
  base::do.call(c, base::lapply(base::seq_len(base::nrow(files)), function(i) {
    compare_read_signature_upload(files$datapath[i], files$name[i])
  }))
}

# ---- the names compareSignatures() will give the signatures ----------------

# Preview one list the way compareSignatures() assembles it, from the
# searchSignature() rows the user can see: rows picked by id (in pick order),
# then rows matching a requested name case-insensitively (every match), a
# signature requested both ways kept once, fetched names that collide told
# apart as "name (id N)", then uploaded signatures under their list names.
#
# The label-pairing inputs are keyed by these names, and compareSignatures()
# validates label_pairing against the same names, so they have to agree.
# Requests matching nothing are returned in attr(, "missing").
compare_preview_list <- function(signature_db, signature_ids = NULL, signature_names = NULL, omic_signatures = NULL) {
  rows <- signature_db[0, , drop = FALSE]
  missing <- base::character()
  db_ids <- base::as.character(signature_db$signature_id)

  ids <- base::as.character(signature_ids)
  if (base::length(ids) > 0) {
    # sprintf, not paste: paste("id", character(0)) is "id ", not empty.
    missing <- c(missing, base::sprintf("id %s", ids[!ids %in% db_ids]))
    rows <- signature_db[base::match(ids[ids %in% db_ids], db_ids), , drop = FALSE]
  }

  if (base::length(signature_names) > 0) {
    found <- base::tolower(base::trimws(base::as.character(signature_db$signature_name)))
    wanted <- base::tolower(signature_names)
    missing <- c(missing, base::sprintf("'%s'", signature_names[!wanted %in% found]))
    ordered <- base::unlist(base::lapply(wanted, function(w) base::which(found == w)), use.names = FALSE)
    rows <- base::rbind(rows, signature_db[ordered, , drop = FALSE])
  }
  rows <- rows[!base::duplicated(base::as.character(rows$signature_id)), , drop = FALSE]

  fetched_names <- base::as.character(rows$signature_name)
  dup <- fetched_names %in% fetched_names[base::duplicated(fetched_names)]
  fetched_names[dup] <- base::sprintf("%s (id %s)", fetched_names[dup], base::as.character(rows$signature_id[dup]))

  uploads <- if (base::is.null(omic_signatures)) base::list() else omic_signatures
  upload_direction <- base::vapply(uploads, function(s) {
    d <- s$metadata$direction_type
    if (base::length(d) > 0 && !base::is.na(d[1])) base::as.character(d[1]) else NA_character_
  }, base::character(1))

  # What the user can pair on: the stored phenotype for fetched signatures
  # (their group labels are only known once fetched), the actual group labels
  # for uploads.
  db_hint <- if ("phenotype" %in% base::names(rows)) base::as.character(rows$phenotype) else base::rep(NA_character_, base::nrow(rows))
  upload_hint <- base::vapply(uploads, function(s) {
    labels <- s$signature$group_label
    if (base::is.null(labels)) NA_character_ else base::paste(base::sort(base::unique(base::as.character(labels))), collapse = " / ")
  }, base::character(1))

  out <- base::data.frame(
    name = c(fetched_names, base::names(uploads)),
    source = c(base::rep("database", base::nrow(rows)), base::rep("upload", base::length(uploads))),
    direction_type = c(base::as.character(rows$direction_type), base::unname(upload_direction)),
    labels_hint = c(db_hint, base::unname(upload_hint)),
    stringsAsFactors = FALSE
  )
  base::attr(out, "missing") <- missing
  out
}

# ---- label pairing ----------------------------------------------------------

# One level1/level2 row per signature into compareSignatures()'s label_pairing
# list. Blank rows fall back to the signature's own group_label factor order;
# a half-filled row is a mistake worth stopping for.
compare_label_pairing <- function(signature_names, level1, level2) {
  level1 <- base::trimws(base::as.character(level1))
  level2 <- base::trimws(base::as.character(level2))
  filled1 <- !base::is.na(level1) & level1 != ""
  filled2 <- !base::is.na(level2) & level2 != ""

  half <- base::xor(filled1, filled2)
  if (base::any(half)) {
    base::stop(base::sprintf(
      "Label pairing needs both levels for %s, or neither to use the signature's own label order.",
      base::paste(base::sprintf("'%s'", signature_names[half]), collapse = ", ")
    ), call. = FALSE)
  }

  both <- filled1 & filled2
  if (!base::any(both)) {
    return(NULL)
  }
  stats::setNames(
    base::lapply(base::which(both), function(i) c(level1[i], level2[i])),
    signature_names[both]
  )
}

# ---- arguments --------------------------------------------------------------

# The argument list for SigRepo::compareSignatures(). `list1`/`list2` each hold
# signature_ids, signature_names, omic_signatures and label_pairing; `list2` is
# NULL for a self-comparison, so none of the *2 arguments are sent. Empty
# sources are left out rather than sent empty, and so is the connection when
# nothing comes from the database.
compare_build_args <- function(conn_handler, list1, list2, settings) {
  from_database <- function(l) !base::is.null(l) && base::length(c(l$signature_ids, l$signature_names)) > 0

  args <- base::list()
  if (from_database(list1) || from_database(list2)) {
    args$conn_handler <- conn_handler
  }

  add_list <- function(args, l, suffix) {
    if (base::length(l$signature_ids) > 0) args[[base::paste0("signature_ids", suffix)]] <- l$signature_ids
    if (base::length(l$signature_names) > 0) args[[base::paste0("signature_names", suffix)]] <- l$signature_names
    if (base::length(l$omic_signatures) > 0) args[[base::paste0("omic_signatures", suffix)]] <- l$omic_signatures
    if (!base::is.null(l$label_pairing)) args[[base::paste0("label_pairing", suffix)]] <- l$label_pairing
    args
  }
  args <- add_list(args, list1, "")
  if (!base::is.null(list2)) {
    args <- add_list(args, list2, "2")
  }

  setting_names <- c(
    "method", "background", "score_cutoff", "adj_p_cutoff", "min_features", "max_feature",
    "feature_col", "score_col", "adj_p_col", "p_value_col", "group_col",
    "adjust", "p_adjust_method", "alternative", "gsea_score", "minSize", "maxSize"
  )
  for (nm in setting_names) {
    if (!base::is.null(settings[[nm]])) args[[nm]] <- settings[[nm]]
  }
  args
}

# ---- running ----------------------------------------------------------------

# Call compareSignatures() and keep everything it says. Its warnings carry real
# information -- signatures left out as missing or not visible, mismatched
# label orders, dropped small sets -- so they are collected for the tab to show
# rather than lost to the server log. Errors are returned, not raised.
compare_run <- function(args, runner = SigRepo::compareSignatures) {
  warnings <- base::character()
  result <- base::tryCatch(
    base::withCallingHandlers(
      base::do.call(runner, args),
      warning = function(w) {
        warnings <<- c(warnings, base::trimws(base::conditionMessage(w)))
        base::invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      base::structure(base::trimws(base::conditionMessage(e)), class = "compare_run_error")
    }
  )

  if (base::inherits(result, "compare_run_error")) {
    return(base::list(result = NULL, warnings = base::unique(warnings), error = base::unclass(result)))
  }
  base::list(result = result, warnings = base::unique(warnings), error = NULL)
}

# ---- reading the result -----------------------------------------------------

# The comparisons as {name -> list of matrices}. An all-uni-directional overlap
# holds its matrices directly in `comparisons`; wrap that as one "overlap"
# comparison so everything downstream sees one shape.
compare_comparisons <- function(result) {
  comparisons <- result$comparisons
  if (base::any(c("jaccard", "score") %in% base::names(comparisons))) {
    comparisons <- base::list(overlap = comparisons)
  }
  comparisons
}

compare_is_rank_based <- function(result) {
  !base::identical(result$method, "overlap")
}

compare_is_flat <- function(result) {
  base::any(c("jaccard", "score") %in% base::names(result$comparisons))
}

# Measures OmicSignature::signature_similarity_heatmap() accepts for this result.
compare_heatmap_measures <- function(result) {
  if (compare_is_rank_based(result)) c("score", "pvalue") else c("jaccard", "pvalue")
}

# Matrices worth tabulating: the heatmap measures plus overlap sizes.
compare_table_matrices <- function(result) {
  if (compare_is_rank_based(result)) c("score", "pvalue") else c("jaccard", "pvalue", "counts")
}

# A self-comparison is the only case with a redundant half to hide, and the
# only one split mode can draw.
compare_is_symmetric <- function(result) {
  if (compare_is_rank_based(result)) {
    return(FALSE)
  }
  m <- compare_comparisons(result)[[1]]$jaccard
  base::identical(base::rownames(m), base::colnames(m))
}

# Modes signature_similarity_heatmap() will draw for this result: combined and
# split need two level comparisons; split also refuses rank-based results and
# anything that is not a square self-comparison.
compare_heatmap_modes <- function(result) {
  modes <- "separate"
  if (base::length(compare_comparisons(result)) >= 2) {
    modes <- c(modes, "combined")
    if (compare_is_symmetric(result)) {
      modes <- c(modes, "split")
    }
  }
  modes
}

# One matrix of one comparison, or NULL when the result has no such matrix.
compare_result_matrix <- function(result, comparison, matrix_name) {
  compare_comparisons(result)[[comparison]][[matrix_name]]
}

# Extra ComplexHeatmap::Heatmap() arguments (passed through
# signature_similarity_heatmap()'s `...`) that give row and column names room
# for the longest signature name. ComplexHeatmap otherwise cuts names at 6 cm,
# which hides the distinguishing tail of most repository signature names.
compare_heatmap_name_args <- function(result) {
  m <- compare_comparisons(result)[[1]][[compare_table_matrices(result)[1]]]
  labels <- base::unique(c(base::rownames(m), base::colnames(m)))
  # Measured at Heatmap()'s default row/column name size, gpar(fontsize = 12).
  width <- ComplexHeatmap::max_text_width(labels, gp = grid::gpar(fontsize = 12)) + grid::unit(2, "mm")
  base::list(row_names_max_width = width, column_names_max_height = width)
}

# Every signature pair with every measure, one row per pair per comparison.
# A self-comparison lists each unordered pair once for overlap (the matrices
# are symmetric) but both orders for rank-based methods, where geneset and
# ranking roles differ. The labels are the group labels compared at that level.
compare_pairs_table <- function(result) {
  comparisons <- compare_comparisons(result)
  measures <- compare_table_matrices(result)
  rank_based <- compare_is_rank_based(result)
  label_order <- result$label_order
  order1 <- label_order$sig_list1
  order2 <- if (!base::is.null(label_order$sig_list2)) label_order$sig_list2 else order1

  label_at <- function(order, sigs, level) {
    if (base::is.null(order) || base::is.na(level) || level > base::ncol(order)) {
      return(base::rep(NA_character_, base::length(sigs)))
    }
    base::unname(order[sigs, level])
  }

  tables <- base::lapply(base::names(comparisons), function(comp_name) {
    base_matrix <- compare_result_matrix(result, comp_name, measures[1])
    row_sigs <- base::rownames(base_matrix)
    col_sigs <- base::colnames(base_matrix)
    grid <- base::expand.grid(i = base::seq_along(row_sigs), j = base::seq_along(col_sigs))
    self <- base::identical(row_sigs, col_sigs)
    if (self) {
      grid <- if (rank_based) grid[grid$i != grid$j, , drop = FALSE] else grid[grid$i < grid$j, , drop = FALSE]
    }
    grid <- grid[base::order(grid$i, grid$j), , drop = FALSE]

    level <- base::suppressWarnings(base::as.integer(base::sub("^level(\\d+)_vs_level\\d+$", "\\1", comp_name)))
    out <- base::data.frame(
      comparison = base::rep(comp_name, base::nrow(grid)),
      signature = row_sigs[grid$i],
      signature_label = label_at(order1, row_sigs[grid$i], level),
      reference = col_sigs[grid$j],
      reference_label = label_at(order2, col_sigs[grid$j], level),
      stringsAsFactors = FALSE
    )
    for (measure in measures) {
      m <- compare_result_matrix(result, comp_name, measure)
      out[[measure]] <- if (base::is.null(m)) NA else m[base::cbind(row_sigs[grid$i], col_sigs[grid$j])]
    }
    out
  })

  out <- base::do.call(base::rbind, tables)
  base::rownames(out) <- NULL
  out
}

# label_order as one table: which group label each signature contributes at
# each level. NULL for an all-uni-directional overlap, which has no levels.
compare_label_order_table <- function(result) {
  lo <- result$label_order
  if (base::is.null(lo)) {
    return(NULL)
  }
  parts <- base::lapply(base::seq_along(lo), function(k) {
    m <- lo[[k]]
    out <- base::data.frame(
      list = base::rep(base::paste("List", k), base::nrow(m)),
      signature = base::rownames(m),
      stringsAsFactors = FALSE
    )
    for (col in base::colnames(m)) {
      out[[col]] <- base::unname(m[, col])
    }
    out
  })
  out <- base::do.call(base::rbind, parts)
  base::rownames(out) <- NULL
  out
}

# ---- the equivalent R call --------------------------------------------------

# The compareSignatures() call that reproduces a run, for pasting into R. Only
# arguments that differ from the function's defaults are written. The
# connection handler and uploaded objects cannot be printed (one holds a
# password, the others are whole signatures), so they are referred to by name
# with a comment saying where they come from.
compare_r_call <- function(args) {
  defaults <- base::formals(SigRepo::compareSignatures)
  by_reference <- c("conn_handler", "omic_signatures", "omic_signatures2")

  differs <- function(nm) {
    if (nm %in% by_reference || !nm %in% base::names(defaults)) {
      return(TRUE)
    }
    default <- base::tryCatch(base::eval(defaults[[nm]]), error = function(e) NULL)
    if (nm == "method") {
      default <- default[1]
    }
    !base::identical(args[[nm]], default) &&
      !(base::is.numeric(args[[nm]]) && base::is.numeric(default) && base::isTRUE(base::all.equal(args[[nm]], default)))
  }
  written <- base::Filter(differs, base::names(args))

  header <- base::character()
  if ("conn_handler" %in% written) {
    header <- c(header, "conn_handler <- SigRepo::newConnHandler(...)  # your SigRepo login")
  }
  for (nm in base::intersect(c("omic_signatures", "omic_signatures2"), written)) {
    header <- c(header, base::sprintf("%s <- readRDS(\"<the file uploaded to the app>.rds\")", nm))
  }

  lines <- base::vapply(written, function(nm) {
    value <- if (nm %in% by_reference) nm else base::paste(base::deparse(args[[nm]], width.cutoff = 500L), collapse = " ")
    base::sprintf("  %s = %s", nm, value)
  }, base::character(1))

  call <- base::paste0("res <- SigRepo::compareSignatures(\n", base::paste(lines, collapse = ",\n"), "\n)")
  base::paste(c(header, if (base::length(header) > 0) "", call), collapse = "\n")
}
