# Helpers for the Shiny Compare tab (modules/compare_module.R).
#
# The tab is a front end to SigRepo::compareSignatures(), which is itself a
# front end to OmicSignature::compare_omic_signatures(). Nothing here compares
# anything: these helpers turn the tab's inputs into that function's arguments,
# and read its result back for display. Keeping them free of Shiny lets them be
# tested on their own (tests/testthat/test-shiny-compare-helpers.R).

# ---- defaults ---------------------------------------------------------------

# compareSignatures()'s defaults for every setting the tab exposes. The inputs
# start at these values and their labels print them; a test holds them to the
# function's formals so the two cannot drift apart.
COMPARE_DEFAULTS <- base::list(
  method = "overlap",
  background = NULL,
  score_cutoff = 0,
  adj_p_cutoff = 0.05,
  min_features = 5,
  max_feature = 500,
  feature_col = "feature_name",
  score_col = "score",
  adj_p_col = "adj_p",
  p_value_col = "p_value",
  group_col = "group_label",
  adjust = FALSE,
  p_adjust_method = "BH",
  alternative = "greater",
  gsea_score = "NES",
  minSize = 1,
  maxSize = Inf
)

# A default as the tab's labels print it: "default (0.05)".
compare_default_text <- function(name) {
  value <- COMPARE_DEFAULTS[[name]]
  shown <- if (base::is.null(value)) {
    "none"
  } else if (base::is.logical(value)) {
    if (base::isTRUE(value)) "on" else "off"
  } else {
    base::format(value)
  }
  base::sprintf("default (%s)", shown)
}

# ---- signature picker filters -----------------------------------------------

# The picker's facet dropdowns: signature table column -> dropdown label.
COMPARE_FACETS <- c(
  assay_type = "Assay",
  type = "Direction",
  organism = "Organism",
  has_difexp = "Difexp table"
)

# Choices for one facet dropdown, "all" first. has_difexp is a 0/1 flag.
compare_facet_choices <- function(signature_db, facet) {
  if (facet == "has_difexp") {
    return(c("All" = "all", "Yes" = "yes", "No" = "no"))
  }
  values <- if (facet %in% base::names(signature_db)) base::as.character(signature_db[[facet]]) else base::character()
  values <- base::sort(base::unique(values[!base::is.na(values) & values != ""]))
  c("All" = "all", stats::setNames(values, values))
}

# The rows of the signature table that pass every facet. `facets` maps a facet
# to its dropdown value; "all", NULL, or a facet the table lacks filter nothing.
compare_facet_filter <- function(signature_db, facets) {
  keep <- base::rep(TRUE, base::nrow(signature_db))
  for (facet in base::names(facets)) {
    value <- facets[[facet]]
    if (base::is.null(value) || base::identical(value, "all") || !facet %in% base::names(signature_db)) {
      next
    }
    column <- signature_db[[facet]]
    if (facet == "has_difexp") {
      flag <- base::as.integer(column) == 1L
      keep <- keep & !base::is.na(flag) & flag == base::identical(value, "yes")
    } else {
      keep <- keep & !base::is.na(column) & base::as.character(column) == value
    }
  }
  signature_db[keep, , drop = FALSE]
}

# The signatures picked for one list once the table shows `view_ids` and has
# `selected_ids` selected. Picks outside the view survive a filter change;
# picks inside it follow the selection. Earlier picks keep their order and new
# ones go last, since pick order is the order compareSignatures() gets.
compare_update_picks <- function(picked_ids, view_ids, selected_ids) {
  picked_ids <- base::as.character(picked_ids)
  view_ids <- base::as.character(view_ids)
  selected_ids <- base::as.character(selected_ids)
  kept <- picked_ids[!picked_ids %in% view_ids | picked_ids %in% selected_ids]
  c(kept, base::setdiff(selected_ids, kept))
}

# ---- parsing free text ------------------------------------------------------

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
# fetched names that collide told apart as "name (id N)", then uploaded
# signatures under their list names.
#
# The label-pairing inputs are keyed by these names, and compareSignatures()
# validates label_pairing against the same names, so they have to agree.
compare_preview_list <- function(signature_db, signature_ids = NULL, omic_signatures = NULL) {
  db_ids <- base::as.character(signature_db$signature_id)
  ids <- base::unique(base::as.character(signature_ids))
  rows <- signature_db[base::match(ids[ids %in% db_ids], db_ids), , drop = FALSE]

  fetched_names <- base::as.character(rows$signature_name)
  dup <- fetched_names %in% fetched_names[base::duplicated(fetched_names)]
  fetched_names[dup] <- base::sprintf("%s (id %s)", fetched_names[dup], base::as.character(rows$signature_id[dup]))

  uploads <- if (base::is.null(omic_signatures)) base::list() else omic_signatures
  upload_direction <- base::vapply(uploads, function(s) {
    d <- s$metadata$type
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

  base::data.frame(
    name = c(fetched_names, base::names(uploads)),
    source = c(base::rep("database", base::nrow(rows)), base::rep("upload", base::length(uploads))),
    signature_id = c(base::as.character(rows$signature_id), base::rep(NA_character_, base::length(uploads))),
    type = c(base::as.character(rows$type), base::unname(upload_direction)),
    labels_hint = c(db_hint, base::unname(upload_hint)),
    stringsAsFactors = FALSE
  )
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
# signature_ids, omic_signatures and label_pairing; `list2` is NULL for a
# self-comparison, so none of the *2 arguments are sent. Empty sources are
# left out rather than sent empty, and so is the connection when nothing comes
# from the database.
compare_build_args <- function(conn_handler, list1, list2, settings) {
  from_database <- function(l) !base::is.null(l) && base::length(l$signature_ids) > 0

  args <- base::list()
  if (from_database(list1) || from_database(list2)) {
    args$conn_handler <- conn_handler
  }

  add_list <- function(args, l, suffix) {
    if (base::length(l$signature_ids) > 0) args[[base::paste0("signature_ids", suffix)]] <- l$signature_ids
    if (base::length(l$omic_signatures) > 0) args[[base::paste0("omic_signatures", suffix)]] <- l$omic_signatures
    if (!base::is.null(l$label_pairing)) args[[base::paste0("label_pairing", suffix)]] <- l$label_pairing
    args
  }
  args <- add_list(args, list1, "")
  if (!base::is.null(list2)) {
    args <- add_list(args, list2, "2")
  }

  for (nm in base::names(COMPARE_DEFAULTS)) {
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

# Whether List 1 was compared against itself. label_order says so directly;
# an all-uni-directional overlap has none, so fall back to the matrix names.
compare_is_self <- function(result) {
  if (!base::is.null(result$label_order)) {
    return(base::is.null(result$label_order$sig_list2))
  }
  m <- compare_comparisons(result)[[1]][[compare_table_matrices(result)[1]]]
  base::identical(base::rownames(m), base::colnames(m))
}

# ---- the matrix view --------------------------------------------------------

# Repository signature names run to 80 characters and a third of them share
# their first 20, so a matrix headed by full or shortened names is unreadable.
# The Matrices tab heads rows S1..Sn (List 1) and columns R1..Rm (List 2, or S
# again for a self-comparison) and lists the full names in a key.
compare_matrix_labels <- function(result) {
  m <- compare_comparisons(result)[[1]][[compare_table_matrices(result)[1]]]
  rows <- stats::setNames(base::sprintf("S%d", base::seq_len(base::nrow(m))), base::rownames(m))
  cols <- if (compare_is_self(result)) rows else stats::setNames(base::sprintf("R%d", base::seq_len(base::ncol(m))), base::colnames(m))
  base::list(rows = rows, cols = cols)
}

# The key to those labels. `previews` are the lists as compare_preview_list()
# showed them before the run, which is where a signature's id comes from.
compare_matrix_key <- function(result, previews = base::list()) {
  labels <- compare_matrix_labels(result)
  part <- function(labels, list_name, preview) {
    signatures <- base::names(labels)
    id <- if (base::is.null(preview)) NA_character_ else preview$signature_id[base::match(signatures, preview$name)]
    base::data.frame(label = base::unname(labels), list = list_name, signature = signatures,
                     signature_id = id, stringsAsFactors = FALSE)
  }
  preview <- function(k) if (base::length(previews) >= k) previews[[k]]
  out <- part(labels$rows, "List 1", preview(1))
  if (!compare_is_self(result)) {
    out <- base::rbind(out, part(labels$cols, "List 2", preview(2)))
  }
  out
}

# Cell shading for one displayed matrix, as the cuts and colours
# DT::styleInterval() takes: pale to mid blue with the value for jaccard and
# counts, with significance for p-values, and blue-white-red around zero for
# rank scores. NULL when there is nothing to shade. A self-comparison's
# diagonal (each signature against itself) is left out of the range, or its
# jaccard of 1 and full set sizes would wash every other cell out.
compare_matrix_shading <- function(m, matrix_name, self = FALSE) {
  if (self && base::nrow(m) > 1) {
    m[base::cbind(base::seq_len(base::nrow(m)), base::seq_len(base::nrow(m)))] <- NA
  }
  values <- base::as.numeric(m)
  values <- values[base::is.finite(values)]
  if (base::length(values) == 0) {
    return(NULL)
  }
  blues <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#7fb5dc", "#6baed6")
  if (matrix_name == "pvalue") {
    return(base::list(cuts = c(1e-8, 1e-4, 1e-3, 1e-2, 0.05), colors = base::rev(blues)))
  }
  if (matrix_name == "score") {
    limit <- base::max(base::abs(values))
    if (limit == 0) {
      return(NULL)
    }
    return(base::list(cuts = limit * c(-0.6, -0.2, 0.2, 0.6),
                      colors = c("#67a9cf", "#d1e5f0", "#f7f7f7", "#fddbc7", "#ef8a62")))
  }
  top <- base::max(values)
  if (top <= 0) {
    return(NULL)
  }
  base::list(cuts = top * (1:5) / 6, colors = blues)
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
