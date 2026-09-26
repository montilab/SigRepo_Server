# The Shiny Compare tab's helpers, exercised without Shiny or a database. The
# tab is a front end to SigRepo::compareSignatures(), so these check that the
# inputs are translated into that function's arguments faithfully and that its
# result is read back correctly -- using OmicSignature's bundled example
# signatures instead of fetched ones.
source(testthat::test_path("../../legacy_app/utils/compare_utils.R"), local = FALSE)

skip_without_example_data <- function() {
  testthat::skip_if_not_installed("OmicSignature")
}

example_signatures <- function() {
  env <- new.env()
  utils::data("compare_signatures_example", package = "OmicSignature", envir = env)
  env$compare_signatures_example
}

# The rows searchSignature() returns, trimmed to the columns the tab reads.
db_rows <- function() {
  data.frame(
    signature_id = c(1, 2, 3, 4),
    signature_name = c("alpha", "beta", "shared", "shared"),
    type = c("bi-directional", "uni-directional", "bi-directional", "bi-directional"),
    stringsAsFactors = FALSE
  )
}

# ---- defaults ---------------------------------------------------------------

test_that("the tab's defaults are compareSignatures()'s defaults", {
  formals_defaults <- formals(SigRepo::compareSignatures)
  for (nm in names(COMPARE_DEFAULTS)) {
    expected <- eval(formals_defaults[[nm]])
    if (nm == "method") {
      expected <- expected[1]
    }
    expect_identical(COMPARE_DEFAULTS[[nm]], expected, label = nm)
  }
})

test_that("defaults are labelled 'default (value)'", {
  expect_equal(compare_default_text("adj_p_cutoff"), "default (0.05)")
  expect_equal(compare_default_text("score_cutoff"), "default (0)")
  expect_equal(compare_default_text("alternative"), "default (greater)")
  expect_equal(compare_default_text("adjust"), "default (off)")
  expect_equal(compare_default_text("background"), "default (none)")
})

# ---- signature picker filters -----------------------------------------------

facet_rows <- function() {
  data.frame(
    signature_id = c(1, 2, 3, 4, 5),
    signature_name = c("a", "b", "c", "d", "e"),
    assay_type = c("transcriptomics", "transcriptomics", "proteomics", "transcriptomics", NA),
    type = c("bi-directional", "uni-directional", "bi-directional", "bi-directional", "bi-directional"),
    organism = c("Homo sapiens", "Homo sapiens", "Homo sapiens", "Mus musculus", "Homo sapiens"),
    has_difexp = c(1L, 0L, 1L, 1L, NA),
    stringsAsFactors = FALSE
  )
}

test_that("facet choices are 'All' then the distinct values present, sorted", {
  expect_equal(compare_facet_choices(facet_rows(), "assay_type"),
               c("All" = "all", "proteomics" = "proteomics", "transcriptomics" = "transcriptomics"))
  expect_equal(unname(compare_facet_choices(facet_rows(), "has_difexp")), c("all", "yes", "no"))
  expect_equal(compare_facet_choices(facet_rows()[0, ], "organism"), c("All" = "all"))
})

test_that("facets combine, so a template like transcriptomics / bi-directional / Homo sapiens narrows to its rows", {
  got <- compare_facet_filter(facet_rows(), list(
    assay_type = "transcriptomics", type = "bi-directional", organism = "Homo sapiens", has_difexp = "all"
  ))
  expect_equal(got$signature_id, 1)
})

test_that("'all', unset facets, and facets the table lacks filter nothing", {
  expect_equal(nrow(compare_facet_filter(facet_rows(), list(assay_type = "all", organism = NULL, platform = "x"))), 5)
})

test_that("the difexp facet reads the 0/1 flag and drops unknowns", {
  expect_equal(compare_facet_filter(facet_rows(), list(has_difexp = "yes"))$signature_id, c(1, 3, 4))
  expect_equal(compare_facet_filter(facet_rows(), list(has_difexp = "no"))$signature_id, 2)
})

test_that("picks outside the filtered view survive, picks inside it follow the selection", {
  # 1 and 4 picked; the view now shows 3 and 4, with 3 selected and 4 deselected.
  expect_equal(compare_update_picks(c("1", "4"), view_ids = c(3, 4), selected_ids = 3), c("1", "3"))
})

test_that("new picks go after earlier ones, in the order they were selected", {
  expect_equal(compare_update_picks("2", view_ids = 1:5, selected_ids = c(2, 5, 1)), c("2", "5", "1"))
  expect_equal(compare_update_picks(c("1", "2"), view_ids = 1:5, selected_ids = integer()), character())
})

# ---- parsing free text ------------------------------------------------------

test_that("a blank background box means no background, so the function builds its own", {
  expect_null(compare_parse_background(""))
  expect_null(compare_parse_background(NULL))
})

test_that("background features are split on commas, whitespace and newlines", {
  expect_equal(compare_parse_background("TP53, MDM2\tMYC\nTP53"), c("TP53", "MDM2", "MYC"))
})

# ---- uploads ----------------------------------------------------------------

test_that("an uploaded list of OmicSignatures keeps its list names", {
  skip_without_example_data()
  sigs <- example_signatures()[1:2]
  path <- tempfile(fileext = ".rds")
  saveRDS(sigs, path)
  got <- compare_read_signature_upload(path, "sigs.rds")
  expect_equal(names(got), names(sigs))
  expect_true(all(vapply(got, methods::is, logical(1), "OmicSignature")))
})

test_that("a single uploaded OmicSignature is named from its metadata", {
  skip_without_example_data()
  sig <- example_signatures()[[1]]
  path <- tempfile(fileext = ".rds")
  saveRDS(sig, path)
  got <- compare_read_signature_upload(path, "one.rds")
  expect_length(got, 1)
  expect_equal(names(got), sig$metadata$signature_name)
})

test_that("an upload that is not an OmicSignature is refused with the file named", {
  path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1), path)
  expect_error(compare_read_signature_upload(path, "table.rds"), "table.rds")
})

test_that("an upload that is not an rds file is refused with the file named", {
  path <- tempfile(fileext = ".rds")
  writeLines("not an rds", path)
  expect_error(compare_read_signature_upload(path, "notes.rds"), "notes.rds")
})

test_that("several uploads into one list are combined in order", {
  skip_without_example_data()
  sigs <- example_signatures()
  p1 <- tempfile(fileext = ".rds")
  p2 <- tempfile(fileext = ".rds")
  saveRDS(sigs[1:2], p1)
  saveRDS(sigs[3], p2)
  files <- data.frame(name = c("a.rds", "b.rds"), datapath = c(p1, p2), stringsAsFactors = FALSE)
  got <- compare_read_signature_uploads(files)
  expect_equal(names(got), names(sigs)[1:3])
})

test_that("no uploaded files gives an empty list", {
  expect_identical(compare_read_signature_uploads(NULL), list())
})

# ---- the names compareSignatures() will give the signatures ----------------

test_that("ids resolve to database names in the order they were picked", {
  got <- compare_preview_list(db_rows(), signature_ids = c(2, 1))
  expect_equal(got$name, c("beta", "alpha"))
  expect_equal(got$source, c("database", "database"))
})

test_that("fetched signatures sharing a name are told apart by id, as compareSignatures() does", {
  got <- compare_preview_list(db_rows(), signature_ids = c(3, 4))
  expect_equal(got$name, c("shared (id 3)", "shared (id 4)"))
  expect_equal(got$signature_id, c("3", "4"))
})

test_that("an id picked twice appears once and an unknown id is not previewed", {
  got <- compare_preview_list(db_rows(), signature_ids = c(1, 1, 99))
  expect_equal(got$name, "alpha")
})

test_that("uploaded signatures follow the fetched ones and report their own direction", {
  skip_without_example_data()
  uploads <- example_signatures()[1]
  got <- compare_preview_list(db_rows(), signature_ids = 1, omic_signatures = uploads)
  expect_equal(got$name, c("alpha", names(uploads)))
  expect_equal(got$source, c("database", "upload"))
  expect_equal(got$type[2], uploads[[1]]$metadata$type)
})

test_that("each previewed signature carries a hint of the labels it can be paired on", {
  skip_without_example_data()
  rows <- transform(db_rows(), phenotype = c("treated vs control", NA, "x vs y", "x vs y"))
  uploads <- example_signatures()[1]
  got <- compare_preview_list(rows, signature_ids = 1, omic_signatures = uploads)
  expect_equal(got$labels_hint[1], "treated vs control")
  expect_equal(got$labels_hint[2], paste(sort(unique(uploads[[1]]$signature$group_label)), collapse = " / "))
})

test_that("without a phenotype column the database hint is blank", {
  got <- compare_preview_list(db_rows(), signature_ids = 1)
  expect_true(is.na(got$labels_hint))
})

# ---- label pairing ----------------------------------------------------------

test_that("label pairing includes only rows with both levels filled in", {
  got <- compare_label_pairing(c("a", "b", "c"), c("treated", "", ""), c("control", "", ""))
  expect_equal(got, list(a = c("treated", "control")))
})

test_that("no filled rows means no label pairing, so factor order is used", {
  expect_null(compare_label_pairing(c("a", "b"), c("", ""), c("", "")))
  expect_null(compare_label_pairing(character(), character(), character()))
})

test_that("a row with only one level filled in is an error naming the signature", {
  expect_error(compare_label_pairing(c("a", "b"), c("", "up"), c("", "")), "'b'")
})

# ---- arguments --------------------------------------------------------------

settings <- function(...) {
  utils::modifyList(list(
    method = "overlap", score_cutoff = 0, adj_p_cutoff = 0.05, min_features = 5, max_feature = 500,
    alternative = "greater", adjust = FALSE, p_adjust_method = "BH", gsea_score = "NES",
    minSize = 1, maxSize = Inf, background = NULL,
    feature_col = "feature_name", score_col = "score", adj_p_col = "adj_p",
    p_value_col = "p_value", group_col = "group_label"
  ), list(...))
}

test_that("a one-list request leaves every second-list argument out", {
  args <- compare_build_args(
    conn_handler = "conn",
    list1 = list(signature_ids = c(1, 2), omic_signatures = list(), label_pairing = NULL),
    list2 = NULL,
    settings = settings()
  )
  expect_equal(args$signature_ids, c(1, 2))
  expect_false(any(c("signature_ids2", "omic_signatures2", "label_pairing2") %in% names(args)))
  expect_false(any(c("signature_names", "signature_names2") %in% names(args)))
  expect_false("omic_signatures" %in% names(args))
})

test_that("a two-list request passes the second list through the *2 arguments", {
  args <- compare_build_args(
    conn_handler = "conn",
    list1 = list(signature_ids = 1, omic_signatures = list(), label_pairing = NULL),
    list2 = list(signature_ids = 2, omic_signatures = list(), label_pairing = list(beta = c("up", "down"))),
    settings = settings(method = "ks_rank")
  )
  expect_equal(args$signature_ids2, 2)
  expect_equal(args$label_pairing2, list(beta = c("up", "down")))
  expect_equal(args$method, "ks_rank")
})

test_that("no signatures from the database means no connection handler is passed", {
  skip_without_example_data()
  args <- compare_build_args(
    conn_handler = "conn",
    list1 = list(signature_ids = character(), omic_signatures = example_signatures()[1:2], label_pairing = NULL),
    list2 = NULL,
    settings = settings()
  )
  expect_null(args$conn_handler)
})

test_that("every argument the tab builds is one compareSignatures() accepts", {
  testthat::skip_if_not("omic_signatures2" %in% names(formals(SigRepo::compareSignatures)),
                        "installed SigRepo predates the two-list compareSignatures()")
  args <- compare_build_args(
    conn_handler = "conn",
    list1 = list(signature_ids = 1, omic_signatures = list(), label_pairing = list(a = c("x", "y"))),
    list2 = list(signature_ids = 2, omic_signatures = list(), label_pairing = list(b = c("x", "y"))),
    settings = settings(background = c("TP53"))
  )
  expect_length(setdiff(names(args), names(formals(SigRepo::compareSignatures))), 0)
})

# ---- running ----------------------------------------------------------------

test_that("warnings are collected and the result still comes back", {
  runner <- function(...) {
    warning("Some requested signatures were left out of the comparison:\nid 9")
    list(method = "overlap")
  }
  got <- compare_run(list(), runner = runner)
  expect_equal(got$result, list(method = "overlap"))
  expect_match(got$warnings, "left out", all = FALSE)
  expect_null(got$error)
})

test_that("an error is returned word for word with no result", {
  runner <- function(...) stop("\nAt least two signatures are required for a self-comparison; got 1.\n")
  got <- compare_run(list(), runner = runner)
  expect_null(got$result)
  expect_match(got$error, "At least two signatures are required")
})

test_that("the runner receives the built arguments", {
  seen <- NULL
  runner <- function(...) {
    seen <<- list(...)
    list(method = "overlap")
  }
  compare_run(list(method = "gsea", min_features = 7), runner = runner)
  expect_equal(seen, list(method = "gsea", min_features = 7))
})

# ---- reading the result -----------------------------------------------------

overlap_self <- function() {
  OmicSignature::compare_omic_signatures(
    example_signatures()[1:3], method = "overlap",
    score_cutoff = log2(1.025), adj_p_cutoff = 0.01, min_features = 10
  )
}

ks_two_list <- function() {
  sigs <- example_signatures()
  suppressWarnings(OmicSignature::compare_omic_signatures(
    sigs[1:2], sig_list2 = sigs[3:4], method = "ks_rank", adj_p_cutoff = 0.01, min_features = 10
  ))
}

# All-uni-directional overlap: comparisons holds the matrices directly.
flat_overlap <- function() {
  m <- matrix(c(1, 0.2, 0.2, 1), 2, dimnames = list(c("u1", "u2"), c("u1", "u2")))
  list(
    method = "overlap",
    comparisons = list(jaccard = m, pvalue = m * 0.01, counts = m * 10),
    label_order = NULL,
    background = letters
  )
}

test_that("a flat all-uni-directional result is wrapped as a single 'overlap' comparison", {
  got <- compare_comparisons(flat_overlap())
  expect_equal(names(got), "overlap")
  expect_true(is.matrix(got$overlap$jaccard))
})

test_that("a levelled result keeps its level comparisons", {
  skip_without_example_data()
  expect_equal(names(compare_comparisons(overlap_self())), c("level1_vs_level1", "level2_vs_level2"))
})

test_that("overlap results offer jaccard and p-value heatmaps, rank results score and p-value", {
  skip_without_example_data()
  expect_equal(compare_heatmap_measures(overlap_self()), c("jaccard", "pvalue"))
  expect_equal(compare_heatmap_measures(ks_two_list()), c("score", "pvalue"))
})

test_that("overlap tables include counts, rank tables do not", {
  skip_without_example_data()
  expect_equal(compare_table_matrices(overlap_self()), c("jaccard", "pvalue", "counts"))
  expect_equal(compare_table_matrices(ks_two_list()), c("score", "pvalue"))
})

test_that("split mode is only offered for a two-level overlap self-comparison", {
  skip_without_example_data()
  expect_equal(compare_heatmap_modes(overlap_self()), c("separate", "combined", "split"))
  expect_equal(compare_heatmap_modes(ks_two_list()), c("separate", "combined"))
  expect_equal(compare_heatmap_modes(flat_overlap()), "separate")
})

test_that("the triangle choice only applies to overlap self-comparisons", {
  skip_without_example_data()
  expect_true(compare_is_symmetric(overlap_self()))
  expect_false(compare_is_symmetric(ks_two_list()))
})

test_that("the pairs table lists each self-comparison pair once per level, with every measure", {
  skip_without_example_data()
  res <- overlap_self()
  got <- compare_pairs_table(res)
  expect_equal(nrow(got), 3 * 2)
  expect_setequal(names(got), c("comparison", "signature", "signature_label", "reference", "reference_label",
                                "jaccard", "pvalue", "counts"))
  row <- got[got$comparison == "level1_vs_level1" & got$signature == "e7386_hsc3" & got$reference == "e7386_cal27", ]
  expect_equal(row$jaccard, res$comparisons$level1_vs_level1$jaccard["e7386_hsc3", "e7386_cal27"])
  expect_equal(row$counts, res$comparisons$level1_vs_level1$counts["e7386_hsc3", "e7386_cal27"])
  expect_equal(row$signature_label, "DMSO")
})

test_that("the pairs table for two lists pairs every row signature with every column signature", {
  skip_without_example_data()
  res <- ks_two_list()
  got <- compare_pairs_table(res)
  expect_equal(nrow(got), 2 * 2 * 2)
  expect_setequal(names(got), c("comparison", "signature", "signature_label", "reference", "reference_label",
                                "score", "pvalue"))
  row <- got[got$comparison == "level2_vs_level2" & got$signature == "e7386_cal27" & got$reference == "icg001_cal27", ]
  expect_equal(row$score, res$comparisons$level2_vs_level2$score["e7386_cal27", "icg001_cal27"])
  expect_equal(row$reference_label, "ICG001")
})

test_that("the pairs table of a flat result has no labels", {
  got <- compare_pairs_table(flat_overlap())
  expect_equal(nrow(got), 1)
  expect_true(is.na(got$signature_label))
  expect_equal(got$comparison, "overlap")
})

test_that("a matrix is read back from its comparison, and a missing one is NULL", {
  skip_without_example_data()
  res <- overlap_self()
  expect_identical(compare_result_matrix(res, "level1_vs_level1", "counts"), res$comparisons$level1_vs_level1$counts)
  expect_null(compare_result_matrix(res, "level1_vs_level1", "score"))
})

test_that("a self-comparison is recognised with and without label order", {
  skip_without_example_data()
  expect_true(compare_is_self(overlap_self()))
  expect_false(compare_is_self(ks_two_list()))
  expect_true(compare_is_self(flat_overlap()))
})

# ---- the matrix view --------------------------------------------------------

test_that("a self-comparison matrix is labelled S1..Sn on both sides", {
  skip_without_example_data()
  res <- overlap_self()
  labels <- compare_matrix_labels(res)
  expect_equal(unname(labels$rows), c("S1", "S2", "S3"))
  expect_equal(names(labels$rows), rownames(res$comparisons$level1_vs_level1$jaccard))
  expect_identical(labels$cols, labels$rows)
})

test_that("a two-list matrix labels List 2's columns R1..Rm", {
  skip_without_example_data()
  res <- ks_two_list()
  labels <- compare_matrix_labels(res)
  expect_equal(unname(labels$cols), c("R1", "R2"))
  expect_equal(names(labels$cols), colnames(res$comparisons$level1_vs_level1$score))
})

test_that("the key maps each label to its full signature name and list", {
  skip_without_example_data()
  res <- ks_two_list()
  key <- compare_matrix_key(res)
  expect_equal(key$label, c("S1", "S2", "R1", "R2"))
  expect_equal(key$list, c("List 1", "List 1", "List 2", "List 2"))
  expect_equal(key$signature, c(rownames(res$comparisons$level1_vs_level1$score), colnames(res$comparisons$level1_vs_level1$score)))
})

test_that("the key takes database ids from the previewed lists, and leaves uploads blank", {
  skip_without_example_data()
  res <- overlap_self()
  names_in_result <- rownames(res$comparisons$level1_vs_level1$jaccard)
  preview <- data.frame(name = names_in_result, signature_id = c("7", "8", NA), stringsAsFactors = FALSE)
  key <- compare_matrix_key(res, list(preview))
  expect_equal(nrow(key), 3)
  expect_equal(key$signature_id, c("7", "8", NA))
})

test_that("matrix shading ignores a self-comparison's diagonal", {
  m <- matrix(c(1, 0.3, 0.3, 1), 2)
  expect_equal(max(compare_matrix_shading(m, "jaccard", self = TRUE)$cuts), 0.3 * 5 / 6)
  expect_equal(max(compare_matrix_shading(m, "jaccard")$cuts), 5 / 6)
})

test_that("p-value shading darkens with significance and score shading is centred on zero", {
  p <- compare_matrix_shading(matrix(c(0.5, 1e-10), 1), "pvalue")
  expect_true(all(diff(p$cuts) > 0))
  expect_equal(length(p$colors), length(p$cuts) + 1)
  s <- compare_matrix_shading(matrix(c(-2, 1), 1), "score")
  expect_equal(s$cuts, c(-1.2, -0.4, 0.4, 1.2))
  expect_null(compare_matrix_shading(matrix(NA_real_, 1), "jaccard"))
})

test_that("the label order is shown as one table across both lists", {
  skip_without_example_data()
  got <- compare_label_order_table(ks_two_list())
  expect_equal(got$list, c("List 1", "List 1", "List 2", "List 2"))
  expect_equal(got$signature, c("e7386_hsc3", "e7386_cal27", "icg001_hsc3", "icg001_cal27"))
  expect_equal(got$level2[3], "ICG001")
  expect_null(compare_label_order_table(flat_overlap()))
})

test_that("heatmap name space fits the longest signature name instead of ComplexHeatmap's 6 cm", {
  testthat::skip_if_not_installed("ComplexHeatmap")
  long <- strrep("Aging_4mosc1_arecoline_vs_PBS_", 3)
  m <- matrix(0, 2, 2, dimnames = list(c(long, "b"), c("c", long)))
  res <- list(method = "overlap", comparisons = list(jaccard = m, pvalue = m, counts = m))
  args <- compare_heatmap_name_args(res)
  needed <- grid::convertWidth(ComplexHeatmap::max_text_width(long), "cm", valueOnly = TRUE)
  expect_gte(grid::convertWidth(args$row_names_max_width, "cm", valueOnly = TRUE), needed)
  expect_gte(grid::convertHeight(args$column_names_max_height, "cm", valueOnly = TRUE), needed)
  expect_gt(needed, 6)
})

# ---- the equivalent R call --------------------------------------------------

test_that("the equivalent R call shows only arguments that differ from the defaults", {
  args <- list(conn_handler = "conn", signature_ids = c(1, 2), method = "ks_rank", min_features = 5, adj_p_cutoff = 0.01)
  code <- compare_r_call(args)
  expect_match(code, "SigRepo::compareSignatures(", fixed = TRUE)
  expect_match(code, "signature_ids = c(1, 2)", fixed = TRUE)
  expect_match(code, "adj_p_cutoff = 0.01", fixed = TRUE)
  expect_match(code, "method = \"ks_rank\"", fixed = TRUE)
  expect_no_match(code, "min_features")
})

test_that("the R call refers to the connection and uploads by name instead of printing them", {
  skip_without_example_data()
  args <- list(conn_handler = list(password = "secret"), omic_signatures2 = example_signatures()[1:2])
  code <- compare_r_call(args)
  expect_no_match(code, "secret")
  expect_match(code, "conn_handler = conn_handler", fixed = TRUE)
  expect_match(code, "omic_signatures2 = omic_signatures2", fixed = TRUE)
  expect_match(code, "readRDS", fixed = TRUE)
})

test_that("the equivalent R call really runs when pasted", {
  skip_without_example_data()
  testthat::skip_if_not("omic_signatures2" %in% names(formals(SigRepo::compareSignatures)),
                        "installed SigRepo predates the two-list compareSignatures()")
  args <- list(omic_signatures = example_signatures()[1:3], method = "overlap",
               score_cutoff = log2(1.025), adj_p_cutoff = 0.01, min_features = 10)
  code <- compare_r_call(args)
  env <- new.env()
  env$omic_signatures <- args$omic_signatures
  # Drop the placeholder header lines; the uploads are already in `env`.
  eval(parse(text = sub("(?s)^.*\n(?=res <- SigRepo::)", "", code, perl = TRUE)), envir = env)
  expect_equal(env$res$comparisons, do.call(SigRepo::compareSignatures, args)$comparisons)
})
