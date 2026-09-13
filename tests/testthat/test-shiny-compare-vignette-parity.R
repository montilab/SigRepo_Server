# Every comparison in OmicSignature's compare vignettes (CompareSignatures,
# CompareMixedDirection, CompareUnidirectional), run through the Shiny Compare
# tab and checked against the vignette's own code. The tab uploads the same
# example signatures and types the same settings a user would; its result
# must be identical() to what the vignette computes, and where the vignette
# expects an error the tab must show that error.
load_compare_app()

# The vignettes' shared label pairing for the three bi-directional toy signatures.
vignette_pairing <- list(
  signature_a = c("treated", "control"),
  signature_b = c("up", "down"),
  signature_c = c("resistant", "sensitive")
)

# Run one comparison through the tab and return its run state.
# `inputs` are the tab's input values; `pairing` is typed into List 1's rows.
run_tab <- function(list1, list2 = NULL, inputs = list(), pairing = NULL, seed = NULL) {
  out <- NULL
  run_compare_module({
    upload_inputs <- list(list1_upload = compare_upload_of(list1), two_lists = !is.null(list2))
    if (!is.null(list2)) {
      upload_inputs$list2_upload <- compare_upload_of(list2)
    }
    do.call(session$setInputs, c(upload_inputs, inputs))
    if (!is.null(pairing)) {
      compare_set_pairing(session, 1, pairing)
    }
    if (!is.null(seed)) {
      set.seed(seed)
    }
    session$setInputs(run = 1)
    out <<- session$returned$run_state()
  })
  out
}

expect_tab_matches_vignette <- function(state, vignette_result) {
  expect_null(state$error)
  expect_identical(state$result, vignette_result)
}

# ---- CompareSignatures.Rmd --------------------------------------------------

test_that("CompareSignatures: overlap self-comparison (overlap_res) matches", {
  skip_without_two_list_client()
  signature_list <- compare_example_data("compare_signatures_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = signature_list,
    method = "overlap",
    score_cutoff = log2(1.25),
    adj_p_cutoff = 0.05,
    min_features = 25,
    max_feature = 2000
  ))
  state <- run_tab(signature_list, inputs = list(
    method = "overlap", score_cutoff = log2(1.25), adj_p_cutoff = 0.05, min_features = 25, max_feature = 2000
  ))
  expect_tab_matches_vignette(state, vignette)
})

# Guards the check above: identical() must notice when the tab is given a
# different setting. (Cutoffs at or below 1.5 would not do: every example
# signature's |score| is already at least 1.5.)
test_that("the parity check notices a setting the vignette did not use", {
  skip_without_two_list_client()
  signature_list <- compare_example_data("compare_signatures_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = signature_list, method = "overlap", score_cutoff = log2(1.25),
    adj_p_cutoff = 0.05, min_features = 25, max_feature = 2000
  ))
  state <- run_tab(signature_list, inputs = list(
    method = "overlap", score_cutoff = 3, adj_p_cutoff = 0.05, min_features = 25, max_feature = 2000
  ))
  expect_null(state$error)
  expect_false(identical(state$result, vignette))
})

test_that("CompareSignatures: query vs reference overlap (cross_res) matches", {
  skip_without_two_list_client()
  compare_signatures_example <- compare_example_data("compare_signatures_example")
  reference_signatures <- compare_signatures_example[1:2]
  query_signatures <- compare_signatures_example[3:4]
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = query_signatures,
    sig_list2 = reference_signatures,
    method = "overlap",
    score_cutoff = log2(1.25),
    adj_p_cutoff = 0.05,
    min_features = 25,
    max_feature = 500
  ))
  state <- run_tab(query_signatures, reference_signatures, inputs = list(
    method = "overlap", score_cutoff = log2(1.25), adj_p_cutoff = 0.05, min_features = 25, max_feature = 500
  ))
  expect_tab_matches_vignette(state, vignette)
  expect_equal(dim(state$result$comparisons$level1_vs_level1$jaccard), c(2L, 2L))
})

test_that("CompareSignatures: overlap with label pairing (paired_res) matches", {
  skip_without_two_list_client()
  compare_label_pairing_example <- compare_example_data("compare_label_pairing_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = compare_label_pairing_example,
    method = "overlap",
    label_pairing = vignette_pairing
  ))
  state <- run_tab(compare_label_pairing_example, inputs = list(method = "overlap"), pairing = vignette_pairing)
  expect_identical(state$args$label_pairing, vignette_pairing)
  expect_tab_matches_vignette(state, vignette)
})

test_that("CompareSignatures: KS rank self-comparison (ks_res) matches", {
  skip_without_two_list_client()
  signature_list <- compare_example_data("compare_signatures_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = signature_list,
    method = "ks_rank",
    adj_p_cutoff = 0.05,
    min_features = 25,
    max_feature = 500
  ))
  state <- run_tab(signature_list, inputs = list(
    method = "ks_rank", adj_p_cutoff = 0.05, min_features = 25, max_feature = 500
  ))
  expect_tab_matches_vignette(state, vignette)
})

test_that("CompareSignatures: KS score with label pairing (toy_ks_score) matches", {
  skip_without_two_list_client()
  compare_label_pairing_example <- compare_example_data("compare_label_pairing_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = compare_label_pairing_example,
    method = "ks_score",
    label_pairing = vignette_pairing,
    min_features = 5
  ))
  state <- run_tab(compare_label_pairing_example, inputs = list(method = "ks_score", min_features = 5),
                   pairing = vignette_pairing)
  expect_tab_matches_vignette(state, vignette)
})

test_that("CompareSignatures: the commented-out GSEA call (gsea_res) matches under the same seed", {
  skip_without_two_list_client()
  testthat::skip_if_not_installed("fgsea")
  signature_list <- compare_example_data("compare_signatures_example")
  set.seed(42)
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = signature_list,
    method = "gsea",
    adj_p_cutoff = 0.05,
    min_features = 25,
    max_feature = 500,
    minSize = 10,
    maxSize = 500
  ))
  state <- run_tab(signature_list, inputs = list(
    method = "gsea", adj_p_cutoff = 0.05, min_features = 25, max_feature = 500, min_size = 10, max_size = 500
  ), seed = 42)
  expect_tab_matches_vignette(state, vignette)
})

# The vignette's four signature_similarity_heatmap() calls: the tab offers each
# measure/mode/triangle combination and draws it from the matching result.
test_that("CompareSignatures: every vignette heatmap is offered and draws in the tab", {
  skip_without_two_list_client()
  testthat::skip_if_not_installed("ComplexHeatmap")
  testthat::skip_if_not_installed("circlize")
  signature_list <- compare_example_data("compare_signatures_example")
  heatmaps <- list(
    list(inputs = list(method = "overlap", score_cutoff = log2(1.25), min_features = 25, max_feature = 2000),
         measure = "jaccard", modes = c("separate", "combined"), triangle = "upper"),
    list(inputs = list(method = "ks_rank", min_features = 25, max_feature = 500),
         measure = "score", modes = c("combined", "separate"), triangle = "upper")
  )
  for (h in heatmaps) {
    run_compare_module({
      do.call(session$setInputs, c(list(list1_upload = compare_upload_of(signature_list), two_lists = FALSE), h$inputs))
      session$setInputs(run = 1)
      res <- session$returned$run_state()$result
      expect_true(h$measure %in% compare_heatmap_measures(res))
      for (mode in h$modes) {
        expect_true(mode %in% compare_heatmap_modes(res))
        session$setInputs(heatmap_measure = h$measure, heatmap_mode = mode, heatmap_triangle = h$triangle,
                          heatmap_cluster = "ward.D", heatmap_na_style = "grey")
        expect_type(output$heatmap, "list")
      }
    })
  }
})

# ---- CompareMixedDirection.Rmd ----------------------------------------------

test_that("CompareMixedDirection: overlap of mixed directions (mixed_res) matches", {
  skip_without_two_list_client()
  compare_mixed_direction_example <- compare_example_data("compare_mixed_direction_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = compare_mixed_direction_example,
    method = "overlap",
    label_pairing = vignette_pairing,
    min_features = 3
  ))
  state <- run_tab(compare_mixed_direction_example, inputs = list(method = "overlap", min_features = 3),
                   pairing = vignette_pairing)
  expect_tab_matches_vignette(state, vignette)
})

test_that("CompareMixedDirection: the uni-directional signature gets no label pairing inputs", {
  skip_without_two_list_client()
  compare_mixed_direction_example <- compare_example_data("compare_mixed_direction_example")
  run_compare_module({
    session$setInputs(list1_upload = compare_upload_of(compare_mixed_direction_example), two_lists = FALSE)
    expect_setequal(names(session$returned$pairing_input_ids(1)), c("signature_a", "signature_b", "signature_c"))
  })
})

test_that("CompareMixedDirection: KS rank of mixed directions (ks_res) matches", {
  skip_without_two_list_client()
  compare_mixed_direction_example <- compare_example_data("compare_mixed_direction_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = compare_mixed_direction_example,
    method = "ks_rank",
    label_pairing = vignette_pairing,
    min_features = 3
  ))
  state <- run_tab(compare_mixed_direction_example, inputs = list(method = "ks_rank", min_features = 3),
                   pairing = vignette_pairing)
  expect_tab_matches_vignette(state, vignette)
  expect_true(all(is.na(state$result$comparisons$level1_vs_level1$score[, "signature_d"])))
})

# ---- CompareUnidirectional.Rmd ----------------------------------------------

test_that("CompareUnidirectional: flat overlap (uni_res) matches", {
  skip_without_two_list_client()
  compare_unidirectional_example <- compare_example_data("compare_unidirectional_example")
  vignette <- suppressWarnings(OmicSignature::compare_omic_signatures(
    sig_list1 = compare_unidirectional_example,
    method = "overlap",
    min_features = 3
  ))
  state <- run_tab(compare_unidirectional_example, inputs = list(method = "overlap", min_features = 3))
  expect_tab_matches_vignette(state, vignette)
  expect_null(compare_label_order_table(state$result))
  expect_equal(compare_heatmap_modes(state$result), "separate")
})

test_that("CompareUnidirectional: KS rank shows the same error the vignette's try() catches", {
  skip_without_two_list_client()
  compare_unidirectional_example <- compare_example_data("compare_unidirectional_example")
  vignette_error <- tryCatch(
    OmicSignature::compare_omic_signatures(compare_unidirectional_example, method = "ks_rank"),
    error = function(e) trimws(conditionMessage(e))
  )
  expect_type(vignette_error, "character")
  state <- run_tab(compare_unidirectional_example, inputs = list(method = "ks_rank"))
  expect_null(state$result)
  expect_identical(state$error, vignette_error)
})
