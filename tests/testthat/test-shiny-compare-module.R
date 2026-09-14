# The Shiny Compare tab end to end through shiny::testServer(), with no
# database: signatures come in as .rds uploads of OmicSignature's bundled
# examples, so the real SigRepo::compareSignatures() runs on them.
load_compare_app()

test_that("a self-comparison of uploaded signatures runs compareSignatures() and keeps the result", {
  skip_without_two_list_client()
  sigs <- compare_example_data("compare_signatures_example")[1:3]
  run_compare_module({
    session$setInputs(
      list1_upload = compare_upload_of(sigs), two_lists = FALSE, method = "overlap",
      score_cutoff = log2(1.025), adj_p_cutoff = 0.01, min_features = 10, max_feature = 500
    )
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_null(state$error)
    expect_equal(state$result$method, "overlap")
    expect_equal(rownames(state$result$comparisons$level1_vs_level1$jaccard), names(sigs))
    expect_false("conn_handler" %in% names(state$args))
    expect_equal(
      state$result$comparisons,
      SigRepo::compareSignatures(omic_signatures = sigs, method = "overlap", score_cutoff = log2(1.025),
                                 adj_p_cutoff = 0.01, min_features = 10)$comparisons
    )
  })
})

test_that("with the second list switched on, uploads go through omic_signatures2", {
  skip_without_two_list_client()
  sigs <- compare_example_data("compare_signatures_example")
  run_compare_module({
    session$setInputs(
      list1_upload = compare_upload_of(sigs[1:2]), two_lists = TRUE, list2_upload = compare_upload_of(sigs[3:4]),
      method = "ks_rank", adj_p_cutoff = 0.01, min_features = 10
    )
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_null(state$error)
    expect_equal(names(state$args$omic_signatures2), names(sigs)[3:4])
    expect_equal(rownames(state$result$label_order$sig_list2), names(sigs)[3:4])
    expect_equal(colnames(state$result$comparisons$level1_vs_level1$score), names(sigs)[3:4])
  })
})

test_that("a ranking signature without p_value is ranked by pvalue or adj_p, and the fallback warning is shown", {
  skip_without_two_list_client()
  testthat::skip_if_not(
    exists("fillRankingPValues", envir = asNamespace("SigRepo"), inherits = FALSE),
    "installed SigRepo predates the p-value column fallback (montilab/SigRepo#212)"
  )
  sigs <- compare_example_data("compare_signatures_example")
  edit_difexp <- function(sig, edit) {
    copy <- sig$clone(deep = TRUE)
    copy$difexp <- edit(copy$difexp)
    copy
  }
  renamed <- edit_difexp(sigs[[3]], function(d) {
    names(d)[names(d) == "p_value"] <- "pvalue"
    d
  })
  adj_only <- edit_difexp(sigs[[4]], function(d) d[, names(d) != "p_value", drop = FALSE])
  list2 <- stats::setNames(list(renamed, adj_only), names(sigs)[3:4])

  run_compare_module({
    session$setInputs(
      list1_upload = compare_upload_of(sigs[1:2]), two_lists = TRUE, list2_upload = compare_upload_of(list2),
      method = "ks_rank", adj_p_cutoff = 0.01, min_features = 10
    )
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_null(state$error)
    expect_true(all(is.finite(state$result$comparisons$level1_vs_level1$score)))
    fallback <- state$warnings[grepl("adj_p", state$warnings)]
    expect_length(fallback, 1)
    expect_match(fallback, names(sigs)[4], fixed = TRUE)
    expect_no_match(fallback, names(sigs)[3], fixed = TRUE)
    expect_match(as.character(output$run_messages$html), "Ranking by &#39;adj_p&#39;|Ranking by 'adj_p'")
  })
})

test_that("output reads do not wait on callbacks other code left on later's global loop", {
  # The shape of the maintenance task an open pool::dbPool() keeps: it
  # reschedules itself, forever in a pool's case. testServer() reads an output
  # by running later's loop until it is empty, so on the global loop the read
  # below would wait out every tick. This one stops after ~20 s so a regression
  # fails on time instead of hanging the suite.
  ticks_left <- 100
  handle <- NULL
  tick <- function() {
    ticks_left <<- ticks_left - 1
    if (ticks_left > 0) handle <<- later::later(tick, 0.2)
  }
  tick()
  on.exit(if (ticks_left > 0) handle(), add = TRUE)

  started <- Sys.time()
  run_compare_module({
    session$setInputs(two_lists = FALSE, method = "overlap")
    expect_match(as.character(output$run_readiness$html), "at least two signatures")
  })
  expect_lt(as.numeric(difftime(Sys.time(), started, units = "secs")), 5)
})

test_that("a second list that is switched off is not sent even if it still holds uploads", {
  skip_without_two_list_client()
  sigs <- compare_example_data("compare_signatures_example")
  run_compare_module({
    session$setInputs(
      list1_upload = compare_upload_of(sigs[1:2]), two_lists = FALSE, list2_upload = compare_upload_of(sigs[3:4]),
      method = "overlap", adj_p_cutoff = 0.01, min_features = 10
    )
    session$setInputs(run = 1)
    expect_false("omic_signatures2" %in% names(session$returned$run_state()$args))
  })
})

test_that("compareSignatures()'s own error is shown when a self-comparison has one signature", {
  skip_without_two_list_client()
  sigs <- compare_example_data("compare_signatures_example")[1]
  run_compare_module({
    session$setInputs(list1_upload = compare_upload_of(sigs), two_lists = FALSE, method = "overlap")
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_null(state$result)
    expect_match(state$error, "At least two signatures are required")
  })
})

test_that("rows picked from the signature table are requested by id, with the user's connection", {
  run_compare_module({
    session$setInputs(two_lists = FALSE, method = "overlap")
    session$setInputs(list1_table_rows_selected = c(2, 1))
    session$setInputs(run = 1)
    args <- session$returned$run_state()$args
    expect_equal(args$signature_ids, c(12, 11))
    expect_false("signature_names" %in% names(args))
    expect_equal(args$conn_handler, "user-conn")
  })
})

test_that("a pick survives a facet that hides it, and deselecting in the filtered view drops only visible picks", {
  run_compare_module({
    session$setInputs(two_lists = FALSE, method = "overlap")
    session$setInputs(list1_table_rows_selected = c(1, 2))
    # has_difexp = yes leaves only "alpha" (id 11) in view, still selected.
    session$setInputs(list1_facet_has_difexp = "yes")
    expect_equal(session$returned$previews[[1]]()$name, c("alpha", "beta"))
    session$setInputs(list1_table_rows_selected = integer())
    expect_equal(session$returned$previews[[1]]()$name, "beta")
    session$setInputs(list1_clear = 1)
    expect_equal(nrow(session$returned$previews[[1]]()), 0)
  })
})

test_that("label pairing typed per signature reaches compareSignatures() and sets the label order", {
  skip_without_two_list_client()
  sigs <- compare_example_data("compare_label_pairing_example")
  run_compare_module({
    session$setInputs(list1_upload = compare_upload_of(sigs), two_lists = FALSE, method = "overlap", min_features = 5)
    ids <- session$returned$pairing_input_ids(1)
    expect_equal(names(ids), names(sigs))
    inputs <- list()
    inputs[[ids[["signature_a"]][["level1"]]]] <- "control"
    inputs[[ids[["signature_a"]][["level2"]]]] <- "treated"
    do.call(session$setInputs, inputs)
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_equal(state$args$label_pairing, list(signature_a = c("control", "treated")))
    expect_equal(unname(state$result$label_order$sig_list1["signature_a", ]), c("control", "treated"))
  })
})

test_that("a half-filled label pairing stops the run before compareSignatures() is called", {
  skip_without_two_list_client()
  sigs <- compare_example_data("compare_label_pairing_example")
  run_compare_module({
    session$setInputs(list1_upload = compare_upload_of(sigs), two_lists = FALSE, method = "overlap")
    ids <- session$returned$pairing_input_ids(1)
    inputs <- list()
    inputs[[ids[["signature_b"]][["level1"]]]] <- "up"
    do.call(session$setInputs, inputs)
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_null(state$result)
    expect_null(state$args)
    expect_match(state$error, "'signature_b'")
  })
})

test_that("an unreadable upload is reported and nothing is run", {
  path <- tempfile(fileext = ".rds")
  saveRDS(list(1, 2), path)
  bad <- data.frame(name = "junk.rds", size = 1, type = "", datapath = path, stringsAsFactors = FALSE)
  run_compare_module({
    session$setInputs(list1_upload = bad, two_lists = FALSE, method = "overlap")
    session$setInputs(run = 1)
    state <- session$returned$run_state()
    expect_null(state$args)
    expect_match(state$error, "junk.rds")
  })
})

test_that("the heatmap and tables render from a finished comparison", {
  skip_without_two_list_client()
  testthat::skip_if_not_installed("ComplexHeatmap")
  testthat::skip_if_not_installed("circlize")
  sigs <- compare_example_data("compare_signatures_example")[1:3]
  run_compare_module({
    session$setInputs(
      list1_upload = compare_upload_of(sigs), two_lists = FALSE, method = "overlap",
      score_cutoff = log2(1.025), adj_p_cutoff = 0.01, min_features = 10
    )
    session$setInputs(run = 1)
    session$setInputs(heatmap_measure = "jaccard", heatmap_mode = "combined", heatmap_triangle = "upper",
                      heatmap_cluster = "ward.D", heatmap_na_style = "grey",
                      table_comparison = "level1_vs_level1", table_matrix = "counts")
    expect_type(output$heatmap, "list")
    expect_match(output$pairs_table, "e7386_cal27")
    expect_match(output$matrix_table, "\"S3\"")
    expect_match(output$matrix_key, "e7386_hsc3")
    expect_match(output$r_call, "SigRepo::compareSignatures(", fixed = TRUE)
  })
})
