source(testthat::test_path("../../api/lib/feature_data.R"), local = FALSE)

test_that("parse_organism_filter splits, trims, sorts, and dedupes comma-separated organisms", {
  expect_equal(parse_organism_filter("Human, Mouse"), c("Human", "Mouse"))
  expect_equal(parse_organism_filter(c("Mouse", " mouse ", "Human")), sort(unique(trimws(c("Mouse", " mouse ", "Human")))))
  expect_equal(parse_organism_filter(NULL), NULL)
  expect_equal(parse_organism_filter(""), "")
  expect_equal(parse_organism_filter(NA), NA)
})

# run_feature_updates(): runs one updater per organism and reports every
# outcome instead of swallowing errors (the routes used to answer "Finish
# updating ..." even when an organism had failed).

organisms <- data.frame(organism = c("Homo sapiens", "Mus musculus"), stringsAsFactors = FALSE)

test_that("run_feature_updates reports success per organism", {
  result <- run_feature_updates(organisms, function(organism) invisible(NULL))
  expect_true(result$ok)
  expect_equal(result$messages, c("Homo sapiens: updated", "Mus musculus: updated"))
})

test_that("run_feature_updates keeps going after a failure and reports it with the organism name", {
  calls <- character()
  result <- run_feature_updates(organisms, function(organism) {
    calls <<- c(calls, organism)
    if (organism == "Homo sapiens") stop("There are no features returned from biomaRt. Updates aborted.")
  })
  expect_equal(calls, c("Homo sapiens", "Mus musculus"))
  expect_false(result$ok)
  expect_match(result$messages[1], "^Homo sapiens: failed: .*no features returned from biomaRt")
  expect_equal(result$messages[2], "Mus musculus: updated")
})

test_that("run_feature_updates treats the client's version no-op as a skip, not a failure", {
  result <- run_feature_updates(organisms, function(organism) {
    stop("Current biomaRt version = '116' is the same as the database version = '116'. No updates needed.\n")
  })
  expect_true(result$ok)
  expect_match(result$messages[1], "^Homo sapiens: skipped: .*No updates needed")
})
