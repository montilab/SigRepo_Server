source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("feature_ids_from_context extracts probe_id or feature_id and dedupes", {
  context <- list(features = list(
    list(probe_id = "p1", feature_id = NA),
    list(probe_id = "p1", feature_id = NA),
    list(probe_id = NA, feature_id = 42)
  ))
  expect_equal(sort(feature_ids_from_context(context)), sort(c("p1", "42")))
  expect_equal(feature_ids_from_context(list(features = list())), character())
})

test_that("signature_similarity_summary computes Jaccard similarity between pairs", {
  contexts <- list(
    sig_a = list(features = list(list(probe_id = "1"), list(probe_id = "2"))),
    sig_b = list(features = list(list(probe_id = "2"), list(probe_id = "3")))
  )
  similarity <- signature_similarity_summary(contexts)
  expect_equal(nrow(similarity), 1)
  expect_equal(similarity$shared_features, 1)
  expect_equal(similarity$jaccard_similarity, 1 / 3)

  expect_equal(nrow(signature_similarity_summary(list(only_one = list()))), 0)
})

test_that("draft_signature_groups merges signatures above the similarity threshold", {
  similarity_tbl <- data.frame(
    signature_hashkey_1 = c("a", "b"),
    signature_hashkey_2 = c("b", "c"),
    jaccard_similarity = c(0.5, 0.05),
    stringsAsFactors = FALSE
  )

  groups <- draft_signature_groups(similarity_tbl, threshold = 0.10)
  expect_length(groups, 2)
  expect_true(any(vapply(groups, function(g) setequal(g, c("a", "b")), logical(1))))
  expect_true(any(vapply(groups, function(g) identical(g, "c"), logical(1))))

  expect_equal(draft_signature_groups(data.frame()), list())
})

test_that("fetch_signature_context joins reference tables and returns features for a seeded signature", {
  skip_if_no_test_db()

  context <- fetch_signature_context(
    "ci_test_signature_hashkey_0000",
    include_features = TRUE,
    max_features = 50,
    auth = list(user_role = "admin", user_name = "ci_viewer")
  )

  expect_equal(context$signature$organism, "CI Test Organism")
  expect_equal(context$signature$phenotype, "CI Test Phenotype")
  expect_equal(context$feature_count, 2)
  expect_length(context$features, 2)
})

test_that("fetch_signature_context returns NULL for a signature that does not exist", {
  skip_if_no_test_db()
  expect_null(fetch_signature_context("does-not-exist-hashkey"))
})
