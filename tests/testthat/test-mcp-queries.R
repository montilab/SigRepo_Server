source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../mcp/lib/queries.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("list_vocabulary returns the distinct in-use values from the seeded fixture", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  vocab <- list_vocabulary(conn)

  expect_equal(vocab$organism, "CI Test Organism")
  expect_equal(vocab$phenotype, "CI Test Phenotype")
  expect_equal(vocab$sample_type, "CI Test Sample Type")
  expect_equal(vocab$platform, "CI Test Platform")
  expect_equal(vocab$assay_type, "transcriptomics")
})

test_that("search_signatures filters by organism/keyword/assay_type and respects visibility", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  # Non-admin: hidden fixture signature must not appear.
  viewer_results <- search_signatures(conn, is_admin = FALSE)
  expect_setequal(
    viewer_results$signature_hashkey,
    c("ci_test_signature_hashkey_0000", "ci_test_signature_hashkey_0001")
  )

  # Admin: hidden fixture signature must appear too.
  admin_results <- search_signatures(conn, is_admin = TRUE)
  expect_true("ci_test_signature_hashkey_hidn" %in% admin_results$signature_hashkey)

  # Keyword match against signature_name.
  kw_results <- search_signatures(conn, keyword = "Signature 2", is_admin = TRUE)
  expect_equal(kw_results$signature_hashkey, "ci_test_signature_hashkey_0001")

  # Organism filter narrows correctly; a non-existent organism returns zero rows.
  none_results <- search_signatures(conn, organism = "Does Not Exist", is_admin = TRUE)
  expect_equal(nrow(none_results), 0)

  # feature_count is computed correctly per signature.
  by_hashkey <- viewer_results[viewer_results$signature_hashkey == "ci_test_signature_hashkey_0000", ]
  expect_equal(by_hashkey$feature_count, 2)

  # limit is clamped to a sane default/max rather than erroring.
  expect_no_error(search_signatures(conn, limit = 0, is_admin = TRUE))
  expect_no_error(search_signatures(conn, limit = 500, is_admin = TRUE))
})

test_that("compare_two_signatures computes similarity and rejects invalid pairs", {
  skip_if_no_test_db()

  result <- compare_two_signatures("ci_test_signature_hashkey_0000", "ci_test_signature_hashkey_0001")
  expect_equal(result$shared_features, 1)
  expect_equal(result$features_1, 2)
  expect_equal(result$features_2, 2)
  expect_equal(result$jaccard_similarity, 1 / 3)

  expect_error(
    compare_two_signatures("ci_test_signature_hashkey_0000", "ci_test_signature_hashkey_0000"),
    "must be different signatures"
  )

  expect_error(
    compare_two_signatures("ci_test_signature_hashkey_0000", "does-not-exist"),
    "Could not find signature"
  )
})
