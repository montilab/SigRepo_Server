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

test_that("search_collections filters by name/user/keyword and respects visibility", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  viewer_results <- search_collections(conn, is_admin = FALSE)
  expect_equal(viewer_results$collection_name, "CI Test Collection")
  expect_equal(viewer_results$signature_count, 2)

  admin_results <- search_collections(conn, is_admin = TRUE)
  expect_true("CI Test Hidden Collection" %in% admin_results$collection_name)

  kw_results <- search_collections(conn, keyword = "MCP search_collections", is_admin = TRUE)
  expect_equal(kw_results$collection_name, "CI Test Collection")

  user_results <- search_collections(conn, user_name = "ci_viewer", is_admin = FALSE)
  expect_equal(nrow(user_results), 1)
})

test_that("search_geneset_resources filters by source/species/collection and current_only", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  current_results <- search_geneset_resources(conn, source = "MSigDB")
  expect_equal(nrow(current_results), 1)
  expect_equal(current_results$version, "2023.2")

  all_results <- search_geneset_resources(conn, source = "MSigDB", current_only = FALSE)
  expect_setequal(all_results$version, c("2023.2", "2022.1"))

  species_results <- search_geneset_resources(conn, species = "Does Not Exist")
  expect_equal(nrow(species_results), 0)
})

test_that("search_geneset_entries filters by resource id and keyword", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  by_keyword <- search_geneset_entries(conn, keyword = "HALLMARK")
  expect_equal(by_keyword$geneset_name, "CI_TEST_HALLMARK_SET")
  expect_equal(by_keyword$source, "MSigDB")

  resource_id <- by_keyword$geneset_resource_id[1]
  by_resource <- search_geneset_entries(conn, geneset_resource_id = resource_id)
  expect_equal(nrow(by_resource), 1)

  none_results <- search_geneset_entries(conn, keyword = "Does Not Exist")
  expect_equal(nrow(none_results), 0)
})

test_that("search_features looks up transcriptomics/proteomics/snps by name and organism", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  transc <- search_features(conn, assay_type = "transcriptomics", feature_name = "CI_TEST_GENE")
  expect_equal(transc$organism, "CI Test Organism")

  prot <- search_features(conn, assay_type = "proteomics", organism = "CI Test Organism")
  expect_equal(prot$feature_name, "CI_TEST_PROTEIN")

  snp <- search_features(conn, assay_type = "snps", feature_name = "rs_ci_test_variant")
  expect_equal(nrow(snp), 1)

  expect_error(
    search_features(conn, assay_type = "methylomics"),
    "Unsupported assay_type"
  )
})

test_that("search_features dispatches metabolomics lookups by feature_database namespace", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))

  by_refmet <- search_features(conn, assay_type = "metabolomics", feature_database = "refmet",
                                feature_name = "CI Test Metabolite")
  expect_equal(nrow(by_refmet), 1)

  by_hmdb <- search_features(conn, assay_type = "metabolomics", feature_database = "hmdb",
                              feature_name = "HMDB0000001")
  expect_equal(by_hmdb$metabolite_id, by_refmet$metabolite_id)

  expect_error(
    search_features(conn, assay_type = "metabolomics"),
    "feature_database is required"
  )

  expect_error(
    search_metabolomics_features(conn, feature_database = "not-a-namespace"),
    "Unsupported feature_database"
  )
})
