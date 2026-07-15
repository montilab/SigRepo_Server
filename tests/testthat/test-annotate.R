source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

skip_if_offline <- function() {
  testthat::skip_if_not_installed("curl")
  testthat::skip_if_not(curl::has_internet(), "no network access (required to fetch MSigDB gene sets)")
}

local_tempdir <- function() {
  dir <- tempfile("sigrepo-annotate-test-")
  dir.create(dir)
  dir
}

test_that("enrichment_reference_table maps assay_type to the right features table", {
  expect_equal(enrichment_reference_table("transcriptomics"), "transcriptomics_features")
  expect_equal(enrichment_reference_table("proteomics"), "proteomics_features")
  expect_null(enrichment_reference_table("metabolomics"))
  expect_null(enrichment_reference_table("snps"))
})

test_that("resolve_enrichment_query resolves hypergeometric queries to real gene symbols", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }

  # The seeded signature's two features (feature_id 1/2, see seed.sql) have
  # no matching transcriptomics_features rows -- add them here so the
  # gene_symbol join has something real to resolve.
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- resolve_enrichment_query(auth, "ci_test_signature_hashkey_0000", "hypergeometric", difexp_dir = tempdir())

  expect_true(result$ok)
  expect_setequal(result$query, c("TP53", "BRCA1"))
  expect_equal(result$signature_name, "CI Test Signature")
})

test_that("resolve_enrichment_query reports no_gene_symbols when features don't map to any", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- resolve_enrichment_query(auth, "ci_test_signature_hashkey_0000", "hypergeometric", difexp_dir = tempdir())

  expect_false(result$ok)
  expect_equal(result$reason, "no_gene_symbols")
})

test_that("resolve_enrichment_query reports not_found for an unknown signature", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- resolve_enrichment_query(auth, "does-not-exist-hashkey", "hypergeometric", difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "not_found")
})

test_that("resolve_enrichment_query requires difexp for kstest, and resolves it correctly when present", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")

  # has_difexp = 0 for the seeded signature -> no_difexp, regardless of files on disk.
  no_difexp_result <- resolve_enrichment_query(auth, "ci_test_signature_hashkey_0000", "kstest", difexp_dir = tempdir())
  expect_false(no_difexp_result$ok)
  expect_equal(no_difexp_result$reason, "no_difexp")

  exec_sql("UPDATE signatures SET has_difexp = 1 WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'")
  on.exit(exec_sql("UPDATE signatures SET has_difexp = 0 WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'"), add = TRUE)

  difexp_dir <- local_tempdir()
  difexp_tbl <- data.frame(
    probe_id = c("probe_1", "probe_2", "probe_unmapped"),
    score = c(2.5, -1.2, 9.9),
    stringsAsFactors = FALSE
  )
  saveRDS(difexp_tbl, file.path(difexp_dir, "ci_test_signature_hashkey_0000.RDS"))

  kstest_result <- resolve_enrichment_query(auth, "ci_test_signature_hashkey_0000", "kstest", difexp_dir = difexp_dir)
  expect_true(kstest_result$ok)
  expect_setequal(names(kstest_result$query), c("TP53", "BRCA1"))
  expect_equal(unname(kstest_result$query["TP53"]), 2.5)
  expect_equal(unname(kstest_result$query["BRCA1"]), -1.2)
})

test_that("run_enrichment computes real hypergeometric enrichment against a live MSigDB collection", {
  skip_if_no_test_db()
  skip_if_offline()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- run_enrichment(
    auth, "ci_test_signature_hashkey_0000", test = "hypergeometric",
    species = "Homo sapiens", collection = "H", fdr = 1, difexp_dir = tempdir()
  )

  expect_true(result$ok)
  expect_equal(result$n_query, 2)
  expect_true(is.data.frame(result$results))
  expect_true(all(c("label", "pval", "fdr", "overlap") %in% colnames(result$results)))
  expect_true(nrow(result$results) > 0)
})
