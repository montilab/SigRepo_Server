source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

local_tempdir <- function() {
  dir <- tempfile("sigrepo-create-signature-test-")
  dir.create(dir)
  dir
}

valid_upload <- function(signature_name, extra_features = data.frame()) {
  list(
    metadata = list(
      signature_name = signature_name,
      direction_type = "bi-directional",
      assay_type = "transcriptomics",
      organism = "CI Test Organism",
      phenotype = "CI Test Phenotype",
      sample_type = "CI Test Sample Type",
      platform_name = "CI Test Platform",
      description = "A test signature",
      keywords = "test,upload"
    ),
    signature = rbind(
      data.frame(probe_id = "1020", feature_id = 1, score = 2.5, group_label = "All Features", stringsAsFactors = FALSE),
      data.frame(probe_id = "1023", feature_id = 2, score = -1.2, group_label = "All Features", stringsAsFactors = FALSE),
      extra_features
    ),
    difexp = NULL
  )
}

test_that("validate_upload_shape rejects missing metadata fields and missing feature columns", {
  bad_meta <- validate_upload_shape(list(metadata = list(signature_name = "x"), signature = data.frame(probe_id = 1)))
  expect_false(bad_meta$ok)
  expect_equal(bad_meta$reason, "invalid_upload")
  expect_match(bad_meta$message, "direction_type")

  bad_features <- validate_upload_shape(list(
    metadata = as.list(setNames(rep("x", length(REQUIRED_UPLOAD_METADATA_FIELDS)), REQUIRED_UPLOAD_METADATA_FIELDS)),
    signature = data.frame(probe_id = 1)
  ))
  expect_false(bad_features$ok)
  expect_equal(bad_features$reason, "invalid_upload")

  expect_null(validate_upload_shape(valid_upload("ok")))
})

test_that("build_signature_from_upload rejects viewers and unsupported assay types", {
  skip_if_no_test_db()
  viewer_auth <- list(user_name = "ci_viewer", user_role = "viewer")
  editor_auth <- list(user_name = "ci_admin", user_role = "admin")

  forbidden <- build_signature_from_upload(viewer_auth, valid_upload("Forbidden Test"), difexp_dir = tempdir())
  expect_false(forbidden$ok)
  expect_equal(forbidden$reason, "forbidden")

  upload <- valid_upload("Bad Assay Test")
  upload$metadata$assay_type <- "metabolomics"
  bad_assay <- build_signature_from_upload(editor_auth, upload, difexp_dir = tempdir())
  expect_false(bad_assay$ok)
  expect_equal(bad_assay$reason, "unsupported_assay_type")
})

test_that("build_signature_from_upload rejects unknown organism/platform/sample_type and unknown features", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")

  bad_org <- valid_upload("Bad Org Test")
  bad_org$metadata$organism <- "Not A Real Organism"
  result <- build_signature_from_upload(auth, bad_org, difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "invalid_upload")
  expect_match(result$message, "organism")

  bad_features <- valid_upload("Bad Features Test")
  bad_features$signature <- data.frame(probe_id = "999999", feature_id = 999999, score = 1, group_label = "All Features", stringsAsFactors = FALSE)
  result2 <- build_signature_from_upload(auth, bad_features, difexp_dir = tempdir())
  expect_false(result2$ok)
  expect_equal(result2$reason, "unknown_features")
  expect_match(result2$message, "999999")
})

test_that("build_signature_from_upload writes a real signature end-to-end and computes up/down counts", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  query_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    DBI::dbGetQuery(conn, stmt)
  }

  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'create_sig_test_feature_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'create_sig_test_feature_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  upload <- valid_upload("Upload Roundtrip Test")
  upload$difexp <- data.frame(probe_id = c("1020", "1023"), score = c(2.5, -1.2), stringsAsFactors = FALSE)

  difexp_dir <- local_tempdir()
  result <- build_signature_from_upload(auth, upload, visibility = TRUE, difexp_dir = difexp_dir)
  expect_true(result$ok)
  expect_equal(result$signature_name, "Upload Roundtrip Test")

  on.exit({
    row <- query_sql(sprintf("SELECT signature_id FROM signatures WHERE signature_hashkey = '%s'", result$signature_hashkey))
    if (nrow(row) > 0) {
      sid <- row$signature_id[1]
      exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", sid))
      exec_sql(sprintf("DELETE FROM signature_access WHERE signature_id = %d", sid))
      exec_sql(sprintf("DELETE FROM signatures WHERE signature_id = %d", sid))
    }
  }, add = TRUE)

  sig_row <- query_sql(sprintf("SELECT * FROM signatures WHERE signature_hashkey = '%s'", result$signature_hashkey))
  expect_equal(nrow(sig_row), 1)
  expect_equal(sig_row$user_name[1], "ci_admin")
  expect_equal(as.integer(sig_row$num_up_regulated[1]), 1)
  expect_equal(as.integer(sig_row$num_down_regulated[1]), 1)
  expect_equal(as.integer(sig_row$has_difexp[1]), 1)

  access_row <- query_sql(sprintf("SELECT * FROM signature_access WHERE signature_id = %d", sig_row$signature_id[1]))
  expect_equal(nrow(access_row), 1)
  expect_equal(access_row$access_type[1], "owner")

  feature_rows <- query_sql(sprintf("SELECT * FROM signature_feature_set WHERE signature_id = %d", sig_row$signature_id[1]))
  expect_equal(nrow(feature_rows), 2)

  loaded_difexp <- load_difexp_rds(difexp_dir, result$signature_hashkey)
  expect_equal(nrow(loaded_difexp), 2)

  # Duplicate name+user is rejected.
  dup <- build_signature_from_upload(auth, valid_upload("Upload Roundtrip Test"), difexp_dir = tempdir())
  expect_false(dup$ok)
  expect_equal(dup$reason, "duplicate")
})

test_that("build_signature_from_upload rolls back the signature row if feature insertion fails", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  query_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    DBI::dbGetQuery(conn, stmt)
  }

  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'create_sig_test_feature_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'create_sig_test_feature_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  # Two rows with the same (group_label, probe_id) violate
  # signature_feature_set's primary key, forcing the insert to fail after
  # the signatures row already exists.
  upload <- valid_upload(
    "Rollback Test",
    extra_features = data.frame(probe_id = "1020", feature_id = 1, score = 3.0, group_label = "All Features", stringsAsFactors = FALSE)
  )

  result <- build_signature_from_upload(auth, upload, difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "write_failed")

  leftover <- query_sql("SELECT * FROM signatures WHERE signature_name = 'Rollback Test'")
  expect_equal(nrow(leftover), 0)
})
