source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/export.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

local_tempdir <- function() {
  dir <- tempfile("sigrepo-export-test-")
  dir.create(dir)
  dir
}

test_that("export_safe_filename strips characters that aren't filename-safe", {
  expect_equal(export_safe_filename("LLFS Aging Gene 2023!"), "LLFS_Aging_Gene_2023_")
  expect_equal(export_safe_filename("normal_name-2"), "normal_name-2")
})

test_that("build_signature_export returns a readRDS-able metadata/signature/difexp object", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")

  result <- build_signature_export(auth, "ci_test_signature_hashkey_0000", difexp_dir = tempdir())

  expect_true(result$ok)
  expect_equal(result$signature_name, "CI Test Signature")
  expect_equal(result$export$metadata$signature_name, "CI Test Signature")
  expect_true(is.data.frame(result$export$signature))
  expect_equal(nrow(result$export$signature), 2)
  expect_null(result$export$difexp)

  # Round-trips through an actual RDS write/read, same as what the API sends.
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(result$export, tmp)
  reloaded <- readRDS(tmp)
  expect_equal(reloaded$metadata$signature_name, "CI Test Signature")
  expect_equal(nrow(reloaded$signature), 2)
})

test_that("build_signature_export reports not_found for an unknown signature", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- build_signature_export(auth, "does-not-exist-hashkey", difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "not_found")
})

test_that("build_signature_basket_zip skips unknown signatures and zips the rest", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")

  result <- build_signature_basket_zip(
    auth,
    c("ci_test_signature_hashkey_0000", "does-not-exist-hashkey"),
    difexp_dir = tempdir()
  )

  expect_true(result$ok)
  expect_equal(result$included, "ci_test_signature_hashkey_0000")
  expect_equal(result$skipped, "does-not-exist-hashkey")
  expect_true(file.exists(result$zip_path))

  extract_dir <- local_tempdir()
  utils::unzip(result$zip_path, exdir = extract_dir)
  unlink(result$zip_path)

  extracted_files <- list.files(extract_dir)
  expect_length(extracted_files, 1)
  reloaded <- readRDS(file.path(extract_dir, extracted_files[1]))
  expect_equal(reloaded$metadata$signature_name, "CI Test Signature")
})

test_that("build_signature_basket_zip reports empty_basket / none_exported", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")

  expect_equal(build_signature_basket_zip(auth, character(0), difexp_dir = tempdir())$reason, "empty_basket")
  expect_equal(build_signature_basket_zip(auth, "does-not-exist-hashkey", difexp_dir = tempdir())$reason, "none_exported")
})
