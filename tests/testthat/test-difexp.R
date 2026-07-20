source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)

withr_local_tempdir <- function() {
  dir <- tempfile("sigrepo-difexp-test-")
  dir.create(dir)
  dir
}

test_that("save/load/delete round-trip a difexp data.frame on disk", {
  difexp_dir <- withr_local_tempdir()
  df <- data.frame(gene = c("a", "b"), score = c(1.1, -2.2))

  expect_true(save_difexp_rds(difexp_dir, "hk1", df))
  expect_true(file.exists(difexp_file_path(difexp_dir, "hk1")))

  loaded <- load_difexp_rds(difexp_dir, "hk1")
  expect_equal(loaded, df)

  delete_difexp_rds(difexp_dir, "hk1")
  expect_false(file.exists(difexp_file_path(difexp_dir, "hk1")))
  expect_null(load_difexp_rds(difexp_dir, "hk1"))
})

test_that("save_difexp_rds rejects non-data.frame payloads", {
  difexp_dir <- withr_local_tempdir()
  expect_false(save_difexp_rds(difexp_dir, "hk2", list(not = "a data.frame")))
  expect_false(file.exists(difexp_file_path(difexp_dir, "hk2")))
})

test_that("load/delete are no-ops for a signature_hashkey with no stored file", {
  difexp_dir <- withr_local_tempdir()
  expect_null(load_difexp_rds(difexp_dir, "missing"))
  expect_invisible(delete_difexp_rds(difexp_dir, "missing"))
})
