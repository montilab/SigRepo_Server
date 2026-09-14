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

test_that("difexp_to_json keeps full numeric precision (#76)", {
  df <- data.frame(
    probe_id = c("p1", "p2", "p3"),
    score = c(1.807553, -2.123456, 0.065250001),
    p_value = c(0.00001234, 1.5e-12, 0.5),
    stringsAsFactors = FALSE
  )

  decoded <- jsonlite::fromJSON(difexp_to_json(df))

  expect_identical(decoded$probe_id, df$probe_id)
  expect_equal(decoded$score, df$score, tolerance = 1e-14)
  expect_equal(decoded$p_value, df$p_value, tolerance = 1e-14)
  expect_true(all(decoded$p_value > 0))
})
