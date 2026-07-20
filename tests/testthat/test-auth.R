source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/auth.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

assign("serializers", list(json = "json-serializer-marker"), envir = globalenv())

mock_res <- function() {
  e <- new.env()
  e$serializer <- NULL
  e$status <- NULL
  e
}

test_that("require_admin_key rejects missing/empty/wrong keys and accepts the right one", {
  old_admin_key <- Sys.getenv("ADMIN_KEY", unset = NA)
  Sys.setenv(ADMIN_KEY = "correct-admin-key")
  on.exit(if (is.na(old_admin_key)) Sys.unsetenv("ADMIN_KEY") else Sys.setenv(ADMIN_KEY = old_admin_key))

  res <- mock_res()
  out <- require_admin_key(res)
  expect_equal(res$status, 404)
  expect_match(as.character(out), "Missing required parameter")

  res <- mock_res()
  out <- require_admin_key(res, "")
  expect_equal(res$status, 404)
  expect_match(as.character(out), "cannot be empty")

  res <- mock_res()
  out <- require_admin_key(res, "wrong-key")
  expect_equal(res$status, 404)
  expect_match(as.character(out), "Invalid admin key")

  res <- mock_res()
  expect_null(require_admin_key(res, "correct-admin-key"))
})

test_that("validate_api_key resolves a seeded user by api_key against a real DB", {
  skip_if_no_test_db()
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)

  conn_handler <- SigRepo::newConnHandler(
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_LOCAL_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  assign("conn_handler", conn_handler, envir = globalenv())

  res <- mock_res()
  auth <- validate_api_key(res, "0d5f1998a2cbbd765b80fdadffc6c0c2")
  expect_equal(auth$user_name, "ci_viewer")
  expect_equal(auth$user_role, "viewer")

  res <- mock_res()
  out <- validate_api_key(res, "not-a-real-key")
  expect_equal(res$status, 404)
  expect_match(as.character(out), "Invalid api key")

  res <- mock_res()
  out <- validate_api_key(res, "")
  expect_equal(res$status, 404)
  expect_match(as.character(out), "cannot be empty")
})

test_that("require_api_key resolves a seeded user, and throws a classed condition on failure", {
  skip_if_no_test_db()
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)

  conn_handler <- SigRepo::newConnHandler(
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_LOCAL_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  assign("conn_handler", conn_handler, envir = globalenv())

  auth <- require_api_key("0d5f1998a2cbbd765b80fdadffc6c0c2")
  expect_equal(auth$user_name, "ci_viewer")
  expect_equal(auth$user_role, "viewer")

  err <- tryCatch(require_api_key("not-a-real-key"), error = function(e) e)
  expect_true(inherits(err, "sigrepo_api_key_error"))
  expect_true(inherits(err, "sigrepo_invalid_api_key"))
  expect_equal(err$status, 404)
  expect_match(conditionMessage(err), "Invalid api key")

  err <- tryCatch(require_api_key(""), error = function(e) e)
  expect_true(inherits(err, "sigrepo_empty_api_key"))
  expect_match(conditionMessage(err), "cannot be empty")

  err <- tryCatch(require_api_key(NULL), error = function(e) e)
  expect_true(inherits(err, "sigrepo_missing_api_key"))
})
