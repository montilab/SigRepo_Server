source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/login.R"), local = FALSE)
source(testthat::test_path("../../api/lib/vocabulary.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("hash_user_password matches addUser's md5(tolower(password))", {
  testthat::skip_if_not_installed("digest")
  expect_equal(
    hash_user_password("sigrepo"),
    digest::digest("sigrepo", algo = "md5", serialize = FALSE)
  )
  # addUser lowercases before hashing, so login is case-insensitive on password
  expect_equal(hash_user_password("SIGREPO"), hash_user_password("sigrepo"))
})

test_that("list_vocabulary returns the five fields as character vectors", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  vocab <- list_vocabulary(conn)
  expect_setequal(names(vocab), c("organism", "phenotype", "sample_type", "platform", "assay_type"))
  for (field in names(vocab)) {
    expect_true(is.character(vocab[[field]]), info = field)
  }
})

test_that("authenticate_user validates password, activity, and existence", {
  skip_if_no_test_db()

  # Short-lived connection per statement -- authenticate_user() opens and
  # closes its own connection, and RMySQL corrupts handles if another
  # connection is held open across those calls.
  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }

  uname <- "slice1_login_test"
  api_key <- "slice1_api_key_0001"
  pw <- "TopSecret123"

  exec_sql(sprintf("DELETE FROM users WHERE user_name = '%s'", uname))
  on.exit(exec_sql(sprintf("DELETE FROM users WHERE user_name = '%s'", uname)), add = TRUE)

  exec_sql(sprintf(
    "INSERT INTO users (user_name, user_password_hashkey, user_email, user_role, api_key, user_hashkey, active)
     VALUES ('%s','%s','%s','viewer','%s','%s',1)",
    uname, hash_user_password(pw), "slice1_test@montilab.bu.edu", api_key, "slice1_user_hashkey_0001"
  ))

  # Correct credentials -> account details + api_key
  auth <- authenticate_user(uname, pw)
  expect_false(is.null(auth))
  expect_equal(auth$user_name, uname)
  expect_equal(auth$user_role, "viewer")
  expect_equal(auth$api_key, api_key)

  # Password check is case-insensitive (md5 of lowercased input)
  expect_false(is.null(authenticate_user(uname, "topsecret123")))

  # Wrong password / empty inputs / unknown user -> NULL
  expect_null(authenticate_user(uname, "wrong-password"))
  expect_null(authenticate_user(uname, ""))
  expect_null(authenticate_user("", pw))
  expect_null(authenticate_user("no_such_user_xyz123", pw))

  # Inactive account -> NULL even with the right password
  exec_sql(sprintf("UPDATE users SET active = 0 WHERE user_name = '%s'", uname))
  expect_null(authenticate_user(uname, pw))
})
