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

test_that("authenticate_user proves the password against MySQL, not a stored hash", {
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
  # A MySQL account that is deliberately NOT a SigRepo user, to prove that
  # holding database credentials is not by itself enough to log in.
  db_only <- "slice1_db_only_test"
  db_only_pw <- "DbOnlyPass456"

  # SigRepo accounts are MySQL accounts, so the fixture needs both halves: a
  # real database login (which is what the password is checked against) and a
  # users row (which supplies role and api_key).
  make_db_user <- function(u, p) {
    exec_sql(sprintf("DROP USER IF EXISTS '%s'@'%%'", u))
    exec_sql(sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s'", u, p))
    exec_sql(sprintf("GRANT SELECT ON `%s`.* TO '%s'@'%%'", Sys.getenv("DB_NAME"), u))
  }

  make_db_user(uname, pw)
  make_db_user(db_only, db_only_pw)
  exec_sql(sprintf("DELETE FROM users WHERE user_name = '%s'", uname))
  on.exit({
    exec_sql(sprintf("DELETE FROM users WHERE user_name = '%s'", uname))
    exec_sql(sprintf("DROP USER IF EXISTS '%s'@'%%'", uname))
    exec_sql(sprintf("DROP USER IF EXISTS '%s'@'%%'", db_only))
  }, add = TRUE)

  # user_password_hashkey is deliberately junk: on a repository migrated from
  # the Shiny app its contents do not correspond to anyone's real password, and
  # login must not depend on it.
  exec_sql(sprintf(
    "INSERT INTO users (user_name, user_password_hashkey, user_email, user_role, api_key, user_hashkey, active)
     VALUES ('%s','%s','%s','viewer','%s','%s',1)",
    uname, "not-a-real-hash", "slice1_test@montilab.bu.edu", api_key, "slice1_user_hashkey_0001"
  ))

  # Correct credentials -> account details + api_key
  auth <- authenticate_user(uname, pw)
  expect_false(is.null(auth))
  expect_equal(auth$user_name, uname)
  expect_equal(auth$user_role, "viewer")
  expect_equal(auth$api_key, api_key)

  # MySQL passwords are case-sensitive, unlike the old md5(tolower()) path.
  expect_null(authenticate_user(uname, tolower(pw)))

  # Wrong password / empty inputs / unknown user -> NULL
  expect_null(authenticate_user(uname, "wrong-password"))
  expect_null(authenticate_user(uname, ""))
  expect_null(authenticate_user("", pw))
  expect_null(authenticate_user("no_such_user_xyz123", pw))

  # A valid database account with no users row is not a SigRepo user. This is
  # what stops a bare MySQL login (root included) reaching the portal.
  expect_null(authenticate_user(db_only, db_only_pw))

  # Inactive account -> NULL even with the right password
  exec_sql(sprintf("UPDATE users SET active = 0 WHERE user_name = '%s'", uname))
  expect_null(authenticate_user(uname, pw))
})

test_that("verify_db_credentials does not accept a bad password via the pool", {
  skip_if_no_test_db()

  # Regression guard: verify_db_credentials() must open its own connection. If
  # it ever borrows the API's pooled connection (already authenticated as
  # DB_USER) it would report success for any password at all.
  expect_false(verify_db_credentials("no_such_user_xyz123", "anything"))
  expect_false(verify_db_credentials(Sys.getenv("DB_USER"), "definitely-the-wrong-password"))
  expect_true(verify_db_credentials(Sys.getenv("DB_USER"), Sys.getenv("DB_PASSWORD")))
})
