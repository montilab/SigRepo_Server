# Shared helper for tests that need a real MySQL connection (loaded automatically
# by testthat before any test-*.R file runs). CI provisions this database via
# .github/workflows/test.yml (schema + tests/testthat/fixtures/seed.sql); locally,
# point DB_NAME/DB_LOCAL_HOST/DB_PORT/DB_USER/DB_PASSWORD at an equivalent instance
# or these tests will skip.

db_test_available <- function() {
  if (!nzchar(Sys.getenv("DB_NAME"))) {
    return(FALSE)
  }

  tryCatch({
    conn <- db_connect_local()
    DBI::dbDisconnect(conn)
    TRUE
  }, error = function(e) FALSE)
}

# Some tests below tear down every table in the configured database --
# test-database_admin.R exercises reset_db_tables() and generate_db_schema(),
# which DROP and recreate all of them. That is fine against a disposable CI
# database and destroys a real one.
#
# It has now happened twice, most recently by running this suite against a local
# development database that happened to be reachable: 102,563 reference features
# and every signature were replaced by the CI fixture, with no prompt and no
# warning. Nothing about the invocation distinguished it from a CI run.
#
# So require the database to NAME ITSELF disposable. CI uses "sigrepo_test".
db_is_disposable <- function() {
  if (identical(tolower(Sys.getenv("SIGREPO_ALLOW_DESTRUCTIVE_TESTS")), "true")) {
    return(TRUE)
  }
  grepl("test", Sys.getenv("DB_NAME"), ignore.case = TRUE)
}

skip_if_no_test_db <- function() {
  testthat::skip_if_not(db_test_available(), "no test database available (set DB_NAME/DB_LOCAL_HOST/DB_PORT/DB_USER/DB_PASSWORD)")
  testthat::skip_if_not(
    db_is_disposable(),
    sprintf(
      paste0(
        "refusing to run destructive tests against DB_NAME='%s': the name does not identify it as a test database. ",
        "These tests drop and recreate every table. Point DB_NAME at a disposable database (CI uses 'sigrepo_test'), ",
        "or set SIGREPO_ALLOW_DESTRUCTIVE_TESTS=true if you are certain."
      ),
      Sys.getenv("DB_NAME")
    )
  )
}
