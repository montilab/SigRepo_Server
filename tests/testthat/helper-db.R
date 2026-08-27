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

# These tests are destructive. They insert and delete rows, and
# test-database_admin.R exercises reset_db_tables() and generate_db_schema(),
# which drop and recreate EVERY table in the configured database. That is fine
# against the disposable database CI provisions -- and catastrophic against a
# real one.
#
# It has already happened once: running testthat::test_dir() with DB_NAME
# pointed at a local development repository emptied it (24 tables to 0) in a
# single run, with no prompt and no warning. Nothing about the invocation
# distinguished it from a CI run.
#
# So require the database to name itself disposable. CI uses "sigrepo_test" and
# is unaffected. Anything else has to opt in explicitly, per-invocation, via
# SIGREPO_ALLOW_DESTRUCTIVE_TESTS=true -- which is a deliberate act rather than
# something a developer can do by accident.
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
