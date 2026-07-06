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

skip_if_no_test_db <- function() {
  testthat::skip_if_not(db_test_available(), "no test database available (set DB_NAME/DB_LOCAL_HOST/DB_PORT/DB_USER/DB_PASSWORD)")
}
