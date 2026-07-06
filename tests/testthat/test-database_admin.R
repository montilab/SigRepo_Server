source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/database_admin.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

# Both tests below tear down every table in the test database, which would
# break other test-*.R files that depend on tests/testthat/fixtures/seed.sql
# already being loaded (test files run alphabetically in the same session).
# Reseed after each destructive assertion so the DB is left as this test
# file found it.
reseed_test_db <- function() {
  sigrepo_server_path <- Sys.getenv("SIGREPO_SERVER_DIR", unset = testthat::test_path("../.."))
  generate_db_schema(sigrepo_server_path)

  conn <- db_connect_local()
  on.exit(DBI::dbDisconnect(conn))
  lines <- readLines(testthat::test_path("fixtures/seed.sql"))
  lines <- lines[!grepl("^\\s*--", lines)]
  seed_sql <- paste(lines, collapse = "\n")
  for (stmt in Filter(nzchar, trimws(strsplit(seed_sql, ";")[[1]]))) {
    DBI::dbGetQuery(conn, stmt)
  }
}

test_that("generate_db_schema (re)creates every expected table regardless of the configured DB name", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  sigrepo_server_path <- Sys.getenv("SIGREPO_SERVER_DIR", unset = testthat::test_path("../.."))
  generate_db_schema(sigrepo_server_path)

  conn <- db_connect_local()
  tables <- DBI::dbGetQuery(conn, "SHOW TABLES;")[[1]]
  DBI::dbDisconnect(conn)

  expected <- c(
    "collection", "collection_access", "keywords", "geneset_resources", "geneset_entries",
    "organisms", "phenotypes", "platforms", "proteomics_features", "sample_types",
    "signature_access", "signature_collection_access", "signature_feature_set", "signatures",
    "transcriptomics_features", "users", "metabolite_reference", "metabolite_xref",
    "signature_feature_set_ambiguity", "genetic_variants_features"
  )
  expect_true(all(expected %in% tables))
})

test_that("reset_db_tables drops every table it finds, independent of the DB name column", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  conn <- db_connect_local()
  DBI::dbGetQuery(conn, "SET FOREIGN_KEY_CHECKS=0;")
  DBI::dbGetQuery(conn, "CREATE TABLE IF NOT EXISTS reset_db_tables_smoke_test (id INT);")
  DBI::dbDisconnect(conn)

  reset_db_tables(conn_handler = NULL)

  conn <- db_connect_local()
  tables <- DBI::dbGetQuery(conn, "SHOW TABLES;")[[1]]
  DBI::dbDisconnect(conn)
  expect_length(tables, 0)
})
