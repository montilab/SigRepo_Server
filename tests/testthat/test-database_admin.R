source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/schema_migrations.R"), local = FALSE)
source(testthat::test_path("../../api/lib/database_admin.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

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
    "signature_feature_set_ambiguity", "genetic_variants_features", "schema_migrations"
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
