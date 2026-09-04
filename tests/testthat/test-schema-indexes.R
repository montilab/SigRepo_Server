# Guards the indexes that make gene-content search possible. They are easy to
# lose in a schema edit and nothing else fails loudly when they go -- the query
# still returns correct rows, just by scanning the whole feature table. CI
# builds this database from mysql/schema/, so asserting here also proves the
# schema files themselves carry the indexes.
source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

index_columns <- function(conn, table_name, index_name) {
  rows <- DBI::dbGetQuery(conn, sprintf(
    "SELECT COLUMN_NAME, SEQ_IN_INDEX FROM information_schema.STATISTICS
      WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = '%s' AND INDEX_NAME = '%s'
      ORDER BY SEQ_IN_INDEX",
    table_name, index_name
  ))
  as.character(rows$COLUMN_NAME)
}

test_that("signature_feature_set can be entered from the feature side", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # Every other key on this table leads with signature_id, which answers "what
  # is in this signature?" but gives no way in from the gene. Without a key
  # leading on feature_id, "which signatures contain this gene?" degrades to a
  # full pass over the table -- 1.35M rows on the production repository.
  expect_equal(index_columns(conn, "signature_feature_set", "sfs_feature_id"), "feature_id")
})

test_that("gene_symbol is indexed on every feature table that has one", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # gene_symbol is TEXT, so these are prefix indexes -- information_schema
  # reports the column name either way.
  expect_equal(index_columns(conn, "transcriptomics_features", "tf_gene_symbol"), "gene_symbol")
  expect_equal(index_columns(conn, "proteomics_features", "pf_gene_symbol"), "gene_symbol")
})

test_that("a gene-to-signature lookup can be planned through the new indexes", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  plan <- DBI::dbGetQuery(conn, "
    EXPLAIN SELECT s.signature_id, COUNT(DISTINCT tf.gene_symbol)
    FROM transcriptomics_features tf
    JOIN signature_feature_set sfs ON sfs.feature_id = tf.feature_id
    JOIN signatures s ON s.signature_id = sfs.signature_id
    WHERE tf.gene_symbol IN ('TP53','MDM2')
    GROUP BY s.signature_id")

  # Deliberately not asserting a specific join order. Which table the optimizer
  # starts from depends on table statistics: on a large repository it enters at
  # the gene index, on CI's small fixture database it enters at
  # sfs_feature_id instead, and both are correct. What must hold either way is
  # that at least one of the indexes this migration adds is actually usable for
  # this query -- if they were missing or unusable, none would appear.
  keys <- paste(as.character(plan$key), collapse = " ")
  expect_true(
    grepl("sfs_feature_id", keys, fixed = TRUE) || grepl("gene_symbol", keys, fixed = TRUE),
    info = sprintf("plan used keys: %s", keys)
  )
})
