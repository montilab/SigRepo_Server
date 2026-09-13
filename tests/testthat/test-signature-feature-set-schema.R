# Guards mysql/schema/signature_feature_set.sql against drifting from the
# production table again (#78). The repo file had grown a PRIMARY KEY on
# (signature_id, group_label, probe_id) that production never had, so a
# database built from it rejected 13 of 282 production signatures: array
# probes and SomaScan aptamers that map to several features, and signatures
# whose probe_id repeats on every row. CI builds this database from
# mysql/schema/, so these tests exercise the schema file itself.
source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

sfs_columns <- function(conn) {
  DBI::dbGetQuery(conn, "
    SELECT COLUMN_NAME, IS_NULLABLE, COLUMN_DEFAULT
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'")
}

sfs_index_columns <- function(conn, index_name) {
  rows <- DBI::dbGetQuery(conn, sprintf(
    "SELECT COLUMN_NAME FROM information_schema.STATISTICS
      WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND INDEX_NAME = '%s'
      ORDER BY SEQ_IN_INDEX",
    index_name
  ))
  as.character(rows$COLUMN_NAME)
}

test_that("signature_feature_set has production's keys", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  expect_length(sfs_index_columns(conn, "PRIMARY"), 0)
  expect_equal(
    sfs_index_columns(conn, "sig_feature_assay_probe"),
    c("signature_id", "feature_id", "assay_type", "probe_id")
  )
  expect_equal(sfs_index_columns(conn, "idx_signature_nomenclature"), c("assay_type", "nomenclature_type"))
})

test_that("signature_feature_set has production's columns and nullability", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  cols <- sfs_columns(conn)
  expect_true(all(c("nomenclature_type", "match_status") %in% cols$COLUMN_NAME))
  nullable <- stats::setNames(cols$IS_NULLABLE, cols$COLUMN_NAME)
  expect_equal(unname(nullable[c("feature_id", "probe_id", "group_label")]), c("YES", "YES", "YES"))
  expect_equal(unname(nullable[c("signature_id", "assay_type", "sig_feature_hashkey")]), c("NO", "NO", "NO"))
})

test_that("one probe can carry several features within a group", {
  skip_if_no_test_db()
  db <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(db)), add = TRUE)

  signature_id <- DBI::dbGetQuery(db, "SELECT MIN(signature_id) AS id FROM signatures")$id
  skip_if(is.na(signature_id), "no seeded signature to attach rows to")

  # Transactions need one physical connection, not the pool.
  conn <- pool::poolCheckout(db)
  on.exit(pool::poolReturn(conn), add = TRUE, after = FALSE)
  DBI::dbBegin(conn)
  on.exit(suppressWarnings(DBI::dbRollback(conn)), add = TRUE, after = FALSE)
  insert <- function(feature_id, hashkey) {
    DBI::dbExecute(conn, sprintf(
      "INSERT INTO signature_feature_set
         (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
       VALUES (%d, %d, 'shared_probe_78', 1.5, 'All Features', 'transcriptomics', '%s')",
      signature_id, feature_id, hashkey
    ))
  }

  # A multi-gene array probe: same signature, group and probe, two features.
  expect_no_error(insert(900001L, "issue78_hashkey_000000000000001"))
  expect_no_error(insert(900002L, "issue78_hashkey_000000000000002"))

  # The same feature twice under one probe is still a duplicate.
  expect_error(insert(900001L, "issue78_hashkey_000000000000003"), "Duplicate entry")
})
