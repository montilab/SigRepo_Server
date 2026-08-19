#!/usr/bin/env Rscript
# Migration: add the indexes that make "which signatures contain this gene?"
# answerable, on a database created before mysql/schema/ carried them.
#
# Additive only -- three CREATE INDEX statements, no ALTER of any column, no
# DROP, no data touched. Safe to re-run: each index is skipped if it already
# exists, so running this twice (or against an already-migrated database) is
# harmless.
#
# Why: every index on signature_feature_set leads with signature_id, which
# answers "what is in this signature?" but gives no way in from the feature
# side. And gene_symbol is TEXT, which MySQL cannot index without a prefix
# length. Together that forced the gene-to-signature query to walk every
# signature's features and filter on the gene last -- a full pass over
# signature_feature_set.
#
# Measured on a 1.35M-row copy matching production scale:
#   query   1.21s -> 0.11s
#   build   1.81s (signature_feature_set) + 0.20s (feature tables)
#
# The build takes a metadata lock on each table for its duration. Two seconds
# at production scale, but run it in a quiet window rather than mid-upload.
#
# Usage:
#   DB_NAME=sigrepo DB_HOST=<host> DB_PORT=3306 \
#   DB_USER=<user> DB_PASSWORD=<password> \
#   Rscript scripts/migrate_gene_search_indexes.R

library(DBI)
library(RMySQL)

conn <- dbConnect(
  RMySQL::MySQL(),
  dbname   = Sys.getenv("DB_NAME"),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT", "3306")),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)
on.exit(dbDisconnect(conn), add = TRUE)

table_exists <- function(table_name) {
  table_name %in% DBI::dbListTables(conn)
}

index_exists <- function(table_name, index_name) {
  n <- DBI::dbGetQuery(conn, sprintf(
    "SELECT COUNT(*) AS n FROM information_schema.STATISTICS
      WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = '%s' AND INDEX_NAME = '%s'",
    table_name, index_name
  ))
  isTRUE(as.integer(n$n[1]) > 0)
}

add_index <- function(table_name, index_name, definition) {
  if (!table_exists(table_name)) {
    cat(sprintf("  skip   %-26s table not present\n", table_name))
    return(invisible(FALSE))
  }
  if (index_exists(table_name, index_name)) {
    cat(sprintf("  skip   %-26s %s already exists\n", table_name, index_name))
    return(invisible(FALSE))
  }
  started <- Sys.time()
  DBI::dbExecute(conn, sprintf("CREATE INDEX `%s` ON `%s` (%s)", index_name, table_name, definition))
  cat(sprintf("  added  %-26s %s (%.2fs)\n", table_name, index_name,
              as.numeric(difftime(Sys.time(), started, units = "secs"))))
  invisible(TRUE)
}

cat("Adding gene-content search indexes\n")

# The way in from the feature side.
add_index("signature_feature_set", "sfs_feature_id", "`feature_id`")

# gene_symbol is TEXT -> needs a prefix length. Both feature tables that carry
# one get the same treatment so the lookup behaves the same across assay types.
add_index("transcriptomics_features", "tf_gene_symbol", "`gene_symbol`(64)")
add_index("proteomics_features", "pf_gene_symbol", "`gene_symbol`(64)")

cat("Done.\n")
