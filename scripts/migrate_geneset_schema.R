#!/usr/bin/env Rscript
# Migration: create geneset_resources + geneset_entries on a database that
# predates them (mysql/schema/geneset_resources.sql, geneset_entries.sql).
# Additive only -- CREATE TABLE, no ALTER/DROP, nothing else on the database
# is touched. Safe to re-run: skips any table that already exists instead of
# erroring, so running this twice (or against a DB that's already been
# migrated) is harmless.
#
# Usage:
#   DB_NAME=sigrepo DB_HOST=<host> DB_PORT=3306 \
#   DB_USER=<user> DB_PASSWORD=<password> \
#   Rscript scripts/migrate_geneset_schema.R

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

table_exists <- function(conn, table_name) {
  table_name %in% DBI::dbListTables(conn)
}

# geneset_resources first -- geneset_entries has a foreign key to it, so
# creating them in the other order would fail.
create_geneset_resources <- "
CREATE TABLE `geneset_resources` (
  `geneset_resource_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `source` VARCHAR(64) NOT NULL,
  `species` VARCHAR(128) NOT NULL,
  `collection` VARCHAR(128) NOT NULL,
  `subcollection` VARCHAR(128) DEFAULT NULL,
  `version` VARCHAR(64) NOT NULL,
  `source_version` VARCHAR(64) DEFAULT NULL,
  `format` VARCHAR(32) NOT NULL DEFAULT 'rds',
  `storage_path` VARCHAR(512) NOT NULL,
  `checksum` VARCHAR(128) DEFAULT NULL,
  `n_genesets` INT UNSIGNED DEFAULT NULL,
  `n_features` INT UNSIGNED DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `notes` TEXT DEFAULT NULL,
  `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `geneset_resource_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`geneset_resource_id`),
  UNIQUE (`source`, `species`, `collection`, `subcollection`, `version`),
  UNIQUE (`geneset_resource_hashkey`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
"

create_geneset_entries <- "
CREATE TABLE `geneset_entries` (
  `geneset_entry_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `geneset_resource_id` INT UNSIGNED NOT NULL,
  `geneset_name` VARCHAR(255) NOT NULL,
  `description` TEXT DEFAULT NULL,
  `n_features` INT UNSIGNED DEFAULT NULL,
  `geneset_entry_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`geneset_entry_id`),
  UNIQUE (`geneset_resource_id`, `geneset_name`),
  UNIQUE (`geneset_entry_hashkey`),
  FOREIGN KEY (`geneset_resource_id`) REFERENCES `geneset_resources` (`geneset_resource_id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
"

cat("geneset_* tables before migration:\n")
before <- DBI::dbListTables(conn)
print(before[grepl("^geneset_", before)])
cat("\n")

if (table_exists(conn, "geneset_resources")) {
  cat("[skip] geneset_resources already exists, not touching it.\n")
} else {
  dbExecute(conn, create_geneset_resources)
  cat("[created] geneset_resources\n")
}

if (table_exists(conn, "geneset_entries")) {
  cat("[skip] geneset_entries already exists, not touching it.\n")
} else {
  dbExecute(conn, create_geneset_entries)
  cat("[created] geneset_entries\n")
}

cat("\ngeneset_* tables after migration:\n")
after <- DBI::dbListTables(conn)
print(after[grepl("^geneset_", after)])

cat("\nCurrent row counts:\n")
cat("geneset_resources:", dbGetQuery(conn, "SELECT COUNT(*) AS n FROM geneset_resources")$n, "\n")
cat("geneset_entries:  ", dbGetQuery(conn, "SELECT COUNT(*) AS n FROM geneset_entries")$n, "\n")
