-- Create geneset_resources and geneset_entries on a database that predates
-- them.
--
-- The tables joined the schema on 2026-06-23 (commit 3c88d943). This replaces
-- scripts/migrate_geneset_schema.R, which did the same from R and recorded
-- nothing. geneset_resources comes first: geneset_entries has a foreign key
-- to it. The definitions are the ones in mysql/schema/ on 2026-09-30.
--
-- Idempotent: CREATE TABLE IF NOT EXISTS.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

CREATE TABLE IF NOT EXISTS `geneset_resources` (
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

CREATE TABLE IF NOT EXISTS `geneset_entries` (
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
