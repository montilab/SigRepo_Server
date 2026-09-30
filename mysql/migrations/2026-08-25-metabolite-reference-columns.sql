-- Bring metabolite_reference to the shape the client and the MCP server read.
--
-- Schema change of 2026-08-25 (commit 0707ebb3), which shipped without a
-- migration. A database built from the file as first committed has a
-- `chemical_name` column nothing reads, and lacks `refmet_id` and `hmdb_id`,
-- which createOmicSignature() and the metabolomics feature search select. On
-- such a database every metabolomics signature fails with
-- "Unknown column 'refmet_id' in 'field list'".
--
-- `chemical_name` is dropped. It is a reference column no code has ever read
-- or written, and keeping it would leave a migrated database different from a
-- fresh one. It is dropped only while it holds nothing: a value someone put
-- there by hand is still a value, so if any row has one this migration stops
-- and says so instead of losing it.
--
-- Idempotent: each column and index is added only if absent, and
-- `chemical_name` is dropped only if present and empty.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND COLUMN_NAME = 'refmet_id');
SET @sql := IF(@has = 0,
  'ALTER TABLE `metabolite_reference` ADD COLUMN `refmet_id` VARCHAR(255) NULL AFTER `metabolite_id`',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND COLUMN_NAME = 'hmdb_id');
SET @sql := IF(@has = 0,
  'ALTER TABLE `metabolite_reference` ADD COLUMN `hmdb_id` VARCHAR(255) NULL AFTER `refmet_name`',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND INDEX_NAME = 'idx_refmet_id');
SET @sql := IF(@has = 0,
  'CREATE INDEX `idx_refmet_id` ON `metabolite_reference` (`refmet_id`)',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND INDEX_NAME = 'idx_hmdb_id');
SET @sql := IF(@has = 0,
  'CREATE INDEX `idx_hmdb_id` ON `metabolite_reference` (`hmdb_id`)',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- smiles is TEXT, so the index needs an explicit prefix length.
SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND INDEX_NAME = 'idx_smiles');
SET @sql := IF(@has = 0,
  'CREATE INDEX `idx_smiles` ON `metabolite_reference` (`smiles`(255))',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- Dropping the column drops idx_chemical_name with it: MySQL removes an index
-- when its only column goes.
SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND COLUMN_NAME = 'chemical_name');
-- Counted through a prepared statement, because a plain SELECT on the column
-- is an error on a database where it is already gone.
SET @sql := IF(@has > 0,
  'SELECT COUNT(*) INTO @held FROM `metabolite_reference` WHERE `chemical_name` IS NOT NULL AND `chemical_name` <> ''''',
  'SET @held := 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
SET @sql := IF(@has > 0,
  IF(@held > 0,
    'SELECT `metabolite_reference.chemical_name holds values; move them before this column can go` FROM `migration_blocked`',
    'ALTER TABLE `metabolite_reference` DROP COLUMN `chemical_name`'),
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @added := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference'
    AND COLUMN_NAME IN ('refmet_id', 'hmdb_id'));
SET @left := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'metabolite_reference' AND COLUMN_NAME = 'chemical_name');
SET @postcondition_sql := IF(@added = 2 AND @left = 0, 'DO 0',
  'SELECT `metabolite_reference lacks refmet_id or hmdb_id` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
