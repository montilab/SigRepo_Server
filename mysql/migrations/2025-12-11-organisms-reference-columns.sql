-- Add the BioMart and proteomics reference columns to `organisms`.
--
-- Schema change of 2025-12-11 (commits 5a22eacb, abd4611d, 12ae811c), which
-- shipped without a migration. A database built before it has only
-- organism_id and organism. Four of the columns were briefly INT or DATETIME
-- before settling on VARCHAR(255); a database caught in between is converted.
--
-- Idempotent: each column is added only if absent and converted only if it is
-- not already a VARCHAR.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_db');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_db'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `biomart_db` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `biomart_db` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_dataset');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_dataset'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `biomart_dataset` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `biomart_dataset` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_description');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_description'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `biomart_description` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `biomart_description` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_version');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_version'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `biomart_version` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `biomart_version` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_updated_date');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'biomart_updated_date'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `biomart_updated_date` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `biomart_updated_date` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'prot_organism_code');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'prot_organism_code'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `prot_organism_code` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `prot_organism_code` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'prot_organism_taxid');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'prot_organism_taxid'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `prot_organism_taxid` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `prot_organism_taxid` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'prot_updated_date');
SET @is_varchar := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_NAME = 'prot_updated_date'
    AND COLUMN_TYPE = 'varchar(255)');
SET @sql := IF(@has = 0,
  'ALTER TABLE `organisms` ADD COLUMN `prot_updated_date` VARCHAR(255) DEFAULT NULL',
  IF(@is_varchar = 0,
    'ALTER TABLE `organisms` MODIFY COLUMN `prot_updated_date` VARCHAR(255) DEFAULT NULL',
    'DO 0'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @done := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'organisms' AND COLUMN_TYPE = 'varchar(255)'
    AND COLUMN_NAME IN ('biomart_db', 'biomart_dataset', 'biomart_description', 'biomart_version',
                        'biomart_updated_date', 'prot_organism_code', 'prot_organism_taxid', 'prot_updated_date'));
SET @postcondition_sql := IF(@done = 8, 'DO 0',
  'SELECT `organisms is missing a BioMart or proteomics column` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
