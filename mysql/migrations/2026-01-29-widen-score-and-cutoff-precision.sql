-- Widen score and cutoff columns from NUMERIC(10,8) to NUMERIC(12,8).
--
-- Schema change of 2026-01-29 (commits 7bb9077e, 4fb3dc28), which shipped
-- without a migration. NUMERIC(10,8) holds two digits before the point, so a
-- score of 100 or more was rejected. Widening keeps every stored value.
--
-- Idempotent: a column already NUMERIC(12,8) is left alone.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @wide := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signatures' AND COLUMN_NAME = 'score_cutoff'
    AND COLUMN_TYPE = 'decimal(12,8)');
SET @sql := IF(@wide = 0,
  'ALTER TABLE `signatures` MODIFY COLUMN `score_cutoff` NUMERIC(12, 8) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @wide := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signatures' AND COLUMN_NAME = 'logfc_cutoff'
    AND COLUMN_TYPE = 'decimal(12,8)');
SET @sql := IF(@wide = 0,
  'ALTER TABLE `signatures` MODIFY COLUMN `logfc_cutoff` NUMERIC(12, 8) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @wide := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signatures' AND COLUMN_NAME = 'p_value_cutoff'
    AND COLUMN_TYPE = 'decimal(12,8)');
SET @sql := IF(@wide = 0,
  'ALTER TABLE `signatures` MODIFY COLUMN `p_value_cutoff` NUMERIC(12, 8) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @wide := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signatures' AND COLUMN_NAME = 'adj_p_cutoff'
    AND COLUMN_TYPE = 'decimal(12,8)');
SET @sql := IF(@wide = 0,
  'ALTER TABLE `signatures` MODIFY COLUMN `adj_p_cutoff` NUMERIC(12, 8) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @wide := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND COLUMN_NAME = 'score'
    AND COLUMN_TYPE = 'decimal(12,8)');
SET @sql := IF(@wide = 0,
  'ALTER TABLE `signature_feature_set` MODIFY COLUMN `score` NUMERIC(12, 8) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @done := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND COLUMN_TYPE = 'decimal(12,8)'
    AND ((TABLE_NAME = 'signatures' AND COLUMN_NAME IN ('score_cutoff', 'logfc_cutoff', 'p_value_cutoff', 'adj_p_cutoff'))
      OR (TABLE_NAME = 'signature_feature_set' AND COLUMN_NAME = 'score')));
SET @postcondition_sql := IF(@done = 5, 'DO 0',
  'SELECT `a score or cutoff column is not NUMERIC(12,8)` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
