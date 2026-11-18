-- Name the unique index on proteomics_features (feature_name, organism_id)
-- `feature_organism`.
--
-- The schema first declared it as an unnamed UNIQUE, which MySQL names after
-- its first column: `feature_name`. Since 2025-12-11 (commit 756844a6, made
-- valid SQL in 05bebc6d) it is CONSTRAINT `feature_organism`. Same columns,
-- same guarantee; only the name differs, and a migrated database should carry
-- the name a fresh one does.
--
-- Idempotent: renames only when the old name exists and the new one does not.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @old := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'proteomics_features' AND INDEX_NAME = 'feature_name');
SET @new := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'proteomics_features' AND INDEX_NAME = 'feature_organism');
SET @sql := IF(@old > 0 AND @new = 0,
  'ALTER TABLE `proteomics_features` RENAME INDEX `feature_name` TO `feature_organism`',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @new := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'proteomics_features' AND INDEX_NAME = 'feature_organism');
SET @postcondition_sql := IF(@new > 0, 'DO 0',
  'SELECT `proteomics_features has no feature_organism unique index` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
