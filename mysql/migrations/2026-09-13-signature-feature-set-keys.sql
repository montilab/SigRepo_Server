-- Give signature_feature_set the keys and columns of the production table.
--
-- Schema change of 2026-09-13 (commit 1210f74d, issue 78), which shipped
-- without a migration. The table has had three shapes:
--
--   until 2026-07-07   PRIMARY KEY (signature_id, feature_id)
--   until 2026-09-13   PRIMARY KEY (signature_id, group_label, probe_id),
--                      probe_id and group_label NOT NULL
--   since              no primary key; UNIQUE sig_feature_assay_probe
--                      (signature_id, feature_id, assay_type, probe_id);
--                      feature_id, probe_id, group_label nullable; two new
--                      columns, nomenclature_type and match_status
--
-- Both older shapes reject real signatures: array probes and aptamers that
-- map to several features, and signatures whose probe_id repeats.
--
-- Order matters. The foreign key on signature_id needs an index that leads
-- with signature_id at every moment, so the new unique key is added before
-- the old primary key and the old unnamed unique key (which MySQL named
-- `signature_id`) are dropped. feature_id cannot become nullable while it is
-- part of a primary key, so the nullability changes come last.
--
-- The new unique key can be refused by the data: the 2026-07-07 shape allowed
-- one feature under one probe in two groups of the same signature, which the
-- production key does not. If such rows exist this migration stops before
-- changing any key and says so. See "signature_feature_set duplicates" in
-- mysql/migrations/README.md.
--
-- Idempotent: every step checks information_schema first.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

-- New columns ---------------------------------------------------------------

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND COLUMN_NAME = 'nomenclature_type');
SET @sql := IF(@has = 0,
  'ALTER TABLE `signature_feature_set` ADD COLUMN `nomenclature_type` VARCHAR(64) DEFAULT NULL AFTER `assay_type`',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND COLUMN_NAME = 'match_status');
SET @sql := IF(@has = 0,
  'ALTER TABLE `signature_feature_set` ADD COLUMN `match_status` VARCHAR(32) DEFAULT NULL AFTER `sig_feature_hashkey`',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- The production unique key, added before anything is dropped -----------------

SET @has_new := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND INDEX_NAME = 'sig_feature_assay_probe');
SET @duplicates := (SELECT COUNT(*) FROM (
  SELECT 1 FROM `signature_feature_set`
   WHERE `feature_id` IS NOT NULL AND `probe_id` IS NOT NULL
   GROUP BY `signature_id`, `feature_id`, `assay_type`, `probe_id`
  HAVING COUNT(*) > 1) AS d);
SET @sql := IF(@has_new > 0,
  'DO 0',
  IF(@duplicates > 0,
    'SELECT `signature_feature_set repeats a feature under one probe` FROM `migration_blocked`',
    'ALTER TABLE `signature_feature_set`
       ADD UNIQUE KEY `sig_feature_assay_probe` (`signature_id`, `feature_id`, `assay_type`, `probe_id`)'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND INDEX_NAME = 'idx_signature_nomenclature');
SET @sql := IF(@has = 0,
  'CREATE INDEX `idx_signature_nomenclature` ON `signature_feature_set` (`assay_type`, `nomenclature_type`)',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- The old keys ----------------------------------------------------------------

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND INDEX_NAME = 'PRIMARY');
SET @sql := IF(@has > 0,
  'ALTER TABLE `signature_feature_set` DROP PRIMARY KEY',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND INDEX_NAME = 'signature_id');
SET @sql := IF(@has > 0,
  'ALTER TABLE `signature_feature_set` DROP INDEX `signature_id`',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- Nullability -----------------------------------------------------------------

SET @strict := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND COLUMN_NAME = 'feature_id' AND IS_NULLABLE = 'NO');
SET @sql := IF(@strict > 0,
  'ALTER TABLE `signature_feature_set` MODIFY COLUMN `feature_id` INT UNSIGNED DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @strict := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND COLUMN_NAME = 'probe_id' AND IS_NULLABLE = 'NO');
SET @sql := IF(@strict > 0,
  'ALTER TABLE `signature_feature_set` MODIFY COLUMN `probe_id` VARCHAR(255) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- group_label also loses its 'All Features' default. Rows keep their value.
SET @strict := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND COLUMN_NAME = 'group_label' AND (IS_NULLABLE = 'NO' OR COLUMN_DEFAULT IS NOT NULL));
SET @sql := IF(@strict > 0,
  'ALTER TABLE `signature_feature_set` MODIFY COLUMN `group_label` VARCHAR(255) DEFAULT NULL',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- Postcondition ---------------------------------------------------------------

SET @has_new := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND INDEX_NAME = 'sig_feature_assay_probe');
SET @old_keys := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND INDEX_NAME IN ('PRIMARY', 'signature_id'));
SET @nullable := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set'
    AND COLUMN_NAME IN ('feature_id', 'probe_id', 'group_label') AND IS_NULLABLE = 'YES');
SET @postcondition_sql := IF(@has_new > 0 AND @old_keys = 0 AND @nullable = 3, 'DO 0',
  'SELECT `signature_feature_set does not have the production keys` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
