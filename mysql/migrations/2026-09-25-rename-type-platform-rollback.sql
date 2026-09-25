-- Rollback for 2026-09-25-rename-type-platform.sql
--
-- Rename signatures.type -> signatures.direction_type
-- Rename platforms.platform -> platforms.platform_name
--
-- Requires MySQL 8.0 or later for RENAME COLUMN.
--
-- RENAME COLUMN is used deliberately instead of CHANGE COLUMN, for the same
-- reason as the forward migration: signatures.type (formerly direction_type)
-- is a SET(...) column, and CHANGE COLUMN would require restating that
-- definition, which is exactly how the repo schema file and the deployed
-- schema have drifted and truncated data before (see the comment in
-- mysql/schema/signatures.sql). RENAME COLUMN needs no type restatement.
--
-- This is a metadata-only, in-place operation: no table rebuild, no row copy,
-- and duration is independent of row count. When a column that an index
-- covers is renamed, the column reference inside that index follows
-- automatically; the UNIQUE constraint on platforms keeps guarding the right
-- column with no separate ALTER for that. But the index's own NAME does not
-- follow the column rename, so this script also renames the index itself
-- back to `platform_name`, undoing the forward migration's `RENAME INDEX`.
-- signatures has no index at all on direction_type/type (confirmed via SHOW
-- INDEX), so it needs no equivalent statement. No foreign key references
-- either renamed column; they reference platform_id.
--
-- Each rename is guarded so re-running this file is a no-op.
--
-- Both guards below filter on TABLE_SCHEMA = DATABASE(). If the connecting
-- session has no default database selected, DATABASE() is NULL, every guard
-- matches zero rows regardless of the table's real state, and the script
-- would exit successfully having renamed nothing. That is the worst outcome
-- available here: a clean-looking run that changed nothing, followed by an
-- operator believing the rollback took effect when it did not. The
-- precondition check immediately below exists to fail loudly in that case
-- instead. SIGNAL is only valid inside a stored program, so a plain script
-- uses the usual workaround: reference a table name that cannot resolve, so
-- the client aborts with that name shown as the error.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @has_type := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'signatures'
    AND COLUMN_NAME = 'type'
);
SET @sql := IF(
  @has_type > 0,
  'ALTER TABLE `signatures` RENAME COLUMN `type` TO `direction_type`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has_platform := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND COLUMN_NAME = 'platform'
);
SET @sql := IF(
  @has_platform > 0,
  'ALTER TABLE `platforms` RENAME COLUMN `platform` TO `platform_name`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- The column rename above does not rename the index. Rename the index back
-- to the name a fresh install of the pre-rename schema would produce.
-- RENAME INDEX is available in MySQL 5.7 and later, so it is safe under
-- this file's 8.0 floor.
SET @has_platform_index := (
  SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND INDEX_NAME = 'platform'
);
SET @sql := IF(
  @has_platform_index > 0,
  'ALTER TABLE `platforms` RENAME INDEX `platform` TO `platform_name`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- Postcondition: the precondition above closes one route to a silent no-op
-- (no default database), but it is not the only conceivable one. Verify the
-- actual end state directly, columns and index name both, and fail loudly if
-- any of it is missing, rather than let the script exit 0 having
-- accomplished nothing or only partly completed.
SET @has_direction_type_after := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'signatures'
    AND COLUMN_NAME = 'direction_type'
);
SET @has_platform_name_after := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND COLUMN_NAME = 'platform_name'
);
SET @has_platform_name_index_after := (
  SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND INDEX_NAME = 'platform_name'
);
SET @postcondition_sql := IF(
  @has_direction_type_after > 0 AND @has_platform_name_after > 0 AND @has_platform_name_index_after > 0,
  'DO 0',
  'SELECT `Rollback did not complete: signatures.direction_type, platforms.platform_name, or the platform_name index is still missing` FROM `migration_postcondition_failed`'
);
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
