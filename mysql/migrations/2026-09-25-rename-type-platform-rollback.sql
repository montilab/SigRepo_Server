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
-- and duration is independent of row count. Indexes follow the renamed column
-- automatically, so the UNIQUE constraint on platforms needs no separate
-- statement. No foreign key references either column; they reference
-- platform_id.
--
-- Each rename is guarded so re-running this file is a no-op.

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
