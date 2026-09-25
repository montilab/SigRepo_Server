-- Rename signatures.direction_type -> signatures.type
-- Rename platforms.platform_name  -> platforms.platform
--
-- Requires MySQL 8.0 or later for RENAME COLUMN.
--
-- RENAME COLUMN is used deliberately instead of CHANGE COLUMN.
-- signatures.direction_type is a SET(...) column, and CHANGE COLUMN would
-- require restating that definition. The repo schema file and the deployed
-- schema have drifted on exactly such a definition before, truncating data
-- (see the comment in mysql/schema/signatures.sql). RENAME COLUMN needs no
-- type restatement, so that failure mode is impossible here.
--
-- This is a metadata-only, in-place operation: no table rebuild, no row copy,
-- and duration is independent of row count. Indexes follow the renamed column
-- automatically, so the UNIQUE constraint on platforms needs no separate
-- statement. No foreign key references either column; they reference
-- platform_id.
--
-- Each rename is guarded so re-running this file is a no-op.

SET @has_direction_type := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'signatures'
    AND COLUMN_NAME = 'direction_type'
);
SET @sql := IF(
  @has_direction_type > 0,
  'ALTER TABLE `signatures` RENAME COLUMN `direction_type` TO `type`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has_platform_name := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND COLUMN_NAME = 'platform_name'
);
SET @sql := IF(
  @has_platform_name > 0,
  'ALTER TABLE `platforms` RENAME COLUMN `platform_name` TO `platform`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
