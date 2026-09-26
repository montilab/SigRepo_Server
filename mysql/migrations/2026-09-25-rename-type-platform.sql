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
-- and duration is independent of row count. When a column that an index
-- covers is renamed, the column reference inside that index follows
-- automatically; the UNIQUE constraint on platforms keeps guarding the right
-- column with no separate ALTER for that. But the index's own NAME does not
-- follow the column rename. platforms' unique index was created implicitly
-- from `UNIQUE (`platform_name`)` in mysql/schema/platforms.sql, so MySQL
-- named it `platform_name`, and renaming the column leaves that index still
-- named `platform_name` even once it covers `platform`. A database built
-- fresh from the schema file (which now says `UNIQUE (`platform`)`) auto-
-- names that same index `platform`, so a migrated database and a fresh
-- install would disagree on the index name unless it is renamed explicitly
-- below. signatures has no index at all on direction_type/type (confirmed
-- via SHOW INDEX), so it needs no equivalent statement. No foreign key
-- references either renamed column; they reference platform_id.
--
-- Each rename is guarded so re-running this file is a no-op.
--
-- Both guards below filter on TABLE_SCHEMA = DATABASE(). If the connecting
-- session has no default database selected, DATABASE() is NULL, every guard
-- matches zero rows regardless of the table's real state, and the script
-- would exit successfully having renamed nothing. That is the worst outcome
-- available here: a clean-looking run that changed nothing, followed by an
-- operator restarting the API against a database that was never migrated.
-- The precondition check immediately below exists to fail loudly in that
-- case instead. SIGNAL is only valid inside a stored program, so a plain
-- script uses the usual workaround: reference a table name that cannot
-- resolve, so the client aborts with that name shown as the error.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

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

-- The column rename above does not rename the index. Rename the index
-- itself so a migrated database matches what a fresh install from
-- mysql/schema/platforms.sql produces. RENAME INDEX is available in MySQL
-- 5.7 and later, so it is safe under this file's 8.0 floor.
SET @has_platform_name_index := (
  SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND INDEX_NAME = 'platform_name'
);
SET @sql := IF(
  @has_platform_name_index > 0,
  'ALTER TABLE `platforms` RENAME INDEX `platform_name` TO `platform`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- Postcondition: the precondition above closes one route to a silent no-op
-- (no default database), but it is not the only conceivable one. Verify the
-- actual end state directly and fail loudly if any of it is missing, rather
-- than let the script exit 0 having accomplished nothing or only partly
-- completed.
--
-- The index check here is deliberately a negative assertion (no leftover
-- `platform_name`-named index remains) rather than a positive one (a
-- `platform`-named index exists). This project has documented drift between
-- the repo schema and deployed databases on exactly this class of unique
-- index, so a database that never had the index at all -- under either name
-- -- is a real possibility. On such a database, the guarded rename block
-- above correctly no-ops (there is no `platform_name` index to rename), and
-- a positive "does `platform` exist" check would then fail every single
-- time, including on a re-run, with no way for an operator to satisfy it
-- short of hand-creating an index this migration was never meant to add.
-- The negative assertion instead only confirms the rename did what it could:
-- if a `platform_name` index existed, it is gone (renamed to `platform`); if
-- none existed, none exists now either, and that is success, not failure.
SET @has_type_after := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'signatures'
    AND COLUMN_NAME = 'type'
);
SET @has_platform_after := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND COLUMN_NAME = 'platform'
);
SET @has_platform_name_index_after := (
  SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND INDEX_NAME = 'platform_name'
);
SET @postcondition_sql := IF(
  @has_type_after > 0 AND @has_platform_after > 0 AND @has_platform_name_index_after = 0,
  'DO 0',
  'SELECT `Migration did not complete: signatures.type or platforms.platform is still missing, or an index named platform_name still remains` FROM `migration_postcondition_failed`'
);
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
