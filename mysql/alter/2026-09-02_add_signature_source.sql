-- Add signatures.signature_source, and backfill it.
--
-- APPLY THIS TO ANY DATABASE THAT ALREADY HAS DATA. mysql/schema/signatures.sql
-- has the same column for fresh installs, but generate_db_schema() DROPS every
-- table before recreating, so it can only ever be used on a disposable
-- database. There is no migration mechanism in this repo; this file is the
-- whole mechanism for this change, which is why it must be kept in step with
-- the schema file by hand.
--
--   mysql -h <host> -u <user> -p <database> < 2026-09-02_add_signature_source.sql
--
-- Safe to run twice: the ADD COLUMN is guarded on information_schema, and the
-- backfill is idempotent.

-- ---------------------------------------------------------------------------
-- 1. Add the column, only if it is not already there.
--    MySQL 8 has no ADD COLUMN IF NOT EXISTS, so this is the standard
--    prepared-statement guard.
-- ---------------------------------------------------------------------------
SET @col_exists := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'signatures'
    AND COLUMN_NAME = 'signature_source'
);

SET @ddl := IF(@col_exists = 0,
  'ALTER TABLE signatures ADD COLUMN `signature_source` VARCHAR(64) NOT NULL DEFAULT ''curated'' AFTER `user_name`',
  'SELECT ''signature_source already present - skipping ADD COLUMN'' AS note'
);
PREPARE stmt FROM @ddl;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- ---------------------------------------------------------------------------
-- 2. Backfill.
--
--    Existing rows take the column default ('curated'), so only the pulled
--    ones need identifying. They are recognised by their provenance string --
--    and there are TWO formats in the wild: the original wrote "source=..."
--    and the corrected version writes "source: ..." in the "key: value;"
--    form SigRepo:::parseRetrievedOthers() reads back. Matching only one would
--    silently mislabel the other as curated, which is the exact claim this
--    column exists to make trustworthy.
-- ---------------------------------------------------------------------------
UPDATE signatures
SET signature_source = 'rummagene'
WHERE others LIKE '%source: rummagene%'
   OR others LIKE '%source=rummagene%';

-- ---------------------------------------------------------------------------
-- 3. Report, so the operator can see what happened.
-- ---------------------------------------------------------------------------
SELECT signature_source, COUNT(*) AS n
FROM signatures
GROUP BY signature_source
ORDER BY n DESC;
