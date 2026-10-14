-- Make 'genetic_variants' the fifth assay_type on `signatures` and
-- `signature_feature_set`.
--
-- Schema change of 2026-08-26 (commit 37d92a64), which shipped without a
-- migration. The member list has had three shapes:
--
--   until 2025-12-11   ..., 'genetic_variations', 'dna_binding_sites'
--   until 2026-08-26   ..., 'snps'
--   since              ..., 'genetic_variants'
--
-- The first four members never changed. 'snps' and 'genetic_variations' both
-- mean what is now 'genetic_variants', so a row holding exactly one of them
-- is rewritten. Anything else outside the final list stops the migration
-- before the member list changes: 'dna_binding_sites', which has no
-- equivalent, and a value with more than one member (SET columns allow it;
-- nothing in SigRepo writes one), which is not something to guess a meaning
-- for. Stopping is what keeps MySQL from truncating such a value to an
-- empty string, and this file from quietly dropping one of its members.
--
-- mysql/migrations/README.md warns against restating a SET definition by
-- hand, because the repository and a deployed table have disagreed on one
-- before and data was truncated. This migration has to change a SET, so it
-- does it in three steps that never drop a member a row is using:
--
--   1. append 'genetic_variants' to whatever list the table has now, built
--      from information_schema, not from this file;
--   2. rewrite the rows;
--   3. count rows outside the final list, and narrow only if there are none.
--
-- Idempotent: a column already at the final definition is left alone.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @final := 'set(''transcriptomics'',''proteomics'',''metabolomics'',''methylomics'',''genetic_variants'')';

-- signatures ----------------------------------------------------------------

SET @cur := (SELECT COLUMN_TYPE FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signatures' AND COLUMN_NAME = 'assay_type');

SET @sql := IF(@cur = @final OR LOCATE('''genetic_variants''', @cur) > 0,
  'DO 0',
  CONCAT('ALTER TABLE `signatures` MODIFY COLUMN `assay_type` ',
         LEFT(@cur, CHAR_LENGTH(@cur) - 1), ',''genetic_variants'') NOT NULL'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @sql := IF(@cur = @final,
  'DO 0',
  'UPDATE `signatures` SET `assay_type` = ''genetic_variants''
    WHERE `assay_type` IN (''snps'', ''genetic_variations'')');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @stranded := (SELECT COUNT(*) FROM `signatures`
  WHERE `assay_type` NOT IN ('transcriptomics', 'proteomics', 'metabolomics', 'methylomics', 'genetic_variants'));
SET @sql := IF(@stranded > 0,
  'SELECT `signatures holds an assay_type with no current equivalent` FROM `migration_blocked`',
  IF(@cur = @final,
    'DO 0',
    'ALTER TABLE `signatures` MODIFY COLUMN `assay_type`
       SET(''transcriptomics'', ''proteomics'', ''metabolomics'', ''methylomics'', ''genetic_variants'') NOT NULL'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- signature_feature_set -----------------------------------------------------

SET @cur := (SELECT COLUMN_TYPE FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND COLUMN_NAME = 'assay_type');

SET @sql := IF(@cur = @final OR LOCATE('''genetic_variants''', @cur) > 0,
  'DO 0',
  CONCAT('ALTER TABLE `signature_feature_set` MODIFY COLUMN `assay_type` ',
         LEFT(@cur, CHAR_LENGTH(@cur) - 1), ',''genetic_variants'') NOT NULL'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @sql := IF(@cur = @final,
  'DO 0',
  'UPDATE `signature_feature_set` SET `assay_type` = ''genetic_variants''
    WHERE `assay_type` IN (''snps'', ''genetic_variations'')');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @stranded := (SELECT COUNT(*) FROM `signature_feature_set`
  WHERE `assay_type` NOT IN ('transcriptomics', 'proteomics', 'metabolomics', 'methylomics', 'genetic_variants'));
SET @sql := IF(@stranded > 0,
  'SELECT `signature_feature_set holds an assay_type with no current equivalent` FROM `migration_blocked`',
  IF(@cur = @final,
    'DO 0',
    'ALTER TABLE `signature_feature_set` MODIFY COLUMN `assay_type`
       SET(''transcriptomics'', ''proteomics'', ''metabolomics'', ''methylomics'', ''genetic_variants'') NOT NULL'));
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

-- Postcondition -------------------------------------------------------------

SET @done := (SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE() AND COLUMN_NAME = 'assay_type'
    AND TABLE_NAME IN ('signatures', 'signature_feature_set')
    AND COLUMN_TYPE = @final);
SET @postcondition_sql := IF(@done = 2, 'DO 0',
  'SELECT `assay_type does not have its final member list` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
