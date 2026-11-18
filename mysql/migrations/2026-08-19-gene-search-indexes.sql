-- Add the indexes that make "which signatures contain this gene?" answerable.
--
-- Schema change of 2026-08-19 (commit fab22a06). This replaces
-- scripts/migrate_gene_search_indexes.R, which did the same from R and
-- recorded nothing. Additive only. gene_symbol is TEXT, which MySQL cannot
-- index without a prefix length; 64 characters covers every real symbol.
--
-- The build takes a metadata lock on each table for its duration, about two
-- seconds at production scale.
--
-- Idempotent: each index is created only if no index of that name exists.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'proteomics_features' AND INDEX_NAME = 'pf_gene_symbol');
SET @sql := IF(@has = 0,
  'CREATE INDEX `pf_gene_symbol` ON `proteomics_features` (`gene_symbol`(64))',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'transcriptomics_features' AND INDEX_NAME = 'tf_gene_symbol');
SET @sql := IF(@has = 0,
  'CREATE INDEX `tf_gene_symbol` ON `transcriptomics_features` (`gene_symbol`(64))',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has := (SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signature_feature_set' AND INDEX_NAME = 'sfs_feature_id');
SET @sql := IF(@has = 0,
  'CREATE INDEX `sfs_feature_id` ON `signature_feature_set` (`feature_id`)',
  'DO 0');
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @done := (SELECT COUNT(DISTINCT INDEX_NAME) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE()
    AND INDEX_NAME IN ('pf_gene_symbol', 'tf_gene_symbol', 'sfs_feature_id'));
SET @postcondition_sql := IF(@done = 3, 'DO 0',
  'SELECT `a gene search index is missing` FROM `migration_postcondition_failed`');
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
