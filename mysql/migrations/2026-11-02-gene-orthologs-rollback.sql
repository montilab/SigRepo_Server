-- Roll back 2026-11-02-gene-orthologs.sql: drop the `gene_orthologs` table.
--
-- The table holds reference data only (babelgene ortholog pairs that
-- POST /update_orthologs recreates from the package in a minute), and
-- nothing references it by foreign key, so dropping it loses no deposited
-- data. `DROP TABLE IF EXISTS` makes a re-run a no-op.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

DROP TABLE IF EXISTS `gene_orthologs`;

SET @has_table_after := (
  SELECT COUNT(*) FROM information_schema.TABLES
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'gene_orthologs'
);
SET @postcondition_sql := IF(
  @has_table_after = 0,
  'DO 0',
  'SELECT `Rollback did not complete: gene_orthologs still exists` FROM `migration_postcondition_failed`'
);
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
