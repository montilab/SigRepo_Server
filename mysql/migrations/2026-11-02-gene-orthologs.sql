-- Create the `gene_orthologs` reference table (SigRepo_Server #166,
-- SigRepo #280): babelgene mouse to human ortholog pairs, so that a mouse
-- UniProt, Ensembl, Entrez or symbol id can be translated to its human
-- ortholog by a join (and back) instead of a string comparison that never
-- matches across species.
--
-- Requires MySQL 8.0 or later, like every migration in this directory.
--
-- The CREATE TABLE below is kept identical to mysql/schema/gene_orthologs.sql,
-- which fresh installs use. `CREATE TABLE IF NOT EXISTS` makes a re-run a
-- no-op, so this file is idempotent without an information_schema guard
-- around the statement itself. The table is empty after this migration; load
-- it with POST /update_orthologs (admin key) once the API runs a SigRepo
-- client that has updateOrthologFeatureSet(), or run that function from R.
--
-- Precondition: a default database must be selected. With none, DATABASE()
-- is NULL, the postcondition's information_schema query matches nothing,
-- and the script would exit 0 having created a table in no schema at all
-- (MySQL refuses the CREATE with "No database selected", but the check makes
-- the reason unmistakable). SIGNAL is only valid inside a stored program,
-- so a plain script uses the usual workaround: reference a table name that
-- cannot resolve, so the client aborts with that name shown as the error.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;

CREATE TABLE IF NOT EXISTS `gene_orthologs` (
  `ortholog_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` VARCHAR(255) NOT NULL,
  `ensembl_gene_id` VARCHAR(255) DEFAULT NULL,
  `entrez_id` INT UNSIGNED DEFAULT NULL,
  `human_symbol` VARCHAR(255) NOT NULL,
  `human_ensembl_gene_id` VARCHAR(255) DEFAULT NULL,
  `human_entrez_id` INT UNSIGNED DEFAULT NULL,
  `support` VARCHAR(255) DEFAULT NULL,
  `support_n` TINYINT UNSIGNED DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `version` VARCHAR(32) NOT NULL,
  `ortholog_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`ortholog_id`),
  UNIQUE KEY `uq_ortholog_hash` (`ortholog_hashkey`),
  KEY `go_organism_symbol` (`organism_id`, `gene_symbol`),
  KEY `go_ensembl` (`ensembl_gene_id`),
  KEY `go_entrez` (`entrez_id`),
  KEY `go_human_symbol` (`human_symbol`),
  KEY `go_human_ensembl` (`human_ensembl_gene_id`),
  KEY `go_human_entrez` (`human_entrez_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

-- Postcondition: the table exists and carries the index the ortholog lookup
-- enters by. Fail loudly otherwise rather than exit 0 having changed nothing.
SET @has_table_after := (
  SELECT COUNT(*) FROM information_schema.TABLES
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'gene_orthologs'
);
SET @has_symbol_index_after := (
  SELECT COUNT(*) FROM information_schema.STATISTICS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'gene_orthologs'
    AND INDEX_NAME = 'go_organism_symbol'
);
SET @postcondition_sql := IF(
  @has_table_after > 0 AND @has_symbol_index_after > 0,
  'DO 0',
  'SELECT `Migration did not complete: gene_orthologs or its go_organism_symbol index is missing` FROM `migration_postcondition_failed`'
);
PREPARE postcondition_stmt FROM @postcondition_sql;
EXECUTE postcondition_stmt;
DEALLOCATE PREPARE postcondition_stmt;
