-- Create genetic_variants_features on a database that predates it.
--
-- The table joined the schema on 2026-02-25 (commit ff6e1bab) without a
-- migration. The definition is the one in mysql/schema/ on 2026-09-30.
--
-- Idempotent: CREATE TABLE IF NOT EXISTS.

SET @precondition_sql := IF(
  DATABASE() IS NULL,
  'SELECT `Select a database first: pass the schema name to the mysql client` FROM `migration_precondition_failed`',
  'DO 0'
);
PREPARE precondition_stmt FROM @precondition_sql;
EXECUTE precondition_stmt;
DEALLOCATE PREPARE precondition_stmt;


CREATE TABLE IF NOT EXISTS genetic_variants_features (
  feature_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  feature_name VARCHAR(255) NOT NULL,
  chromosome VARCHAR(10) DEFAULT NULL,
  position INT UNSIGNED DEFAULT NULL,
  annotation VARCHAR(50) DEFAULT NULL,
  organism_id INT UNSIGNED NOT NULL,
  is_current TINYINT(1) NOT NULL DEFAULT 1,
  version INT DEFAULT NULL,
  feature_hashkey CHAR(32) DEFAULT NULL,
  PRIMARY KEY (feature_id),
  UNIQUE KEY feature_organism_ukey (feature_name, organism_id),
  KEY organism_id_idx (organism_id),
  CONSTRAINT feature_organism_fkey
  FOREIGN KEY (organism_id)
  REFERENCES organisms (organism_id),
  CONSTRAINT genetic_variants_features_chk_1
  CHECK (is_current IN (0,1))
)
ENGINE=InnoDB
DEFAULT CHARSET=utf8mb4
COLLATE=utf8mb4_unicode_ci;
