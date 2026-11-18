-- Create metabolite_reference, metabolite_xref and
-- signature_feature_set_ambiguity on a database that predates them.
--
-- The files joined the repository on 2026-07-06 (commit ea7c3522).
-- metabolite_reference comes first: the other two have foreign keys to it.
-- The definitions are the ones in mysql/schema/ on 2026-09-30, so a database
-- that gets metabolite_reference here already has the columns the
-- 2026-08-25 migration adds.
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


CREATE TABLE IF NOT EXISTS metabolite_reference (
  metabolite_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  refmet_id VARCHAR(255) NULL,
  refmet_name VARCHAR(255) NULL,
  hmdb_id VARCHAR(255) NULL,
  smiles TEXT NULL,
  inchikey VARCHAR(64) NULL,
  is_current BOOL NOT NULL DEFAULT 1,
  version INT NOT NULL,
  metabolite_hashkey VARCHAR(32) NOT NULL,
  PRIMARY KEY (metabolite_id),
  UNIQUE KEY uq_metabolite_hash (metabolite_hashkey),
  KEY idx_refmet_id (refmet_id),
  KEY idx_refmet_name (refmet_name),
  KEY idx_hmdb_id (hmdb_id),
  KEY idx_smiles (smiles(255)),
  KEY idx_inchikey (inchikey),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;




CREATE TABLE IF NOT EXISTS metabolite_xref (
  xref_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  metabolite_id INT UNSIGNED NOT NULL,
  source_db VARCHAR(32) NOT NULL,
  source_value VARCHAR(255) NOT NULL,
  is_primary BOOL NOT NULL DEFAULT 0,
  xref_hashkey VARCHAR(32) NOT NULL,
  PRIMARY KEY (xref_id),
  UNIQUE KEY uq_xref_hash (xref_hashkey),
  UNIQUE KEY uq_source_value_metabolite (source_db, source_value, metabolite_id),
  KEY idx_source_lookup (source_db, source_value),
  KEY idx_metabolite_id (metabolite_id),
  CONSTRAINT fk_metabolite_xref_metabolite
    FOREIGN KEY (metabolite_id) REFERENCES metabolite_reference(metabolite_id)
    ON DELETE CASCADE,
  CHECK (is_primary IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;




CREATE TABLE IF NOT EXISTS signature_feature_set_ambiguity (
  ambiguity_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  sig_feature_hashkey VARCHAR(32) NOT NULL,
  candidate_metabolite_id INT UNSIGNED NOT NULL,
  PRIMARY KEY (ambiguity_id),
  KEY idx_sig_feature_hashkey (sig_feature_hashkey),
  KEY idx_candidate_metabolite_id (candidate_metabolite_id),
  CONSTRAINT fk_ambiguity_metabolite
    FOREIGN KEY (candidate_metabolite_id) REFERENCES metabolite_reference(metabolite_id)
    ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
