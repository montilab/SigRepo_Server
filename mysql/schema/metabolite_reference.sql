--
-- Table structure for `metabolite_reference`
--
-- Kept in step with the deployed repository (SHOW CREATE TABLE on the
-- production database, 2026-08-25). This file had drifted: it declared a
-- `chemical_name` column that production does not have, and omitted
-- `refmet_id` and `hmdb_id`, which production does. Both the SigRepo client
-- (createOmicSignature() selects metabolite_id, refmet_id, refmet_name,
-- hmdb_id, smiles, inchikey) and the MCP metabolomics feature search
-- (mcp/lib/queries.R) require those two columns, so any database built from
-- this file -- a fresh deployment, the local dev stack, or CI -- could not
-- load a metabolomics signature at all: it failed with
-- "Unknown column 'refmet_id' in 'field list'".

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
  -- smiles is TEXT, so the index needs an explicit prefix length.
  KEY idx_smiles (smiles(255)),
  KEY idx_inchikey (inchikey),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
