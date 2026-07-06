--
-- Table structure for `metabolite_reference`
--

CREATE TABLE IF NOT EXISTS metabolite_reference (
  metabolite_id INT UNSIGNED NOT NULL AUTO_INCREMENT,
  chemical_name VARCHAR(255) NULL,
  refmet_name VARCHAR(255) NULL,
  inchikey VARCHAR(64) NULL,
  smiles TEXT NULL,
  is_current BOOL NOT NULL DEFAULT 1,
  version INT NOT NULL,
  metabolite_hashkey VARCHAR(32) NOT NULL,
  PRIMARY KEY (metabolite_id),
  UNIQUE KEY uq_metabolite_hash (metabolite_hashkey),
  KEY idx_chemical_name (chemical_name),
  KEY idx_refmet_name (refmet_name),
  KEY idx_inchikey (inchikey),
  CHECK (is_current IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
