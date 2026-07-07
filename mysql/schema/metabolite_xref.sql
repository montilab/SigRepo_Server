
--
-- Table structure for `metabolite_xref`
--


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
