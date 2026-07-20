--
-- Table structure for `genetic_variants_features`
--

CREATE TABLE genetic_variants_features (
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
