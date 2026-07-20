--
-- Table structure for `signature_feature_set_ambiguity`
--



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
