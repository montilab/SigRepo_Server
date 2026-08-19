--
-- Table structure for `proteomics_features`
--
CREATE TABLE `proteomics_features` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` TEXT DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `version` DATETIME DEFAULT CURRENT_TIMESTAMP, 
  `feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`feature_id`), 
  CONSTRAINT `feature_organism` UNIQUE (`feature_name`, `organism_id`),
  -- gene_symbol is TEXT, which MySQL cannot index without a prefix length.
  -- 64 characters covers every real symbol with room to spare, and turns the
  -- gene-to-signature lookup's final filter into an index range scan.
  KEY `pf_gene_symbol` (`gene_symbol`(64)),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
