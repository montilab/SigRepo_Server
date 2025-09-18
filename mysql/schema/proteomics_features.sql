--
-- Table structure for `proteomics_features`
--
CREATE TABLE `proteomics_features` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` TEXT DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `feature_hashkey` VARCHAR(32) NOT NULL,
  `version` DATETIME DEFAULT CURRENT_TIMESTAMP, 
  PRIMARY KEY (`feature_id`), 
  UNIQUE (`feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
