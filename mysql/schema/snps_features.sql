--
-- Table structure for `snps_features`
--
CREATE TABLE `snps_features` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `chromosome` INT DEFAULT NULL,
  `position` INT DEFAULT NULL,
  `annotation` VARCHAR(50) DEFAULT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `is_current` BOOL DEFAULT 1,
  `version` INT DEFAULT NULL,
  `feature_hashkey` VARCHAR(32) DEFAULT NULL,
  PRIMARY KEY (`feature_id`),
  UNIQUE (`feature_name`, `organism_id`, `chromosome`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

