--
-- Table structure for `sample_types`
--
CREATE TABLE `sample_types` (
  `sample_type_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `sample_type` VARCHAR(255) NOT NULL,
  `brenda_accession` VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (`sample_type_id`), 
  UNIQUE (`sample_type`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;