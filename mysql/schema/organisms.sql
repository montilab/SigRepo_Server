--
-- Table structure for `organisms`
--
CREATE TABLE `organisms` (
  `organism_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism` VARCHAR(255) NOT NULL,
  `biomart_db`  VARCHAR(255) DEFAULT NULL,
  `biomart_dataset` VARCHAR(255) DEFAULT NULL,
  `biomart_description` VARCHAR(255) DEFAULT NULL,
  `biomart_version` INT DEFAULT NULL,
  `biomart_updated_date` DATETIME DEFAULT CURRENT_TIMESTAMP,
  `prot_organism_code` VARCHAR(255) DEFAULT NULL,
  `prot_organism_taxid` INT DEFAULT NULL,
  `prot_updated_date` DATETIME DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`organism_id`), 
  UNIQUE (`organism`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
