--
-- Table structure for `organisms`
--
CREATE TABLE `organisms` (
  `organism_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism` VARCHAR(255) NOT NULL,
  `biomart_db`  VARCHAR(255) DEFAULT NULL,
  `biomart_dataset` VARCHAR(255) DEFAULT NULL,
  `biomart_description` VARCHAR(255) DEFAULT NULL,
  `biomart_version` VARCHAR(255) DEFAULT NULL,
  `biomart_updated_date` VARCHAR(255) DEFAULT NULL,
  `prot_organism_code` VARCHAR(255) DEFAULT NULL,
  `prot_organism_taxid` VARCHAR(255) DEFAULT NULL,
  `prot_updated_date` VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (`organism_id`), 
  UNIQUE (`organism`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
