--
-- Table structure for `phenotypes`
--
CREATE TABLE `phenotypes` (
  `phenotype_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `phenotype` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`phenotype_id`), 
  UNIQUE (`phenotype`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
