--
-- Table structure for `organisms`
--
CREATE TABLE `organisms` (
  `organism_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`organism_id`), 
  UNIQUE (`organism`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
