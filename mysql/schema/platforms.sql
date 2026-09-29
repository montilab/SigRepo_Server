--
-- Table structure for `platforms`
--
CREATE TABLE `platforms` (
  `platform_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `platform` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`platform_id`),
  UNIQUE (`platform`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
