--
-- Table structure for `platforms`
--
CREATE TABLE `platforms` (
  `platform_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `platform_name` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`platform_id`),
  UNIQUE (`platform_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;