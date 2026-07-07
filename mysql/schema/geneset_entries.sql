--
-- Table structure for `geneset_entries`
--
CREATE TABLE `geneset_entries` (
  `geneset_entry_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `geneset_resource_id` INT UNSIGNED NOT NULL,
  `geneset_name` VARCHAR(255) NOT NULL,
  `description` TEXT DEFAULT NULL,
  `n_features` INT UNSIGNED DEFAULT NULL,
  `geneset_entry_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`geneset_entry_id`),
  UNIQUE (`geneset_resource_id`, `geneset_name`),
  UNIQUE (`geneset_entry_hashkey`),
  FOREIGN KEY (`geneset_resource_id`) REFERENCES `geneset_resources` (`geneset_resource_id`) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
