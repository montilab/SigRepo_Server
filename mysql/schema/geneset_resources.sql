--
-- Table structure for `geneset_resources`
--
CREATE TABLE `geneset_resources` (
  `geneset_resource_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `source` VARCHAR(64) NOT NULL,
  `species` VARCHAR(128) NOT NULL,
  `collection` VARCHAR(128) NOT NULL,
  `subcollection` VARCHAR(128) DEFAULT NULL,
  `version` VARCHAR(64) NOT NULL,
  `source_version` VARCHAR(64) DEFAULT NULL,
  `format` VARCHAR(32) NOT NULL DEFAULT 'rds',
  `storage_path` VARCHAR(512) NOT NULL,
  `checksum` VARCHAR(128) DEFAULT NULL,
  `n_genesets` INT UNSIGNED DEFAULT NULL,
  `n_features` INT UNSIGNED DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `notes` TEXT DEFAULT NULL,
  `created_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `updated_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP,
  `geneset_resource_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`geneset_resource_id`),
  UNIQUE (`source`, `species`, `collection`, `subcollection`, `version`),
  UNIQUE (`geneset_resource_hashkey`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
