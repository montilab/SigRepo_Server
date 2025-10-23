--
-- Table structure for `signature_collection_access`
--
CREATE TABLE `signature_collection_access` (
  `collection_id` INT UNSIGNED NOT NULL,
  `signature_id` INT UNSIGNED NOT NULL,
  `signature_collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`, `signature_id`),
  UNIQUE (`collection_id`, `signature_id`),
  FOREIGN KEY (`collection_id`) REFERENCES `collection` (`collection_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
