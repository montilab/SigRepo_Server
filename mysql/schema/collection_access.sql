--
-- Table structure for `collection_access`
--
CREATE TABLE `collection_access` (
  `collection_id` INT UNSIGNED NOT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `access_type` SET("owner", "editor", "viewer") NOT NULL,
  `access_collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`, `user_name`),
  UNIQUE (`collection_id`, `user_name`),
  FOREIGN KEY (`collection_id`) REFERENCES `collection` (`collection_id`),
  FOREIGN KEY (`user_name`) REFERENCES `users` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
