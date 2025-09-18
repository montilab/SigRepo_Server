--
-- Table structure for `collection`
--
CREATE TABLE `collection` (
  `collection_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `collection_name` VARCHAR(255) NOT NULL,
  `description` TEXT DEFAULT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  `visibility` BOOL DEFAULT 0,  
  `collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`),
  UNIQUE (`collection_name`, `user_name`),
  FOREIGN KEY (`user_name`) REFERENCES `users` (`user_name`),
  CHECK (`visibility` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
