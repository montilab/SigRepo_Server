--
-- Table structure for `signature_access`
--
CREATE TABLE `signature_access` (
  `signature_id` INT UNSIGNED NOT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `access_type` SET("owner", "editor", "viewer") NOT NULL,
  `access_sig_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`, `user_name`),
  UNIQUE (`signature_id`, `user_name`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`),
  FOREIGN KEY (`user_name`) REFERENCES `users` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
