--
-- Table structure for `users`
--
CREATE TABLE `users` (
  `user_name` VARCHAR(255) NOT NULL,
  `user_password_hashkey` VARCHAR(255) NOT NULL,            
  `user_email` VARCHAR(255) NOT NULL,
  `user_first` VARCHAR(255) DEFAULT NULL,
  `user_last` VARCHAR(255) DEFAULT NULL,
  `user_affiliation` TEXT DEFAULT NULL,
  `user_role` SET("admin", "editor", "viewer") NOT NULL,
  `api_key` VARCHAR(32) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP, 
  `active` BOOL DEFAULT 0,
  `user_hashkey` VARCHAR(32) NOT NULL,                 
  PRIMARY KEY (`user_name`),
  UNIQUE (`user_name`),
  CHECK (`user_email` REGEXP "^[a-zA-Z0-9][+a-zA-Z0-9._-]*@[a-zA-Z0-9][a-zA-Z0-9._-]*[a-zA-Z0-9]*\\.[a-zA-Z]{2,4}$"),
  CHECK (`active` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

