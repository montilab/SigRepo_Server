
--
-- Create users
--
CREATE USER 'guest'@'%' IDENTIFIED BY 'guest';
GRANT SELECT, SHOW DATABASES ON *.* TO 'guest'@'%';
--
-- Configure settings
--
SET FOREIGN_KEY_CHECKS=0;
--
-- Table structure for table `signatures`
--
DROP TABLE IF EXISTS `signatures`;
CREATE TABLE `signatures` (
  `signature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `signature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `direction_type` SET("uni-directional", "bi-directional", "multiple") NOT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites") NOT NULL,
  `phenotype_id` INT UNSIGNED DEFAULT NULL,
  `platform_id` VARCHAR(255) DEFAULT NULL,
  `sample_type_id` INT UNSIGNED DEFAULT NULL,
  `covariates` TEXT DEFAULT NULL,
  `score_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `logfc_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `p_value_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `adj_p_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `description` TEXT DEFAULT NULL,
  `keywords` TEXT DEFAULT NULL,
  `PMID` INT DEFAULT NULL,
  `year` INT DEFAULT NULL,
  `author` TEXT DEFAULT NULL,
  `others` TEXT DEFAULT NULL,
  `has_difexp` BOOL DEFAULT 0,
  `user_id` VARCHAR(255) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  `signature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`),
  UNIQUE (`signature_name`, `organism_id`, `direction_type`, `assay_type`, `user_id`),
  FOREIGN KEY (`organism_id`) REFERENCES organisms (`organism_id`),
  FOREIGN KEY (`phenotype_id`) REFERENCES phenotypes (`phenotype_id`),
  FOREIGN KEY (`platform_id`) REFERENCES platforms (`platform_id`),
  FOREIGN KEY (`sample_type_id`) REFERENCES sample_types (`sample_type_id`),
  FOREIGN KEY (`user_id`) REFERENCES users (`user_id`),
  CHECK (`has_difexp` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `signature_feature_set`
--
DROP TABLE IF EXISTS `signature_feature_set`;
CREATE TABLE `signature_feature_set` (
  `sig_feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `signature_id` INT UNSIGNED NOT NULL,
  `orig_feature_id` VARCHAR(255) DEFAULT NULL,
  `feature_id` INT UNSIGNED NOT NULL,
  `score` NUMERIC(10, 8) DEFAULT NULL,
  `direction` SET("+", "-"),
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites") NOT NULL,
  `sig_feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`sig_feature_id`),
  UNIQUE (`signature_id`, `orig_feature_id`, `feature_id`, `assay_type`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `signature_access`
--
DROP TABLE IF EXISTS `signature_access`;
CREATE TABLE `signature_access` (
  `access_signature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `signature_id` INT UNSIGNED NOT NULL,
  `user_id` VARCHAR(255) NOT NULL,
  `access_type` SET("owner", "viewer") NOT NULL,
  `access_sig_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`access_signature_id`),
  UNIQUE (`signature_id`, `user_id`, `access_type`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`),
  FOREIGN KEY (`user_id`) REFERENCES `users` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `signature_collection`
--
DROP TABLE IF EXISTS `signature_collection`;
CREATE TABLE `signature_collection` (
  `collection_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `collection_name` VARCHAR(255) NOT NULL,
  `description` TEXT DEFAULT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `user_id` VARCHAR(255) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  `sig_collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`),
  UNIQUE (`collection_name`, `organism_id`, `user_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  FOREIGN KEY (`user_id`) REFERENCES `users` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `signature_collection_access`
--
DROP TABLE IF EXISTS `signature_collection_access`;
CREATE TABLE `signature_collection_access` (
  `access_collection_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `collection_id` INT UNSIGNED NOT NULL,
  `signature_id` INT UNSIGNED NOT NULL,
  `user_id` VARCHAR(255) NOT NULL,
  `access_type` SET("admin", "owner", "viewer") NOT NULL,
  `access_collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`access_collection_id`),
  UNIQUE (`collection_id`, `signature_id`, `user_id`, `access_type`),
  FOREIGN KEY (`collection_id`) REFERENCES `signature_collection` (`collection_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`),
  FOREIGN KEY (`user_id`) REFERENCES `users` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `transcriptomics_features`
--
DROP TABLE IF EXISTS `transcriptomics_features`;
CREATE TABLE `transcriptomics_features` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` TEXT DEFAULT NULL,
  `feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`feature_id`), 
  UNIQUE (`feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `proteomics_features`
--
DROP TABLE IF EXISTS `proteomics_features`;
CREATE TABLE `proteomics_features` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` TEXT DEFAULT NULL,
  `feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`feature_id`), 
  UNIQUE (`feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `organisms`
--
DROP TABLE IF EXISTS `organisms`;
CREATE TABLE `organisms` (
  `organism_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`organism_id`), 
  UNIQUE (`organism`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `platforms`
--
DROP TABLE IF EXISTS `platforms`;
CREATE TABLE `platforms` (
  `platform_id` VARCHAR(255) NOT NULL,
  `platform_name` TEXT DEFAULT NULL,
  `seq_technology` TEXT DEFAULT NULL,
  `organisms` TEXT DEFAULT NULL,
  PRIMARY KEY (`platform_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `phenotypes`
--
DROP TABLE IF EXISTS `phenotypes`;
CREATE TABLE `phenotypes` (
  `phenotype_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `phenotype` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`phenotype_id`), 
  UNIQUE (`phenotype`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `sample_types`
--
DROP TABLE IF EXISTS `sample_types`;
CREATE TABLE `sample_types` (
  `sample_type_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `sample_type` VARCHAR(255) NOT NULL,
  `brenda_accession` VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (`sample_type_id`), 
  UNIQUE (`sample_type`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `keywords`
--
DROP TABLE IF EXISTS `keywords`;
CREATE TABLE `keywords` (
  `keyword_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `keyword` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`keyword_id`),
  UNIQUE (`keyword`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
--
-- Table structure for table `users`
--
DROP TABLE IF EXISTS `users`;
CREATE TABLE `users` (
  `user_id` VARCHAR(255) NOT NULL,
  `user_password_hashkey` VARCHAR(255) NOT NULL,            
  `user_email` VARCHAR(255) NOT NULL,
  `user_first` VARCHAR(255) DEFAULT NULL,
  `user_last` VARCHAR(255) DEFAULT NULL,
  `user_affiliation` TEXT DEFAULT NULL,
  `user_role` SET("admin", "editor", "viewer") NOT NULL,
  `api_key` VARCHAR(32) NOT NULL,
  `user_hashkey` VARCHAR(32) NOT NULL,                 
  PRIMARY KEY (`user_id`),
  UNIQUE (`user_email`),
  CHECK (`user_email` REGEXP "^[a-zA-Z0-9][+a-zA-Z0-9._-]*@[a-zA-Z0-9][a-zA-Z0-9._-]*[a-zA-Z0-9]*\\.[a-zA-Z]{2,4}$")
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;



