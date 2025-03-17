
# For DB connection
library(RMySQL)
library(DBI)

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Show all tables in DB
table_results <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
table_results

###################
#
# DROP TABLES
#
##################

purrr::walk(
  base::seq_len(nrow(table_results)),
  function(t){
    #t=1;
    table_name <- table_results$Tables_in_sigrepo[t]
    drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))
  }
)

############# 
#
# SIGNATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "signatures"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `signature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `signature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `direction_type` SET("uni-directional", "bi-directional", "categorical") NOT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites") NOT NULL,
  `phenotype_id` INT UNSIGNED NOT NULL,
  `platform_id` VARCHAR(255) NOT NULL,
  `sample_type_id` INT UNSIGNED NOT NULL,
  `covariates` TEXT DEFAULT NULL,
  `description` TEXT DEFAULT NULL,
  `score_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `logfc_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `p_value_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `adj_p_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `cutoff_description` TEXT DEFAULT NULL,
  `keywords` TEXT DEFAULT NULL,
  `PMID` INT DEFAULT NULL,
  `year` INT DEFAULT NULL,
  `others` TEXT DEFAULT NULL,
  `has_difexp` BOOL DEFAULT 0,
  `num_of_difexp` INT DEFAULT NULL,
  `num_up_regulated` INT DEFAULT NULL,
  `num_down_regulated` INT DEFAULT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  `signature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`),
  UNIQUE (`signature_name`, `user_name`),
  FOREIGN KEY (`organism_id`) REFERENCES organisms (`organism_id`),
  FOREIGN KEY (`phenotype_id`) REFERENCES phenotypes (`phenotype_id`),
  FOREIGN KEY (`platform_id`) REFERENCES platforms (`platform_id`),
  FOREIGN KEY (`sample_type_id`) REFERENCES sample_types (`sample_type_id`),
  FOREIGN KEY (`user_name`) REFERENCES users (`user_name`),
  CHECK (`has_difexp` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
# SIGNATURE FEATURE SET ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "signature_feature_set"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `signature_id` INT UNSIGNED NOT NULL,
  `feature_id` INT UNSIGNED NOT NULL,
  `probe_id` VARCHAR(255) DEFAULT NULL,
  `score` NUMERIC(10, 8) DEFAULT NULL,
  `direction` SET("+", "-") NOT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites") NOT NULL,
  `sig_feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`, `feature_id`),
  UNIQUE (`signature_id`, `feature_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  ACCESS SIGNATURE ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "signature_access"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `signature_id` INT UNSIGNED NOT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `access_type` SET("owner", "editor", "viewer") NOT NULL,
  `access_sig_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`, `user_name`),
  UNIQUE (`signature_id`, `user_name`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`),
  FOREIGN KEY (`user_name`) REFERENCES `users` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
# COLLECTION  ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "collection"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `collection_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `collection_name` VARCHAR(255) NOT NULL,
  `description` TEXT DEFAULT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  `collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`),
  UNIQUE (`collection_name`, `user_name`),
  FOREIGN KEY (`user_name`) REFERENCES `users` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
# ACCESS COLLECTION ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "collection_access"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `collection_id` INT UNSIGNED NOT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  `access_type` SET("owner", "editor", "viewer") NOT NULL,
  `access_collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`, `user_name`),
  UNIQUE (`collection_id`, `user_name`),
  FOREIGN KEY (`collection_id`) REFERENCES `collection` (`collection_id`),
  FOREIGN KEY (`user_name`) REFERENCES `users` (`user_name`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
# ACCESS SIGNATURE COLLECTION ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "signature_collection_access"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `collection_id` INT UNSIGNED NOT NULL,
  `signature_id` INT UNSIGNED NOT NULL,
  `signature_collection_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`collection_id`, `signature_id`),
  UNIQUE (`collection_id`, `signature_id`),
  FOREIGN KEY (`collection_id`) REFERENCES `collection` (`collection_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  TRANSCRIPTOMICS FEATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "transcriptomics_features"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` TEXT DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`feature_id`), 
  UNIQUE (`feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  PROTEOMICS FEATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "proteomics_features"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` TEXT DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`feature_id`), 
  UNIQUE (`feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  METABOLOMICS FEATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "metabolomics_features"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

############# 
#
#  METHYLOMICS FEATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "methylomics_features"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

############# 
#
#  GENETIC VARIATIONS FEATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "genetic_variations_features"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

############# 
#
#  DNA BINDING SITES FEATURES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

# Table name
table_name <- "DNA_binding_sites_features"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

############# 
#
#  ORGANISMS ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

table_name <- "organisms"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `organism_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`organism_id`), 
  UNIQUE (`organism`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  PLATFORMS ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

table_name <- "platforms"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `platform_id` VARCHAR(255) NOT NULL,
  `platform_name` TEXT NOT NULL,
  `seq_technology` TEXT DEFAULT NULL,
  `organisms` TEXT DEFAULT NULL,
  PRIMARY KEY (`platform_id`),
  UNIQUE (`platform_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  PHENOTYPES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

table_name <- "phenotypes"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `phenotype_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `phenotype` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`phenotype_id`), 
  UNIQUE (`phenotype`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  SAMPLE TYPES ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

table_name <- "sample_types"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `sample_type_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `sample_type` VARCHAR(255) NOT NULL,
  `brenda_accession` VARCHAR(255) DEFAULT NULL,
  PRIMARY KEY (`sample_type_id`), 
  UNIQUE (`sample_type`)
) ENGINE=InnoDB  DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  KEYWORDS ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

table_name <- "keywords"

drop_table_sql <- base::sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `keyword_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `keyword` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`keyword_id`),
  UNIQUE (`keyword`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

############# 
#
#  USERS ####
#
############# 

# Set foreign key checks to false when dropping tables
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))

table_name <- "users"

drop_table_sql <- sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))

# Create table
create_table_sql <- base::sprintf(
'
CREATE TABLE `%s` (
  `user_name` VARCHAR(255) NOT NULL,
  `user_password_hashkey` VARCHAR(255) NOT NULL,            
  `user_email` VARCHAR(255) NOT NULL,
  `user_first` VARCHAR(255) DEFAULT NULL,
  `user_last` VARCHAR(255) DEFAULT NULL,
  `user_affiliation` TEXT DEFAULT NULL,
  `user_role` SET("admin", "editor", "viewer") NOT NULL,
  `api_key` VARCHAR(32) NOT NULL,
  `user_hashkey` VARCHAR(32) NOT NULL,                 
  PRIMARY KEY (`user_name`),
  UNIQUE (`user_name`),
  CHECK (`user_email` REGEXP "^[a-zA-Z0-9][+a-zA-Z0-9._-]*@[a-zA-Z0-9][a-zA-Z0-9._-]*[a-zA-Z0-9]*\\.[a-zA-Z]{2,4}$")
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = create_table_sql))

# Check the imported values
statement <- "select * FROM users"
user_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Show all created tables
all_table_results <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
all_table_results

# Disconnect from database ####
base::suppressWarnings(DBI::dbDisconnect(conn))    













