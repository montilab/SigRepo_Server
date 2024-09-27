# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Establish DB connection 
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "sigrepo",
  host = "montilab.bu.edu",
  port = 3306,
  username = "root",
  password = "root"
)

# Set foreign key checks to false when dropping tables
DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;")

# Show all tables in DB
table_results <- dbGetQuery(conn=conn, statement="show tables;")
table_results

###################
#
# DROP TABLES
#
##################

if(nrow(table_results) > 0){
  1:nrow(table_results) %>% 
    walk(
      function(t){
        #t=1;
        table_name <- table_results$Tables_in_sigrepo[t]
        # Drop table
        drop_table_sql <- sprintf('DROP TABLE IF EXISTS `%s`;', table_name)
        DBI::dbGetQuery(conn = conn, statement = drop_table_sql)
      }
    )
}

# Initiate table auto increment value
auto_increment <- 1000

############# 
#
# SIGNATURES ####
#
############# 

# Drop table
table_number <- 1
table_name <- "signatures"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `signature_id` INT NOT NULL,
  `signature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `direction_type` SET("uni-directional", "bi-directional", "multiple") NOT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites", "others") NOT NULL,
  `phenotype_id` INT DEFAULT NULL,
  `platform_id` VARCHAR(255) DEFAULT NULL,
  `sample_type_id` INT DEFAULT NULL,
  `covariates` TEXT DEFAULT NULL,
  `score_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `logfc_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `p_value_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `adj_p_cutoff` NUMERIC(10, 8) DEFAULT NULL,
  `description` TEXT DEFAULT NULL,
  `keywords` TEXT DEFAULT NULL,
  `PMID` INT DEFAULT NULL,
  `year` INT DEFAULT NULL,
  `others` TEXT DEFAULT NULL,
  `has_difexp` BOOL DEFAULT 0,
  `user_id` VARCHAR(255) NOT NULL,
  `uploaded_date` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  PRIMARY KEY (`signature_id`, `signature_name`, `organism_id`, `direction_type`, `assay_type`, `user_id`),
  FOREIGN KEY (`organism_id`) REFERENCES organisms (`organism_id`),
  FOREIGN KEY (`phenotype_id`) REFERENCES phenotypes (`phenotype_id`),
  FOREIGN KEY (`platform_id`) REFERENCES platforms (`platform_id`),
  FOREIGN KEY (`sample_type_id`) REFERENCES sample_types (`sample_type_id`),
  FOREIGN KEY (`user_id`) REFERENCES users (`user_id`),
  CHECK (`has_difexp` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
# SIGNATURE FEATURE SET ####
#
############# 

table_name <- "signature_feature_set"

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `signature_id` INT NOT NULL,
  `feature_id` INT NOT NULL,
  `orig_feature_id` TEXT DEFAULT NULL,
  `score` NUMERIC(10, 8) DEFAULT NULL,
  `direction` SET("+", "-"),
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites") NOT NULL,
  PRIMARY KEY (`signature_id`, `feature_id`, `assay_type`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  ACCESS SIGNATURE ####
#
############# 

table_name <- "signature_access"

# Create table
create_table_sql <- sprintf(
  '
CREATE TABLE `%s` (
  `signature_id` INT NOT NULL,
  `user_id` VARCHAR(255) NOT NULL,
  `access_type` SET("admin", "owner", "viewer") NOT NULL,
  PRIMARY KEY (`signature_id`, `user_id`, `access_type`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`),
  FOREIGN KEY (`user_id`) REFERENCES `users` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
# SIGNATURE COLLECTION  ####
#
############# 

table_number <- table_number + 1
table_name <- "signature_collection"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `collection_id` INT NOT NULL AUTO_INCREMENT,
  `collection_name` VARCHAR(255) NOT NULL,
  `description` TEXT DEFAULT NULL,
  `organism_id` INT NOT NULL,
  `user_id` VARCHAR(255) NOT NULL,
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  PRIMARY KEY (`collection_id`, `collection_name`, `organism_id`, `user_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  FOREIGN KEY (`user_id`) REFERENCES `users` (`user_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
# ACCESS SIGNATURE COLLECTION ####
#
############# 

table_name <- "signature_collection_access"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `collection_id` INT NOT NULL,
  `signature_id` INT NOT NULL,
  `user_id` VARCHAR(255) NOT NULL,
  `access_type` SET("admin", "owner", "viewer") NOT NULL,
  PRIMARY KEY (`collection_id`, `signature_id`, `user_id`, `access_type`),
  FOREIGN KEY (`collection_id`) REFERENCES `signature_collection` (`collection_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`),
  FOREIGN KEY (`user_id`) REFERENCES `users` (`user_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  TRANSCRIPTOMICS FEATURES ####
#
############# 

table_number <- table_number + 1
table_name <- "transcriptomics_features"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `description` TEXT DEFAULT NULL,
  `synonyms` TEXT DEFAULT NULL,
  `n_synonyms` INT DEFAULT NULL,
  `ensemble_ids` TEXT DEFAULT NULL,
  `n_ensemble_ids` INT DEFAULT NULL,
  `transcript_biotypes` TEXT DEFAULT NULL,
  `chromosome_name` TEXT DEFAULT NULL,
  `start_position` TEXT DEFAULT NULL,
  `end_position` TEXT DEFAULT NULL,
  PRIMARY KEY (`feature_id`, `feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  PROTEOMICS FEATURES ####
#
############# 

table_number <- table_number + 1
table_name <- "proteomics_features"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `description` TEXT DEFAULT NULL,
  `synonyms` TEXT DEFAULT NULL,
  `n_synonyms` INT DEFAULT NULL,
  `ensemble_ids` TEXT DEFAULT NULL,
  `n_ensemble_ids` INT DEFAULT NULL,
  `transcript_biotypes` TEXT DEFAULT NULL,
  `chromosome_name` TEXT DEFAULT NULL,
  `start_position` TEXT DEFAULT NULL,
  `end_position` TEXT DEFAULT NULL,
  PRIMARY KEY (`feature_id`, `feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  METABOLOMICS FEATURES ####
#
############# 

table_number <- table_number + 1
table_name <- "metabolomics_features"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `description` TEXT DEFAULT NULL,
  `synonyms` TEXT DEFAULT NULL,
  `n_synonyms` INT DEFAULT NULL,
  `ensemble_ids` TEXT DEFAULT NULL,
  `n_ensemble_ids` INT DEFAULT NULL,
  `transcript_biotypes` TEXT DEFAULT NULL,
  `chromosome_name` TEXT DEFAULT NULL,
  `start_position` TEXT DEFAULT NULL,
  `end_position` TEXT DEFAULT NULL,
  PRIMARY KEY (`feature_id`, `feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  METHYLOMICS FEATURES ####
#
############# 

table_number <- table_number + 1
table_name <- "methylomics_features"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `description` TEXT DEFAULT NULL,
  `synonyms` TEXT DEFAULT NULL,
  `n_synonyms` INT DEFAULT NULL,
  `ensemble_ids` TEXT DEFAULT NULL,
  `n_ensemble_ids` INT DEFAULT NULL,
  `transcript_biotypes` TEXT DEFAULT NULL,
  `chromosome_name` TEXT DEFAULT NULL,
  `start_position` TEXT DEFAULT NULL,
  `end_position` TEXT DEFAULT NULL,
  PRIMARY KEY (`feature_id`, `feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  GENETIC VARIATIONS FEATURES ####
#
############# 

table_number <- table_number + 1
table_name <- "genetic_variations_features"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `description` TEXT DEFAULT NULL,
  `synonyms` TEXT DEFAULT NULL,
  `n_synonyms` INT DEFAULT NULL,
  `ensemble_ids` TEXT DEFAULT NULL,
  `n_ensemble_ids` INT DEFAULT NULL,
  `transcript_biotypes` TEXT DEFAULT NULL,
  `chromosome_name` TEXT DEFAULT NULL,
  `start_position` TEXT DEFAULT NULL,
  `end_position` TEXT DEFAULT NULL,
  PRIMARY KEY (`feature_id`, `feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  DNA BINDING SITES FEATURES ####
#
############# 

table_number <- table_number + 1
table_name <- "DNA_binding_sites_features"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `feature_id` INT NOT NULL AUTO_INCREMENT,
  `feature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT NOT NULL,
  `description` TEXT DEFAULT NULL,
  `synonyms` TEXT DEFAULT NULL,
  `n_synonyms` INT DEFAULT NULL,
  `ensemble_ids` TEXT DEFAULT NULL,
  `n_ensemble_ids` INT DEFAULT NULL,
  `transcript_biotypes` TEXT DEFAULT NULL,
  `chromosome_name` TEXT DEFAULT NULL,
  `start_position` TEXT DEFAULT NULL,
  `end_position` TEXT DEFAULT NULL,
  PRIMARY KEY (`feature_id`, `feature_name`, `organism_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  ORGANISMS ####
#
############# 

table_number <- table_number + 1
table_name <- "organisms"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `organism_id` INT NOT NULL AUTO_INCREMENT,
  `organism` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`organism_id`, `organism`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  PLATFORMS ####
#
############# 

table_name <- "platforms"

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `platform_id` VARCHAR(255) NOT NULL,
  `platform_name` TEXT DEFAULT NULL,
  `seq_technology` TEXT DEFAULT NULL,
  `organisms` TEXT DEFAULT NULL,
  PRIMARY KEY (`platform_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  PHENOTYPES ####
#
############# 

table_number <- table_number + 1
table_name <- "phenotypes"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
  '
CREATE TABLE `%s` (
  `phenotype_id` INT NOT NULL AUTO_INCREMENT,
  `phenotype` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`phenotype_id`, `phenotype`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  SAMPLE TYPES ####
#
############# 

table_number <- table_number + 1
table_name <- "sample_types"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `sample_type_id` INT NOT NULL AUTO_INCREMENT,
  `sample_type` VARCHAR(255) NOT NULL,
  `brenda_accession` TEXT DEFAULT NULL,
  PRIMARY KEY (`sample_type_id`, `sample_type`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  KEYWORDS ####
#
############# 

table_number <- table_number + 1
table_name <- "keywords"
table_auto_increment <- auto_increment * table_number

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `keyword_id` INT NOT NULL AUTO_INCREMENT,
  `keyword` VARCHAR(255) NOT NULL,
  PRIMARY KEY (`keyword_id`, `keyword`)
) ENGINE=InnoDB AUTO_INCREMENT=%s DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name, table_auto_increment)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

############# 
#
#  USERS ####
#
############# 

table_name <- "users"

# Create table
create_table_sql <- sprintf(
'
CREATE TABLE `%s` (
  `user_id` VARCHAR(255) NOT NULL,
  `user_password_hashkey` VARCHAR(255) NOT NULL,
  `user_email` VARCHAR(255) NOT NULL,
  `user_first` VARCHAR(255) DEFAULT NULL,
  `user_last` VARCHAR(255) DEFAULT NULL,
  `user_affiliation` TEXT DEFAULT NULL,
  `user_role` SET("admin", "user") NOT NULL,
  `api_key` TEXT DEFAULT NULL,
  PRIMARY KEY (`user_id`, `user_email`, `user_role`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
', table_name)

DBI::dbGetQuery(conn = conn, statement = create_table_sql)

# Show all created tables
all_table_results <- dbGetQuery(conn=conn, statement="show tables;")
all_table_results















