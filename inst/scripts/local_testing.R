# local instance testing script 


# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)
load_all()

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = "sigrepo", 
  host = "127.0.0.1", 
  port = 3307, 
  user = "camv", 
  password = "sigrepo"
)



base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))

users_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from users;"))

transcriptomic_features_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from transcriptomics_features;"))

sample_types_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from sample_types;"))

platforms_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from platforms;"))

phenotypes_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from phenotypes;"))

organisms_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from organisms;"))

keywords_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "select * from keywords;"))

