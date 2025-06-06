
# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all()

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# Establish user connection ###
conn <- SigRepo::conn_init(conn_handler = conn_handler)

# Get data path
data_path <- base::system.file("inst/data", package = "SigRepo")

# 1. Add organisms to database ####
organism_tbl <- readr::read_csv(file.path(data_path, "organisms/organism_tbl.csv"), show_col_types = FALSE)
SigRepo::addOrganism(conn_handler = conn_handler, organism_tbl = organism_tbl)

# Check the imported values
organism_db_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler)

# 2. Add platforms to database ####
platform_tbl <- readRDS(file.path(data_path, "platforms/GEO_platforms.rds")) 
SigRepo::addPlatform(conn_handler = conn_handler, platform_tbl = platform_tbl)

# Check the imported values
platform_db_tbl <- SigRepo::searchPlatform(conn_handler = conn_handler)

# 3. Add phenotypes to database ####
phenotype_tbl <- readr::read_csv(file.path(data_path, "phenotypes/phenotypes_tbl.csv"), show_col_types = FALSE)
SigRepo::addPhenotype(conn_handler = conn_handler, phenotype_tbl = phenotype_tbl)

# Check the imported values
phenotype_db_tbl <- SigRepo::searchPhenotype(conn_handler = conn_handler)

# 4. Add sample types to database ####

# Read in the brenda dictionary
sample_type_tbl <- base::readRDS(file.path(data_path, "sample_types/BRENDA_sample_types.rds"))
SigRepo::addSampleType(conn_handler = conn_handler, sample_type_tbl = sample_type_tbl)

# Check the imported values
sample_type_db_tbl <- SigRepo::searchSampleType(conn_handler = conn_handler)

# 5. Add transcriptomics feature set ####

# Read in the human gene symbols 
human_gene_symbol_tbl <- readr::read_csv(file.path(data_path, "feature_tables/Transcriptomics_HomoSapiens.csv"), show_col_types = FALSE) 
SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = human_gene_symbol_tbl)

# Check the imported values
transcriptomics_features_db_tbl <- SigRepo::searchFeature(conn_handler = conn_handler, assay_type = "transcriptomics")

# Read in the mouse gene symbols 
mouse_gene_symbol_tbl <- readr::read_csv(file.path(data_path, "feature_tables/Transcriptomics_MusMusculus.csv"), show_col_types = FALSE) 
SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = mouse_gene_symbol_tbl)

# Check the imported values
transcriptomics_features_db_tbl <- SigRepo::searchFeature(conn_handler = conn_handler, assay_type = "transcriptomics")

# 6. Add users ####

## Create an user df
user_tbl <- readr::read_csv(file.path(data_path, "users/user_tbl.csv"), show_col_types = FALSE)
SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)

# Default settings for root ####
table <- base::data.frame(
  user_name = "test",
  user_password = "test",
  user_email = "test@bu.edu", 
  user_first = "test", 
  user_last = "test", 
  user_affiliation = "Boston University",
  user_role = "viewer",
  active = 1,
  stringsAsFactors = FALSE
)

SigRepo::addUser(conn_handler = conn_handler, user_tbl = table)

# Check the imported values
user_db_tbl <- SigRepo::searchUser(conn_handler = conn_handler)

DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user"))

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = 'test', 
  password = 'test'
)

# Update user to be inactive in the users table ####
SigRepo::updateUser(
  conn_handler = conn_handler,
  user_name = "test",
  password = NULL,
  email = NULL,
  first_name = NULL,
  last_name = NULL,
  affiliation = NULL,
  role = NULL,
  active = 1
)

user_db_tbl <- SigRepo::searchUser(conn_handler = conn_handler)

SigRepo::deleteUser(
  conn_handler = conn_handler,
  user_name = "test"
)

# Check the imported values
user_db_tbl <- SigRepo::searchUser(conn_handler = conn_handler)

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = "test", 
  password = "test"
)

# Establish user connection ###
conn <- SigRepo::conn_init(conn_handler = conn_handler)






DBI::dbGetQuery(conn = conn, statement = base::sprintf("DROP USER '%s'@'%%';", 'test'))

DBI::dbGetQuery(conn = conn, statement = base::sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", 'test', "test"))
base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("GRANT SELECT ON `sigrepo`.* TO '%s'@'%%';", "test")))
DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")

DBI::dbGetQuery(conn = conn, statement = base::sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", 'test', "test"))
DBI::dbGetQuery(conn = conn, statement = "GRANT ALL PRIVILEGES ON sigrepo.* TO 'test'@'%';")
DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")


# Update user to be inactive in the users table ####
SigRepo::updateUser(
  conn_handler = conn_handler,
  user_name = "root",
  password = NULL,
  email = NULL,
  first_name = NULL,
  last_name = NULL,
  affiliation = NULL,
  role = "admin",
  active = NULL
)

SigRepo::searchUser(conn_handler = conn_handler)

DBI::dbGetQuery(conn = conn, statement = "SHOW GRANTS FOR 'root'@'%';")

DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user"))
DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", "test"))

# 7. Add signatures ####
omic_signature_AGS_OmS <- base::readRDS(file.path(data_path, "signatures/omic_signature_AGS_OmS.RDS"))
SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature_AGS_OmS)

# Check the signatures table ####
signature_db_tbl <- SigRepo::searchSignature(conn_handler = conn_handler)


