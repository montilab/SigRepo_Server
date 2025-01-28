
# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all()

# Load OmicSignature package
devtools::load_all("OmicSignature")

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

## Establish database connection
conn <- SigRepo::conn_init(conn_handler = conn_handler)

# Get data path
data_path <- base::system.file("inst/data", package = "SigRepo")

# 1. Add organisms to database ####
organism_tbl <- readr::read_csv(file.path(data_path, "organisms/organism_tbl.csv"), show_col_types = FALSE)
SigRepo::addOrganism(conn_handler = conn_handler, organism_tbl = organism_tbl)

# Check the imported values
statement <- "select * FROM organisms"
organism_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 2. Add platforms to database ####
platform_tbl <- readRDS(file.path(data_path, "platforms/GEO_platforms.rds")) 
SigRepo::addPlatform(conn_handler = conn_handler, platform_tbl = platform_tbl)

# Check the imported values
statement <- "select * FROM platforms"
platform_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 3. Add phenotypes to database ####
phenotype_tbl <- readr::read_csv(file.path(data_path, "phenotypes/phenotypes_tbl.csv"), show_col_types = FALSE)
SigRepo::addPhenotype(conn_handler = conn_handler, phenotype_tbl = phenotype_tbl)

# phenotypes 
statement <- "select * FROM phenotypes"
phenotype_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 4. Add sample types to database ####

# Read in the brenda dictionary
sample_type_tbl <- base::readRDS(file.path(data_path, "sample_types/BRENDA_sample_types.rds"))
SigRepo::addSampleType(conn_handler = conn_handler, sample_type_tbl = sample_type_tbl)

# sample_types 
statement <- "select * FROM sample_types"
sample_type_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 5. Add transcriptomics feature set ####

# Read in the human gene symbols 
human_gene_symbol_tbl <- readr::read_csv(file.path(data_path, "feature_tables/Transcriptomics_HomoSapiens.csv"), show_col_types = FALSE) 
SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = human_gene_symbol_tbl)

# transcriptomics_features 
statement <- "select * FROM transcriptomics_features"
transcriptomics_features_db_tbl <-  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Read in the mouse gene symbols 
mouse_gene_symbol_tbl <- readr::read_csv(file.path(data_path, "feature_tables/Transcriptomics_MusMusculus.csv"), show_col_types = FALSE) 
SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = mouse_gene_symbol_tbl)

# transcriptomics_features 
statement <- "select * FROM transcriptomics_features"
transcriptomics_features_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 6. Add users ####

## Create an user df
user_tbl <- readr::read_csv(file.path(data_path, "users/user_tbl.csv"), show_col_types = FALSE)
SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)

# Check the imported values
statement <- "select * FROM users"
user_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 7. Add signatures ####
LLFS_Transcriptomic_AGS_OmS <- base::readRDS(file.path(data_path, "signatures/LLFS_Transcriptomic_AGS_OmS.rds"))
SigRepo::addSignature(conn_handler = conn_handler, omic_signature = LLFS_Transcriptomic_AGS_OmS)

omic_signature_MDA_AhR <- readRDS(file.path(data_path, "signatures/omic_signature_MDA_AhR.RDS"))
SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature_MDA_AhR)

omic_signature_MDA_CYP <- readRDS(file.path(data_path, "signatures/omic_signature_MDA_CYP.RDS"))
SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature_MDA_CYP)

# Check the signatures table ####
statement <- "select * FROM signatures"
signature_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the signature_feature_set table ####
statement <- "select * FROM signature_feature_set"
signature_feature_set_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the access_signature table ####
statement <- "select * FROM signature_access"
access_signature_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the phenotypes table ####
statement <- "select * FROM phenotypes"
phenotype_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the keywords table ####
statement <- "select * FROM keywords"
keyword_db_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Disconnect from database ####
base::suppressMessages(DBI::dbDisconnect(conn))    


