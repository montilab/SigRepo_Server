
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

# Read in the Proteomics Human Reference
human_proteomics_tbl <- readr::read_csv(file.path(data_path, "feature_tables/Proteomics_HomoSapiens.csv"), show_col_types = FALSE)
SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "proteomics", feature_set = human_proteomics_tbl)

# Check the imported values
transcriptomics_features_db_tbl <- SigRepo::searchFeature(conn_handler = conn_handler, assay_type = "transcriptomics")

# 6. Add users ####

## Create an user df
user_tbl <- readr::read_csv(file.path(data_path, "users/user_tbl.csv"), show_col_types = FALSE)
SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)

# Check the imported values
user_db_tbl <- SigRepo::searchUser(conn_handler = conn_handler)

# 7. Add signatures ####
omic_signature_AGS_OmS <- base::readRDS(file.path(data_path, "signatures/omic_signature_AGS_OmS.RDS"))
SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature_AGS_OmS)

# Check the signatures table ####
signature_db_tbl <- SigRepo::searchSignature(conn_handler = conn_handler)


