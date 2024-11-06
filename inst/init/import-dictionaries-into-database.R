
# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)
devtools::load_all("/home/SigRepo")

## Establish database connection
conn <- SigRepo::newConnHandler(
  driver = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# 1. Add organisms to database ####
organism_tbl <- readr::read_csv("inst/data/organisms/organism_tbl.csv")

SigRepo::addOrganism(conn=conn, organism_tbl = organism_tbl)

# Check the imported values
statement <- "select * FROM organisms"
organism_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 2. Add platforms to database ####
platform_tbl <- readRDS("/home/SigRepo/inst/data/platforms/GEO_platforms.rds") 

platform_tbl <- platform_tbl %>% 
  dplyr::transmute(
    platform_id = Accession,
    platform_name = Name,
    seq_technology = Technology,
    organisms = Organism
  )

SigRepo::addPlatform(conn=conn, platform_tbl = platform_tbl)

# Check the imported values
statement <- "select * FROM platforms"
platform_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 3. Add phenotypes to database ####

# Read in the brenda dictionary
phenotype_tbl <- readr::read_csv("inst/data/phenotypes/phenotypes_tbl.csv")

SigRepo::addPhenotype(conn=conn, phenotype_tbl = phenotype_tbl)

# phenotypes 
statement <- "select * FROM phenotypes"
phenotype_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 4. Add sample types to database ####

# Read in the brenda dictionary
sample_type_tbl <- readRDS("/home/SigRepo/inst/data/sample_types/BRENDA_sample_types.rds")
colnames(sample_type_tbl) <- c("brenda_accession", "sample_type")

SigRepo::addSampleType(conn=conn, sample_type_tbl = sample_type_tbl)

# sample_types 
statement <- "select * FROM sample_types"
sample_type_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 5. Add transcriptomics feature set ####

# Read in the human and mouse gene symbols 
human_gene_symbol_tbl <- read.csv("/home/SigRepo/inst/data/feature_tables/Transcriptomics_HomoSapiens.csv") %>% 
  dplyr::transmute(
    feature_name = feature_name,
    organism = "homo sapiens",
    gene_symbol = gene_symbol
  )

SigRepo::addRefFeatureSet(conn = conn, assay_type = "transcriptomics", feature_set = human_gene_symbol_tbl)

# transcriptomics_features 
statement <- "select * FROM transcriptomics_features"
transcriptomics_features_db_tbl <-  suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

mouse_gene_symbol_tbl <- read.csv("/home/SigRepo/inst/data/feature_tables/Transcriptomics_MusMusculus.csv") %>% 
  dplyr::transmute(
    feature_name = feature_name,
    organism = "mus musculus",
    gene_symbol = gene_symbol
  )

## Add reference feature set 
SigRepo::addRefFeatureSet(conn = conn, assay_type = "transcriptomics", feature_set = mouse_gene_symbol_tbl)

# transcriptomics_features 
statement <- "select * FROM transcriptomics_features"
transcriptomics_features_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 6. Add users ####

## Create an user df
user_tbl <- readr::read_csv("inst/data/users/user_tbl.csv")

SigRepo::addUser(conn = conn, user_tbl = user_tbl)

# Check the imported values
statement <- "select * FROM users"
user_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 7. Add signatures ####
LLFS_Transcriptomic_AGS_OmS <- readRDS("/home/SigRepo/inst/data/signatures/LLFS_Transcriptomic_AGS_OmS.rds")
SigRepo::addSignatureHandler(conn = conn, omic_signature = LLFS_Transcriptomic_AGS_OmS)

LLFS_Transcriptomic_EOA_OmS <- readRDS("/home/SigRepo/inst/data/signatures/LLFS_Transcriptomic_EOA_OmS.rds")
SigRepo::addSignatureHandler(conn = conn, omic_signature = LLFS_Transcriptomic_EOA_OmS)

LLFS_Transcriptomic_EOAU_OmS <- readRDS("/home/SigRepo/inst/data/signatures/LLFS_Transcriptomic_EOAU_OmS.rds")
SigRepo::addSignatureHandler(conn = conn, omic_signature = LLFS_Transcriptomic_EOAU_OmS)


# Check the phenotypes table ####
statement <- "select * FROM phenotypes"
phenotype_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the keywords table ####
statement <- "select * FROM keywords"
keyword_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the access_signature table ####
statement <- "select * FROM access_signature"
access_signature_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# Check the signatures table ####
statement <- "select * FROM signatures"
signature_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

