
# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load OmicSignature package
devtools::load_all("/home/rstudio/OmicSignature")

# Load OmicSignature package
devtools::load_all("/home/rstudio/SigRepoR")

## Establish database connection
conn <- SigRepoR::newConnHandler(
  driver = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# 1. Add organisms to database ####
organism_tbl <- data.frame(
  organism = c(
    "Homo sapiens",
    "Mus musculus",
    "Rattus norvegicus",
    "Danio rerio",
    "Heterocephalus glaber",
    "Caenorhabditis elegans",
    "Drosophila melanogaster",
    "Arabidopsis thaliana"
  )
)

SigRepoR::addOrganism(conn=conn, organism_tbl = organism_tbl)

# Check the imported values
statement <- "select * FROM organisms"
organism_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 2. Add platforms to database ####
platform_tbl <- readRDS("/home/rstudio/SigRepoR/inst/data/platforms/GEOplatform_2024.rds") 

platform_tbl <- platform_tbl %>% 
  dplyr::transmute(
    platform_id = Accession,
    platform_name = Name,
    seq_technology = Technology,
    organisms = Organism
  )

SigRepoR::addPlatform(conn=conn, platform_tbl = platform_tbl)

# Check the imported values
statement <- "select * FROM platforms"
platform_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 3. Add phenotypes to database ####

# Read in the brenda dictionary
phenotype_tbl <- data.frame(
  phenotype = c("Aging", "Blood", "Extreme Old Age")
)

SigRepoR::addPhenotype(conn=conn, phenotype_tbl = phenotype_tbl)

# phenotypes 
statement <- "select * FROM phenotypes"
phenotype_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 4. Add sample types to database ####

# Read in the brenda dictionary
sample_type_tbl <- readRDS("/home/rstudio/SigRepoR/inst/data/sample_types/BRENDA.rds")
colnames(sample_type_tbl) <- c("brenda_accession", "sample_type")

SigRepoR::addSampleType(conn=conn, sample_type_tbl = sample_type_tbl)

# sample_types 
statement <- "select * FROM sample_types"
sample_type_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 5. Add transcriptomics feature set ####

# Read in the human and mouse gene symbols 
human_gene_symbol_tbl <- read.csv("~/SigRepoR/inst/data/gene_symbols/homo_sapiens.txt", sep="") %>% 
  dplyr::transmute(
    feature_name = hgnc_symbol,
    organism = "homo sapiens",
    description = description,
    synonyms = synonyms,
    n_synonyms = n_synonyms,
    ensemble_ids = ensemble_ids,
    n_ensemble_ids = n_ensembl_ids,
    transcript_biotypes = transcript_biotypes,
    chromosome_name = chromosome_name,
    start_position = start_position,
    end_position = end_position
  )

SigRepoR::addRefFeatureSet(conn = conn, assay_type = "transcriptomics", feature_set = human_gene_symbol_tbl)

# transcriptomics_features 
statement <- "select * FROM transcriptomics_features"
transcriptomics_features_db_tbl <-  suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

mouse_gene_symbol_tbl <- read.csv("/home/rstudio/SigRepoR/inst/data/gene_symbols/mus_musculus.txt", sep = "") %>% 
  dplyr::transmute(
    feature_name = mgi_symbol,
    organism = "mus musculus",
    description = mgi_description,
    synonyms = synonyms,
    n_synonyms = n_synonyms,
    ensemble_ids = ensemble_ids,
    n_ensemble_ids = n_ensembl_ids,
    transcript_biotypes = transcript_biotypes,
    chromosome_name = chromosome_name,
    start_position = start_position,
    end_position = end_position
  )

## Add reference feature set 
SigRepoR::addRefFeatureSet(conn = conn, assay_type = "transcriptomics", feature_set = mouse_gene_symbol_tbl)

# transcriptomics_features 
statement <- "select * FROM transcriptomics_features"
transcriptomics_features_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 6. Add users ####

## Create an user df
user_tbl <- data.frame(
  user_id = c("guest", "rchau88", "smonti", "vmli", "lkroeh", "andrewdr", "zihuang"), 
  user_password = c("guest", "password", "password", "password", "password", "password", "password"),
  user_email = c("guest@bu.edu", "rchau88@bu.edu", "smonti@bu.edu", "vmli@bu.edu", "lkroeh@bu.edu", "andrewdr@bu.edu", "zihuang@bu.edu"), 
  user_first = c("guest", "Reina", "Stefano", "Vanessa", "Lina", "Andrew", "Ziwei"), 
  user_last = c("guest", "Chau", "Monti", "Li", "Kroehling", "Chen", "Huang"), 
  user_affiliation = "Boston University",
  user_role = c("guest", "admin", "admin", "admin", "admin", "admin", "admin"),
  stringsAsFactors = FALSE
)

SigRepoR::addUser(conn = conn, user_tbl = user_tbl)

# Check the imported values
statement <- "select * FROM users"
user_db_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))

# 7. Add signatures ####
LLFS_Transcriptomic_AGS_OmS <- readRDS("~/SigRepoR/inst/data/signatures/LLFS_Transcriptomic_AGS_OmS.rds")
SigRepoR::addSignatureHandler(conn = conn, omic_signature = LLFS_Transcriptomic_AGS_OmS)

LLFS_Transcriptomic_EOA_OmS <- readRDS("~/SigRepoR/inst/data/signatures/LLFS_Transcriptomic_EOA_OmS.rds")
SigRepoR::addSignatureHandler(conn = conn, omic_signature = LLFS_Transcriptomic_EOA_OmS)

LLFS_Transcriptomic_EOAU_OmS <- readRDS("~/SigRepoR/inst/data/signatures/LLFS_Transcriptomic_EOAU_OmS.rds")
SigRepoR::addSignatureHandler(conn = conn, omic_signature = LLFS_Transcriptomic_EOAU_OmS)

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

