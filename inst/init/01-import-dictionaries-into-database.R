
source("R/addFeatureSet.R")
source("R/addKeyword.R")
source("R/addOrganism.R")
source("R/addPhenotype.R")
source("R/addPlatform.R")
source("R/addSampleType.R")
source("R/addSignature.R")
source("R/addSignatureAccess.R")
source("R/addSignatureCollection.R")
source("R/addSignatureFeatureSet.R")
source("R/addTranscriptomicsFeatureSet.R")
source("R/addUser.R")
source("R/newConnHandler.R")
source("R/sqlFunctions.R")

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load OmicSignature package
devtools::load_all("/home/rstudio/OmicSignature")

# Load SigRepoR package
devtools::load_all("/home/rstudio/SigRepoR")

## Establish database connection
conn <- newConnHandle(
  driver = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

## Read in platforms data
platforms <- readRDS("/home/rstudio/SigRepoShiny/miscellanea/platforms/GEOplatform_2024.rds") 

# 1. Add organisms to database
organism_tbl <- platforms %>% 
  dplyr::transmute(
    organism = Organism %>% tolower() %>% trimws() %>% gsub("'", "", .)
  ) %>% 
  dplyr::distinct(organism, .keep_all = TRUE) %>% 
  base::replace(is.na(.), "'NULL'") %>% 
  base::replace(. == "", "'NULL'")

addOrganism(conn=conn, organism_tbl = organism_tbl)

# Check the imported values
table_query <- "select * FROM organisms"
query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)

# 2. Add platforms to database
platform_tbl <- platforms %>% 
  dplyr::transmute(
    platform_id = Accession,
    platform = Name,
    seq_technology = Technology,
    organism = Organism %>% tolower() %>% trimws() %>% gsub("'", "", .)
  ) %>% 
  dplyr::distinct(platform_id, .keep_all = TRUE) %>% 
  base::replace(is.na(.), "'NULL'") %>% 
  base::replace(. == "", "'NULL'")

addPlatform(conn=conn, platform_tbl = platform_tbl)

# 3. Add sample types to database

# Read in the brenda dictionary
sample_type_tbl <- read.delim("/home/rstudio/SigRepoShiny/miscellanea/BRENDA/BRENDA_non-obsolete_leaf.txt", header = FALSE, col.names = c("entry", "brenda_accession", "sample_type"))  %>% 
  dplyr::transmute(
    sample_type = sample_type,
    brenda_accession = brenda_accession
  ) %>% 
  dplyr::distinct(sample_type, .keep_all = TRUE) %>% 
  base::replace(is.na(.), "'NULL'") %>% 
  base::replace(. == "", "'NULL'")

addSampleType(conn=conn, sample_type_tbl = sample_type_tbl)

# 4. Add transcriptomics feature set

# Read in the human and mouse gene symbols 
human_gene_symbol_tbl <- read.csv("~/SigRepoShiny/miscellanea/gene_symbols/homo_sapiens.txt", sep="", na.strings = "'NULL'") %>% 
  dplyr::transmute(
    feature_name = hgnc_symbol,
    assay_type = "transcriptomics",
    organism = "homo sapiens",
    description = description,
    synonyms = synonyms,
    n_synonyms = n_synonyms,
    ensemble_ids = ensemble_ids,
    n_ensembl_ids = n_ensembl_ids,
    transcript_biotypes = transcript_biotypes,
    chromosome_name = chromosome_name,
    start_position = start_position,
    end_position = end_position
  ) %>% 
  dplyr::distinct(feature_name, .keep_all = TRUE) %>% 
  base::replace(is.na(.), "'NULL'") %>% 
  base::replace(. == "", "'NULL'")

mouse_gene_symbol_tbl <- read.csv("/home/rstudio/SigRepoShiny/miscellanea/gene_symbols/mus_musculus.txt", sep = "") %>% 
  dplyr::transmute(
    feature_name = mgi_symbol,
    assay_type = "transcriptomics",
    organism = "mus musculus",
    description = mgi_description,
    synonyms = synonyms,
    n_synonyms = n_synonyms,
    ensemble_ids = ensemble_ids,
    n_ensembl_ids = n_ensembl_ids,
    transcript_biotypes = transcript_biotypes,
    chromosome_name = chromosome_name,
    start_position = start_position,
    end_position = end_position
  ) %>% 
  dplyr::distinct(feature_name, .keep_all = TRUE) %>% 
  base::replace(is.na(.), "'NULL'") %>% 
  base::replace(. == "", "'NULL'")

# Combine human and mouse gene symbols
feature_set <- human_gene_symbol_tbl %>% rbind(mouse_gene_symbol_tbl)

addRefFeatureSet(conn = conn, assay_type = "transcriptomics", feature_set = feature_set)

# 5. Add users (optional) Default guest guest

## Create an user df
user_tbl <- data.frame(
  user_name = c("rchau88", "smonti", "vmli", "lkroeh", "andrewdr", "zihuang"), 
  user_password = "password",
  user_email = c("rchau88@bu.edu", "smonti@bu.edu", "vmli@bu.edu", "lkroeh@bu.edu", "andrewdr@bu.edu", "zihuang@bu.edu"), 
  user_first = c("Reina", "Stefano", "Vanessa", "Lina", "Andrew", "Ziwei"), 
  user_last = c("Chau", "Monti", "Li", "Kroehling", "Chen", "Huang"), 
  user_affiliation = "Boston University",
  user_role = "admin",
  stringsAsFactors = FALSE
) %>% 
  dplyr::distinct(user_name, .keep_all = TRUE) %>% 
  base::replace(is.na(.), "'NULL'") %>% 
  base::replace(. == "", "'NULL'")

addUser(conn = conn, user_tbl = user_tbl)

# 6. Add signatures (optional)
LLFS_Transcriptomic_AGS_OmS <- readRDS("~/SigRepoShiny/miscellanea/signatures/LLFS_Transcriptomic_AGS_OmS.rds")

addSignature(conn = conn, omic_signature = LLFS_Transcriptomic_AGS_OmS, user_id = "rchau88", user_role="admin")


