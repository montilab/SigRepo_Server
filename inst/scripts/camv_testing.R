# trsting script for the minimal demo of the sigrepo package


library(readr)
library(devtools)
library(tidyverse)
devtools::load_all()

# grabbing path of example signatures

signature_path <- base::system.file("inst/data/signatures", package = "SigRepo")

# grabbing all omics type signatures
transcriptomic_sig <- base::readRDS(base::file.path(signature_path, "omic_signature_AGS_OmS.RDS"))

prot_signature_example <- base::readRDS(base::file.path(signature_path, "prot_omic_signature_ex.RDS"))

# metabolics_sig <- base::readRDS(base::file.path(signature_path, "metabolomic_signature_ex.RDS"))

# dna_sig <- base::readRDS(base::file.path(signature_path, "dna_signature_ex.RDS"))

# methylomics_sig <- base::readRDS(base::file.path(signature_path, "methylomic_signature_ex.RDS"))

# genetic_sig <- base::readRDS(base::file.path(signature_path, "genetic_signature_ex.RDS"))

prot_ids <- read_csv(base::file.path(signature_path, "feature_tables/Proteomics_HomoSapiens.csv"))


prot_ids <- prot_ids %>%
  rename(gene_symbol = symbol)

# saving to csv 
readr::write_csv(prot_ids, base::file.path(signature_path, "feature_tables/Proteomics_HomoSapiens.csv"))


# Creating connection handler

conn_handler <- SigRepo::newConnHandler(
  dbname   = Sys.getenv("DBNAME"),
  host     = Sys.getenv("HOST"),
  port     = 3307,
  user     = "root",
  password = "sigrepo"
)



# adding signature

signature_upload <-   SigRepo::addSignature(conn_handler, 
                      prot_signature_example,
                      verbose = TRUE, 
                      return_signature_id =  TRUE)

SigRepo::addSignature(conn_handler,
                      metabolics_sig,
                      verbose = TRUE,
                      return_signature_id = TRUE)

SigRepo::addSignature(conn_handler,
                      dna_sig,
                      verbose = TRUE,
                      return_signature_id = TRUE)

8


signatures <- SigRepo::getSignature(conn_handler)

