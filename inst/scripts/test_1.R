# Load R packages
# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all()

# Load OmicSignature package
library("OmicSignature")

# Create a connection handler
conn_handler <- SigRepo::newConnHandler(
  dbname = "sigrepo", 
  host = "142.93.67.157", 
  port = 3306, 
  user = "montilab", 
  password = "sigrepo"
)

# Getting the signature path
signature_path <- base::system.file("inst/data/signatures", package = "SigRepo")

# Read in the signature object
omic_signature_AGS_OmS <- base::readRDS(base::file.path(signature_path, "omic_signature_AGS_OmS.RDS"))
omic_signature_MDA_CYP <- base::readRDS(base::file.path(signature_path, "omic_signature_MDA_CYP.RDS"))

# Create signature metadata
metadata <- base::list(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m",
  organism = "Mus Musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",
  
  # optional and recommended:
  covariates = "none",
  description = "mice MYC reduced expression",
  platform = "GPL6246", # use GEO platform ID
  sample_type = "liver", # use BRENDA ontology
  
  # optional cut-off attributes.
  # specifying them can facilitate the extraction of signatures.
  logfc_cutoff = NULL,
  p_value_cutoff = NULL,
  adj_p_cutoff = 0.05,
  score_cutoff = 5,
  
  # other optional built-in attributes:
  keywords = c("Myc", "KO", "longevity"),
  cutoff_description = NULL,
  author = NULL,
  PMID = 25619689,
  year = 2015,
  
  # example of customized attributes:
  others = list("animal_strain" = "C57BL/6")
)

# Create difexp object
difexp <- base::readRDS(file.path(system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) %>% dplyr::rename(feature_name = ensembl)
colnames(difexp) <- OmicSignature::replaceDifexpCol(colnames(difexp))

# Create signature object
signature <- difexp %>%
  dplyr::filter(abs(score) > metadata$score_cutoff & adj_p < metadata$adj_p_cutoff) %>%
  dplyr::select(probe_id, feature_name, score) %>%
  dplyr::mutate(direction = ifelse(score > 0, "+", "-"))

# Create signature object 
omic_signature <- OmicSignature::OmicSignature$new(
  metadata = metadata,
  signature = signature,
  difexp = difexp
)

# Add signature to database
SigRepo::addSignature(
  conn_handler = conn_handler,        # A handler contains user credentials to establish connection to a remote database
  omic_signature = omic_signature,    # An R6 object obtained from OmicSignature::OmicSignature()
  visibility = FALSE,                 # Whether to make signature public for everyone to use or allow access to authorized users only
  return_signature_id = FALSE,        # Whether to return the uploaded signature id
  verbose = TRUE                      # Whether to print diagnostic messages
)

signature_tbl <- SigRepo::searchSignature(conn_handler = conn_handler)


# Now search for Myc_reduce_mice_liver_24m in the database
# in which we would like to revise the value of platform to GPLXXXXX
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler, 
  signature_name = metadata$signature_name
)

# Updating the signature with the revised omic_signature object
if(nrow(signature_tbl) > 0){
  SigRepo::updateSignature(
    conn_handler = conn_handler, 
    signature_id = signature_tbl$signature_id, 
    omic_signature = omic_signature_MDA_CYP
  )
}

SigRepo::deleteSignature(
  conn_handler = conn_handler, 
  signature_id = 16
)




