# trsting script for the minimal demo of the sigrepo package


library(readr)
library(devtools)
library(tidyverse)
devtools::load_all()

conn_dbi <- conn_init(conn)

DBI::dbListTables(conn_dbi)


ids <- DBI::dbGetQuery(conn_dbi, statement = "SELECT * FROM signatures")



#connection testing

conn <- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = Sys.getenv("HOST"),  
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD") 

)

SigRepo::deleteSignature(conn_handler = conn, signature_id = 62, verbose = TRUE)

# grabbing path of example signatures

signature_path <- base::system.file("tests/test_data", package = "SigRepo")

# grabbing all omics type signatures

test_transcriptomic_sig <- base::readRDS(base::file.path(signature_path, "test_data_transcriptomics.RDS"))

sig_id <- SigRepo::addSignature(conn, test_transcriptomic_sig, return_signature_id = TRUE, verbose = TRUE)

# creating the revised signature for the updateSignature 

# 1. Revise the metadata object with new platform = GPLXXXXX
metadata_revised <- base::list(
  # required attributes:
  signature_name = "Myc_reduce_mice_liver_24m",
  organism = "Mus Musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = "Myc_reduce",
  
  # optional and recommended:
  covariates = "none",
  description = "mice MYC reduced expression",
  platform = "GPLXXXXX", # use GEO platform ID
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

# 2. Create difexp object
difexp <- readRDS(file.path(system.file("extdata", package = "OmicSignature"), "difmatrix_Myc_mice_liver_24m.rds")) %>% dplyr::rename(feature_name = ensembl)
colnames(difexp) <- OmicSignature::replaceDifexpCol(colnames(difexp))

# 3. Create signature object
signature <- difexp %>%
  dplyr::filter(abs(score) > metadata_revised$score_cutoff & adj_p < metadata_revised$adj_p_cutoff) %>%
  dplyr::select(probe_id, feature_name, score) %>%
  dplyr::mutate(direction = ifelse(score > 0, "+", "-"))

# 4. Create the updated OmicSignature object
test_transcriptomic_sig_revised <- OmicSignature::OmicSignature$new(
  signature = signature,
  metadata = metadata_revised,
  difexp = difexp
)

# exporting the revised omic signature into an RDS object

base::saveRDS(test_transcriptomic_sig_revised, file = file.path(signature_path, "test_data_transcriptomics_revised.RDS"))

conns <- DBI::dbListConnections(RMySQL::MySQL())
lapply(conns, DBI::dbDisconnect)