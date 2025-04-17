# testing proteomics add signature
library(readr)


signature_path <- base::system.file("inst/data/", package = "SigRepo")


prot_signature_example <- base::readRDS(base::file.path(signature_path, "prot_omic_signature_ex.RDS"))

prot_ids <- read_csv(base::file.path(signature_path, "feature_tables/Proteomics_HomoSapiens.csv"))


prot_ids <- prot_ids %>%
  rename(gene_symbol = symbol)

# saving to csv 
readr::write_csv(prot_ids, base::file.path(signature_path, "feature_tables/Proteomics_HomoSapiens.csv"))

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

