# need this script for the difexp table display, need to modify it, onclick 
library(dplyr)
# If signature has difexp, get a copy by its signature hash key ####
sig_tran <- SigRepo::getSignature(
  conn_handler = conn_handler,
  signature_name = "CYP181 knockdown in breast cancer cell line",
  verbose = TRUE
)


print(sig_tran)

OmS$extractSignature("abs(score) > 5; adj_p < 0.01")

difexp_sig <- sig_tran$'CYP181 knockdown in breast cancer cell line'$difexp
sig_sig <- sig_tran$'CYP181 knockdown in breast cancer cell line'$signature

user_tbl <- data.frame(
user_name = c("H_Nikoueian"), 
user_password =c( "H_Nikoueian@1443322"),
user_email = c("hnikouei@bu.edu"),
user_first = c("Helia"),
user_last = c("Nikoueian"),
user_affiliation = c("Boston University"),
user_role = c("editor")
)

SigRepo::addUser(
  conn_handler = conn_handler,
  user_tbl = user_tbl,
  verbose = TRUE
)


platforms <- SigRepo::searchPlatform(conn_handler = conn_handler, verbose = TRUE)
signatures <- SigRepo::searchSignature(conn_handler = conn_handler, verbose = TRUE)
searchFeature <-SigRepo::searchFeature(conn_handler = conn_handler, assay_type = "proteomics",verbose = TRUE)


library(readr)
library(devtools)
library(tidyverse)
devtools::load_all()

conn_dbi <- conn_init(conn)

DBI::dbListTables(conn_dbi)


ids <- DBI::dbGetQuery(conn_dbi, statement = "SELECT * FROM signatures")



#connection testing


transcriptomics <- "ENSG" 






system.file(package = "SigRepo")



SigRepo::deleteSignature(conn_handler = conn, signature_id = 62, verbose = TRUE)

# grabbing path of example signatures
ref_ids <- data.frame(feature_name_ids = c("ENSG00000157764", "ENSG00000248378"))
 

ref_check_test <- SigRepo::Refcheck(
  conn_handler = conn_handler,
  ref_ids_tbl = ref_ids
)

signature_objj <- SigRepo::getSignature(conn_handler = conn_handler,
                      signature_name = "HSC3_YAP_KD",
                      signature_id = "279",
  verbose = TRUE)

sig_check_obj <- signature_objj$HSC3_YAP_KD$difexp

difexp_check <- sig_check_obj[2]

# testing the RefCheck function

api_response <- SigRepo::Refcheck(conn_handler = conn_handler,
                  ref_ids_tbl = sig_ex$feature_name, assay = "transcriptomics")


outdated_ids <- api_response[api_response$is_current == "1", ]

# subsetting the na values

bad_ids <- all_missing_transcriptomics_ids$overall

bad_ids <- as.data.frame(bad_ids)

bad_ids <- bad_ids %>%
  rename("feature_name" = "bad_ids")

# need to also add the other assay types <- proteomics
#
