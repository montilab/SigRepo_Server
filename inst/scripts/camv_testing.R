# need this script for the difexp table display, need to modify it, onclick 
library(dplyr)
library(readr)
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

# SigRepo updateUser

SigRepo::updateUser(
  conn_handler = conn_handler,
  user_name = "H_Nikoueian",
  active = TRUE,
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


# adding new ensembl reference feature id

# feature_name,organism,gene_symbol,is_current

ref_set_ids <- data.frame(
  feature_name = "ENSG00000223784",
  organism = "homo sapiens",
  gene_symbol = "LINP1",
  is_current = 1
)


SigRepo::addRefFeatureSet(conn_handler = conn_handler,
                          assay_type = "transcriptomics",
                          feature_set = ref_set_ids )


sig_object_pr <- SigRepo::getSignature(conn_handler = conn_handler,
                      signature_id = "321",
                      signature_name = "NECS_SomaScan_SurvivalOffspring")

sig_object_tr <- SigRepo::getSignature(conn_handler = conn_handler,
                                       signature_id = "319",
                                       signature_name = "male SIRT6-transgenic mice vs. male WT")

sig_ex <- sig_object_pr$NECS_SomaScan_SurvivalOffspring$signature


sigs_list <- SigRepo::searchSignature(conn_handler = conn_handler)

platforms_list_test <- SigRepo::searchPlatform(conn_handler)

# adding signatures to the database with updated platforms


SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature_SUM_CYP)


ids <- read_delim('HUMAN_9606_idmapping_selected.tab')

old_ids_human <- readr::read_csv(file.path(data_path, "feature_tables/Transcriptomics_HomoSapiens.csv"), show_col_types = FALSE)

# adding a version column to old_ids_human
old_ids_human <- old_ids_human %>%
  mutate(version = 113)

# writing it to the csv

write.csv(old_ids_human, file = file.path(data_path, "feature_tables/Transcriptomics_HomoSapiens.csv"), row.names = FALSE)


old_ids_mouse <- readr::read_csv(file.path(data_path, "feature_tables/Transcriptomics_MusMusculus.csv"), show_col_types = FALSE)

old_ids_mouse <- old_ids_mouse %>%
  mutate(version = 113)
# write to csv



ref_data <- SigRepo::protTransform(
  organism_code = "HUMAN",
  tax_id = "9606"
)


mouse_data <- SigRepo::protTransform(
  organism_code = "MOUSE",
  tax_id = "10090"
)
# grabbing ref ids 

refs <- SigRepo::searchFeature(conn_handler)

