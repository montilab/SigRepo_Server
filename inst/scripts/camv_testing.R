# need this script for the difexp table display, need to modify it, onclick 

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



system.file(package = "SigRepo")



SigRepo::deleteSignature(conn_handler = conn, signature_id = 62, verbose = TRUE)

# grabbing path of example signatures



