# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load package
devtools::load_all()
load_all("OmicSignature")

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# Get data path
data_path <- base::system.file("inst/data", package = "SigRepo")

## Establish database connection
conn <- SigRepo::conn_init(conn_handler = conn_handler)

SigRepo::searchSignature(conn_handler = conn_handler)
SigRepo::searchSignature(conn_handler = conn_handler, signature_name = "kddkdiingn")
SigRepo::searchSignature(conn_handler = conn_handler, signature_name = "LLFS_Aging_Gene_2023")


omic_signature <- SigRepo::getSignature(conn_handler = conn_handler, signature_name = "LLFS_Aging_Gene_2023")[[1]]








conn_handler <- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "montilab.bu.edu",
  port = 3306,
  user = "guest",
  password = "guest"
)

# Get a list of signatures that belongs to user = 'guest'
signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler
)

# Update the desired signature 
SigRepo::updateSignature(
  conn_handler = conn_handler,
  signature_id = signature_tbl$signature_id,
  omic_signature = omic_signature
)





signatures <- SigRepo::getSignature(conn_handler = conn_handler, signature_name = "LLFS_Aging_Gene_2023")
  











