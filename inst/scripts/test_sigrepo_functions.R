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


SigRepo::searchSignature(conn_handler = conn_handler)
SigRepo::searchSignature(conn_handler = conn_handler, signature_name = "kddkdiingn")
SigRepo::searchSignature(conn_handler = conn_handler, signature_name = "LLFS_Aging_Gene_2023")



