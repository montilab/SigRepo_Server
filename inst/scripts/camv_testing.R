# testing script to ensure the minimal demo works correctly


# Load tidyverse package
library(tidyverse)
# devtools
library(devtools)

# Load SigRepo package
load_all()

# Load OmicSignature package
library(OmicSignature)

# Create a connection handler using environment variables
conn_handler <- SigRepo::newConnHandler(
  dbname   = Sys.getenv("DBNAME"),
  host     = Sys.getenv("HOST"),
  port     = as.integer(Sys.getenv("PORT")),
  user     = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD")
)

conn_handler2 <- SigRepo::conn_init(conn_handler)

# Getting the signature path
signature_path <- base::system.file("inst/data/signatures", package = "SigRepo")

# Reading in the signature objects
omic_signature_AGS_OmS <- base::readRDS(base::file.path(signature_path, "omic_signature_AGS_OmS.RDS"))
omic_signature_MDA_CYP <- base::readRDS(base::file.path(signature_path, "omic_signature_MDA_CYP.RDS"))


SigRepo::addSignature(
  conn_handler = conn_handler,
  omic_signature = omic_signature_AGS_OmS
)


SigRepo::addSignature(
  conn_handler = conn_handler,
  omic_signature = omic_signature_MDA_CYP,
  return_missing_features = TRUE            	# Whether to return a list of missing features during upload. Default is FALSE.
)

SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = 151)

signature_tbl <- SigRepo::searchSignature(
  conn_handler = conn_handler,
  signature_name = "LLFS_Aging_Gene_2023"
)

SigRepo::deleteSignature(
  conn_handler = conn_handler,
  signature_id = signature_tbl$signature_id
)

