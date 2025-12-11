# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all("/Users/reinachau/SigRepo")

# Loading OmicSignature package
devtools::load_all("/Users/reinachau/OmicSignature")

conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"),
  host = '0.0.0.0',
  port = base::as.integer(Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD"),
  api_host = base::Sys.getenv("API_HOST"),
  api_port = base::Sys.getenv("API_PORT")
)

transcriptomics_tbl <- SigRepo::searchTranscriptomicsFeatureSet(conn_handler = conn_handler)

proteomics_tbl <- SigRepo::searchProteomicsFeatureSet(conn_handler = conn_handler)

organism_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler)

SigRepo::updateTranscriptomicsFeatureSet(
  conn_handler = conn_handler,
  organism = "homo sapiens"
)

SigRepo::updateProteomicsFeatureSet(
  conn_handler = conn_handler,
  organism = "homo sapiens"
)
