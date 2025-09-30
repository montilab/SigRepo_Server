

# For DB connection
library(RMySQL)
library(DBI)

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = base::Sys.getenv("DB_NAME"), 
  host = base::Sys.getenv("DB_HOST"), 
  port = base::as.integer(base::Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"), 
  password = base::Sys.getenv("DB_PASSWORD")
)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all(base::Sys.getenv("SIGREPO_DIR"))

# Loading OmicSignature package
devtools::load_all(base::Sys.getenv("OMICSIG_DIR"))

# Loading hypeR package
devtools::load_all(base::Sys.getenv("HYPER_DIR"))


conn_handler <- SigRepo::newConnHandler(
  dbname = "sigrepo", 
  host = "sigrepo.org", 
  port = 3306, 
  user = base::Sys.getenv("DB_USER"), 
  password = base::Sys.getenv("DB_PASSWORD")
)

SigRepo::searchUser(conn_handler = conn_handler)
SigRepo::deleteUser(conn_handler = conn_handler, user_name = "reinac")
