# For DB connection
library(RMySQL)
library(DBI)

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = base::Sys.getenv("DB_NAME"), 
  host = "0.0.0.0", 
  port = base::as.integer(base::Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"), 
  password = base::Sys.getenv("DB_PASSWORD")
)

