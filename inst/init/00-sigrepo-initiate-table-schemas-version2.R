
# For DB connection
library(RMySQL)
library(DBI)

## Establish database connection
conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# Get path to database schema
mysql_schema_path <- base::system.file("inst/mysql/schema/init.sql", package = "SigRepo")

#
DBI::dbGetQuery(conn = conn, statement = readr::read_file(mysql_schema_path)) # success




