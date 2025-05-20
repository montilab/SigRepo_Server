# testing function for SQL lit unit testing implementation



library(DBI)
library(RSQLite)
library(testthat)



# Function to init in-memory database

init_db <- function(){
  # create an in-memory SQLite DB
  
  conn <- dbConnect(RSQLite::SQLite(), ":memory:")

  # read and execute the SQL init script 
  
  init_path <- base::system.file("inst/mysql/schema", package = "SigRepo")

  sql_script <- readLines(base::file.path(init_path, "init.sql"))
  sql_script <- paste(sql_script, collapse = "\n")
  
  # Execute the SQL script
  dbExecute(conn, sql_script)
  
  return(conn)
}

conn <- init_db()