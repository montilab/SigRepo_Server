#' @title test_db_conn_handler
#' @description create a connection handler to the test database use dbConnect.

#'
#' 
#' @export

library(DBI)
library(RMySQL)


test_conn <- SigRepo::newConnHandler(
  dbname = "sigrepo",
  host = "142.93.67.157",
  port = 3306,
  user = "montilab",  
  password = "sigrepo"
)


