#' @title test_db_conn_handler
#' @description create a connection handler to the test database
#' @param conn_handler An established connection to database using newConnhandler() 
#'
#' 
#' @export

library(DBI)
library(RMySQL)


test_db <- dbConnect(
  RMySQL::MySQL(),
  dbname = "sigrepo_test",
  host = "localhost",
  port = 3306,
  user = "camv",  # need to create testing connection with super user privs
  password = "camv"
)

