
# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all()

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("DB_USER"), 
  password = Sys.getenv("PASSWORD")
)

# Look for current users in the database
SigRepo::searchUser(conn_handler = conn_handler)

# Delete users (just make it inactive as there are signatures are tied to those users)
SigRepo::deleteUser(conn_handler = conn_handler, user_name = "rchau")
SigRepo::deleteUser(conn_handler = conn_handler, user_name = "rchau88")


SigRepo::updateUser(conn_handler = conn_handler, user_name = 'rchau', active = 1)
SigRepo::updateUser(conn_handler = conn_handler, user_name = 'rchau88', active = 1)






## Create a database handler
user_conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = "rchau88", 
  password = "123456789"
)

SigRepo::validateUser(conn_handler = user_conn_handler)

SigRepo::searchUser(conn_handler = conn_handler)
SigRepo::searchSignature(conn_handler = conn_handler)
SigRepo::searchSampleType(conn_handler = conn_handler)







## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)


SigRepo::updateUser(conn_handler = conn_handler, user_name = 'rchau88', active = FALSE)
SigRepo::searchUser(conn_handler = conn_handler)







## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# Establish user connection ###
conn <- SigRepo::conn_init(conn_handler = conn_handler)

SigRepo::deleteUser(
  conn_handler = conn_handler,
  user_name = "rchau88"
)

SigRepo::delete_table_sql(
  conn = conn,
  db_table_name = "users",
  delete_coln_var = "user_name",
  delete_coln_val = "rchau88"
)

SigRepo::searchUser(conn_handler = conn_handler)


