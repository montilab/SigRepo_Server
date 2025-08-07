
# For API 
library(plumber)

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
  host = Sys.getenv("HOST_DB_NET"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

# 
# This is a Plumber API. You can run the API by clicking the 'run API' button above
# 
# Found out more about building APIs with Plumber here:
#
#       https://www.rplumber.io/
#

# API title and description
#* @apiTitle Plumber API
#* @apiDescription This is a server for accessing data on the SigRepo Database
#* @apiContact list(name = "Reina Chau", email = "rchau88@bu.edu") 

# Set-up header access

#* @filter cors
cors <- function(res){
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-",
      req$REQUEST_METHOD, req$PATH_INFO, "-",
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# Create a list of serializers to return the object ####
serializer <- list(
  "html" = plumber::serializer_html(),
  "json" = plumber::serializer_json(),
  "csv" = plumber::serializer_csv(),
  "rds" = plumber::serializer_rds(),
  "pdf" = plumber::serializer_pdf(),
  "text" = plumber::serializer_text(),
  "htmlwidget" = plumber::serializer_htmlwidget()
)

# Create difexp directory
data_path <- base::file.path("/difexp")
base::dir.create(path = data_path, showWarnings = FALSE, recursive = TRUE, mode = "0777")

#* Store difexp in the database
#* @parser multi
#* @parser rds
#* @param api_key
#* @param signature_hashkey
#* @param difexp:file
#' @post /store_difexp
store_difexp <- function(res, api_key, signature_hashkey, difexp){
  
  # parameters
  variables <- c('api_key', 'signature_hashkey')
  
  # Check parameters
  if(base::missing(api_key)){
    
    missing_variables <- c(base::missing(api_key), base::missing(signature_hashkey))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]); 
  signature_hashkey <- base::trimws(signature_hashkey[1]); 
  
  # Check signature_hashkey ####
  if(signature_hashkey %in% c(NA, "")){
    
    error_message <- "signature_hashkey cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    ## Establish database connection
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    
    # Check if api_key exists in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "*",
      filter_coln_var = "api_key",
      filter_coln_val = list("api_key" = api_key),
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Check user tbl
    if(nrow(user_tbl) == 0){
      error_message <- "Invalid api key."
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Store difexp
  if(base::is.data.frame(difexp[[1]])){
    base::saveRDS(difexp[[1]], file = base::file.path(data_path, base::paste0(signature_hashkey, ".RDS")))
  }else{
    warn_tbl <- base::data.frame(MESSAGES = base::sprintf("difexp is not a valid file."))
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  }  
  
}


#* Get difexp from the database
#* @param api_key
#* @param signature_hashkey
#' @get /get_difexp
get_difexp <- function(res, api_key, signature_hashkey){
  
  # Parameters
  variables <- c('api_key', 'signature_hashkey')
  
  # Check parameters
  if(base::missing(api_key)){
    
    missing_variables <- c(base::missing(api_key), base::missing(signature_hashkey))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]); 
  signature_hashkey <- base::trimws(signature_hashkey[1]); 
  
  # Check signature_hashkey ####
  if(signature_hashkey %in% c(NA, "")){
    
    error_message <- "signature_hashkey cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    ## Establish database connection
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    
    # Check if api_key exists in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "*",
      filter_coln_var = "api_key",
      filter_coln_val = list("api_key" = api_key),
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Check user tbl
    if(nrow(user_tbl) == 0){
      error_message <- "Invalid API Key."
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Get difexp path
  difexp_path <- base::file.path(data_path, base::paste0(signature_hashkey, ".RDS"))
  
  # Check if file exists
  if(base::file.exists(difexp_path)){
    difexp <- base::readRDS(difexp_path)
    return(jsonlite::toJSON(difexp, pretty=TRUE))
  }else{
    warn_tbl <- base::data.frame(MESSAGES = base::sprintf("There is no difexp file found for signature_hashkey = '%s'", signature_hashkey))
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  }  
  
}

#* Delete difexp from the database
#* @param api_key
#* @param signature_hashkey
#' @delete /delete_difexp
delete_difexp <- function(res, api_key, signature_hashkey){
  
  # Parameters
  variables <- c('api_key', 'signature_hashkey')
  
  # Check parameters
  if(base::missing(api_key)){
    
    missing_variables <- c(base::missing(api_key), base::missing(signature_hashkey))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]); 
  signature_hashkey <- base::trimws(signature_hashkey[1]); 
  
  # Check signature_hashkey ####
  if(signature_hashkey %in% c(NA, "")){
    
    error_message <- "signature_hashkey cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    ## Establish database connection
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    
    # Check if api_key exists in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = "*",
      filter_coln_var = "api_key",
      filter_coln_val = list("api_key" = api_key),
      check_db_table = TRUE
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Check user tbl
    if(nrow(user_tbl) == 0){
      error_message <- "Invalid API Key."
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Get difexp path
  difexp_path <- base::file.path(data_path, base::paste0(signature_hashkey, ".RDS"))
  
  # Check if file exists
  if(base::file.exists(difexp_path)){
    base::unlink(difexp_path)
  }
  
  # Return messages
  tbl <- base::data.frame(MESSAGES = base::sprintf("difexp file has been removed for signature_hashkey = '%s'", signature_hashkey))
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}

#* Get a list of signatures available in the database
#* @param author_id
#* @param api_key
#' @get /signatures
signatures <- function(res, author_id="all", api_key){
  
  variables <- c("author_id", "api_key")
  
  # Check parameters
  if(base::missing(author_id) || base::missing(api_key)){
    
    missing_variables <- c(base::missing(author_id),  base::missing(api_key))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse = ", "))
    
    # Initialize the serializers
    res$serializer <- serializer[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=error_message)
    
    return(res)
    
  }
  
  # Check api key
  check_api_key(res=res, api_key = api_key)
  
  ## Establish database connection
  conn <- SigRepo::newConnHandler()
  
  # Get signatures table from database
  statement <- SigRepo::lookup_var_sql(
    table = "signatures",
    return_var = c("signature_name", "user_id", "uploaded_date"),
    filter_coln_var = ifelse(author_id == "all", NULL, "user_id"),
    filter_coln_val = ifelse(author_id == "all", NULL, author_id)
  )  
  
  table <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    # Initialize the serializers
    res$serializer <- serializer[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=paste0(e))
    return(res)
  })
  
  if(nrow(table) == 0){
    return(jsonlite::toJSON(data.frame(WARNING="There is no data returned from the search paramters"), pretty=TRUE)) 
  }else{
    return(jsonlite::toJSON(table, pretty=TRUE))
  }
  
}

#* Get a list of organisms available in the database
#* @param organism
#* @param api_key
#' @get /organisms
organisms <- function(res, organism="all", api_key){
  
  variables <- c("organisms", "api_key")
  
  # Check parameters
  if(base::missing(organisms) || base::missing(api_key)){
    
    missing_variables <- c(base::missing(organisms),  base::missing(api_key))
    error_message <- sprintf('Missing required parameter(s): %s', paste0(variables[which(missing_variables==TRUE)], collapse = ", "))
    
    # Initialize the serializers
    res$serializer <- serializer[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=error_message)
    
    return(res)
    
  }
  
  ## Establish database connection
  conn <- SigRepo::newConnHandler()  
  
  # Get organisms table from database
  statement <- SigRepo::lookup_var_sql(
    table = "organisms",
    return_var = c("organism"),
    filter_coln_var = ifelse(organisms == "all", NULL, "organism"),
    filter_coln_val = ifelse(organisms == "all", NULL, organisms)
  )  
  
  # Get 
  table <- base::tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    # Initialize the serializers
    res$serializer <- serializer[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=paste0(e))
    return(res)
  })
  
  if(nrow(table) == 0){
    return(jsonlite::toJSON(data.frame(WARNING="There is no data returned from the search paramters"), pretty=TRUE)) 
  }else{
    return(jsonlite::toJSON(table, pretty=TRUE))
  }
  
}

