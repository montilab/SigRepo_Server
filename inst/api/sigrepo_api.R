
# For API 
library(plumber)

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

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

# Function to check api key
check_api_key <- function(res, api_key){
  
  # Check if api_key exists in the database
  statement <- SigRepoR::lookup_table_sql(
    table_name = "users",
    return_var = "user_id",
    filter_coln_var = "api_key",
    filter_coln_val = list("api_key" = api_key)
  )
  
  result <- tryCatch({
    DBI::dbGetQuery(conn = SigRepoR::newConnHandler(), statement = statement)
  }, error = function(e){
    # Initialize the serializers
    res$serializer <- serializer[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=paste0(e))
    return(res)
  })
  
  if(nrow(result) == 0){
    
    error_message <- sprintf("api_key = `%s` currently not existed in our database. To add an API Key, please contact our admin for more details.\n", api_key)
    
    # Initialize the serializers
    res$serializer <- serializer[["html"]]
    res$status <- 500
    res$body <- page_not_found(status="500", message=error_message)
    return(res)
    
  }
  
}


## Function to create an error page ####
page_not_found <- function(status, message){
  
  sprintf(
    '
    <!DOCTYPE html>
    <html lang="en">
    <head>
     <title>%s Error</title>
    </head>
    <body>
      <div id="error-404" style="text-align: center;
      margin: 0;
      padding: 0.6em 2em 0.4em;
      border-bottom: 2px solid #000;
      color: #fff;
      background-color: #900;
      font-size: 0.9em;
      font-weight: normal;
      font-family: sans-serif, helvetica;"><h1><strong>%s Error</strong></h1></div>
      <div id="error-message" style="padding: 1em 5em;
      text-align: center;
      border-bottom: 2px solid #000;"><h3>%s</h3></div>
    </body>
    </html>
    ', status, status, message
  )
  
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
  conn <- SigRepoR::newConnHandler()
  
  # Get signatures table from database
  statement <- SigRepoR::lookup_var_sql(
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
  conn <- SigRepoR::newConnHandler()  
  
  # Get organisms table from database
  statement <- SigRepoR::lookup_var_sql(
    table = "organisms",
    return_var = c("organism"),
    filter_coln_var = ifelse(organisms == "all", NULL, "organism"),
    filter_coln_val = ifelse(organisms == "all", NULL, organisms)
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


