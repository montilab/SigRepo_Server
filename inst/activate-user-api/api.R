
# For API 
library(plumber)
library(httr)
library(jsonlite)

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all()

# Create a default database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"),
  host = Sys.getenv("HOST"),
  port = as.integer(Sys.getenv("PORT")),
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD")
)

# Function to generate random password
randPassword <- function(n = 1){
  a <- base::do.call(base::paste0, base::replicate(5, base::sample(LETTERS, n, TRUE), FALSE))
  base::paste0(a, base::sprintf("%04d", base::sample(9999, n, TRUE)), base::sample(LETTERS, n, TRUE))
}

# Function to send a notification email to the user and let them know that their
# account are activated
send_notifications_to_user <- function(
    from_sender = "sigrepo@bu.edu",
    user_name = "rchau88",
    user_email = "rchau88@bu.edu"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>SIGREPO</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', user_name, ',</strong></p>',
      '<br>',
      '<p>Your request to access our <strong>SigRepo</strong> database has been granted.</p>',
      '<br>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<br>',
      '<p>To log back in? Please follow this link, <strong>https://sigrepo.org/</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Account Activated Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

# Function to send temporary password to user
send_tmp_password_to_user <- function(
    from_sender = "sigrepo@bu.edu",
    user_name = "rchau88",
    user_email = "rchau88@bu.edu"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>SIGREPO</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', user_name, ',</strong></p>',
      '<br>',
      '<p>Below is a temporary password for accessing your account on the <strong>SigRepo</strong> database.</p>',
      '<br>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Temporary password: <strong>', temp_password, '</strong></p>',
      '<br>',
      '<p>To log back in? Follow this link, <strong>https://sigrepo.org/</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Temporary Password Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

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
serializers <- list(
  "html" = plumber::serializer_html(),
  "json" = plumber::serializer_json(),
  "csv" = plumber::serializer_csv(),
  "rds" = plumber::serializer_rds(),
  "pdf" = plumber::serializer_pdf(),
  "text" = plumber::serializer_text(),
  "htmlwidget" = plumber::serializer_htmlwidget()
)

#* Send notifications to user that their account is activated
#* @param user_name
#* @param api_key
#' @get /send_notifications
send_notifications <- function(res, user_name, api_key){

  variables <- c("api_key", "user_name")
  
  # Check parameters
  if(base::missing(user_name) || base::missing(api_key)){
    
    missing_variables <- c(base::missing(user_name),  base::missing(api_key))
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[which(missing_variables==TRUE)], collapse = ", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]) 
  user_name <- base::trimws(user_name[1])
  
  # Check user_name ####
  if(user_name %in% c(NA, "")){
    
    error_message <- "user_name cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    # Check user table
    user_tbl <- check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
    
    # If user exists, throw an error
    if(nrow(check_user_tbl) == 0){
      error_message <- base::sprintf("User = '%s' does not exist in our database. Please choose a different name.", user_name)
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(!api_key %in% base::Sys.getenv("API_KEY")){
    
    error_message <- "Invalid API Key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Update user with new password
  SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, active = TRUE)
  
  # Send email to users
  send_notifications_to_user(
    user_name = user_tbl$user_name[1],
    user_email = user_tbl$user_email[1]
  )
  
  # Return message
  tbl <- base::data.frame(MESSAGES = base::sprintf("\tUser = '%s' account has been activated. Sending a notified email to user!\n", user_name))
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}

# Function to send temporary password to user
#* @param user_name
#* @param api_key
#' @get /send_tmp_password
send_tmp_password <- function(res, user_name, api_key){
  
  variables <- c("api_key", "user_name")
  
  # Check parameters
  if(base::missing(user_name) || base::missing(api_key)){
    
    missing_variables <- c(base::missing(user_name),  base::missing(api_key))
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[which(missing_variables==TRUE)], collapse = ", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  api_key <- base::trimws(api_key[1]) 
  user_name <- base::trimws(user_name[1])
  
  # Check user_name ####
  if(user_name %in% c(NA, "")){
    
    error_message <- "user_name cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    # Check user table
    user_tbl <- check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
    
    # If user exists, throw an error
    if(nrow(check_user_tbl) == 0){
      error_message <- base::sprintf("User = '%s' does not exist in our database. Please choose a different name.", user_name)
      res$serializer <- serializers[["json"]]
      res$status <- 404
      warn_tbl <- base::data.frame(MESSAGES = error_message)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }
  
  # Check api_key ####
  if(api_key %in% ""){
    
    error_message <- "api_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(!api_key %in% base::Sys.getenv("API_KEY")){
    
    error_message <- "Invalid API Key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Create a temporary password
  tmp_pwd <- randPassword()
  
  # Update user with new password
  SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, password = tmp_pwd)
  
  # Send email to users
  send_tmp_password_to_user(
    user_name = user_tbl$user_name[1],
    user_email = user_tbl$user_email[1],
    temp_password = tmp_pwd
  )
  
  # Return message
  tbl <- base::data.frame(MESSAGES = base::sprintf("A temporary password has been sent to user = '%s'.", user_tbl$user_name[1]))
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}



