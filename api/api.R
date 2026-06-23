
# For API 
library(plumber)
library(httr)
library(jsonlite)

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning
library(dplyr)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all(base::Sys.getenv("SIGREPO_DIR"))

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"), 
  host = base::Sys.getenv("DB_LOCAL_HOST"), 
  port = base::as.integer(base::Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"), 
  password = base::Sys.getenv("DB_PASSWORD")
)

# Create difexp directory
difexp_dir <- base::file.path(base::Sys.getenv("DIFEXP_DIR"))
base::dir.create(path = difexp_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

# Get sigrepo server path
sigrepo_server_path <- base::Sys.getenv("SIGREPO_SERVER_DIR")

# Get the 
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
  base::cat(base::as.character(Sys.time()), "-",
            req$REQUEST_METHOD, req$PATH_INFO, "-",
            req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# Create a list of serializers to return the object ####
serializers <- base::list(
  "html" = plumber::serializer_html(),
  "json" = plumber::serializer_json(),
  "csv" = plumber::serializer_csv(),
  "rds" = plumber::serializer_rds(),
  "pdf" = plumber::serializer_pdf(),
  "text" = plumber::serializer_text(),
  "htmlwidget" = plumber::serializer_htmlwidget()
)

db_connect_local <- function() {
  DBI::dbConnect(
    drv = RMySQL::MySQL(),
    dbname = base::Sys.getenv("DB_NAME"),
    host = base::Sys.getenv("DB_LOCAL_HOST"),
    port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"),
    password = base::Sys.getenv("DB_PASSWORD")
  )
}

json_response <- function(res, status = 200, payload = NULL) {
  res$serializer <- serializers[["json"]]
  res$status <- status
  jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
}

json_error <- function(res, status = 400, message) {
  json_response(
    res = res,
    status = status,
    payload = base::data.frame(MESSAGES = as.character(message), stringsAsFactors = FALSE)
  )
}

require_admin_key <- function(res, admin_key) {
  if (base::missing(admin_key) || is.null(admin_key)) {
    return(json_error(res, 404, "Missing required parameter: admin_key"))
  }

  admin_key <- base::trimws(admin_key[1])

  if (identical(admin_key, "")) {
    return(json_error(res, 404, "admin_key cannot be empty."))
  }

  if (!identical(admin_key, base::Sys.getenv("ADMIN_KEY"))) {
    return(json_error(res, 404, "Invalid admin key."))
  }

  NULL
}

normalize_flag <- function(x, default = TRUE) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(as.integer(default))
  }

  value <- tolower(trimws(as.character(x[1])))
  as.integer(value %in% c("1", "true", "yes", "y"))
}

# Reset database ####
reset_db_tables <- function(conn_handler){
  
  ## Establish database connection
  conn <- DBI::dbConnect(
    drv = RMySQL::MySQL(),
    dbname = base::Sys.getenv("DB_NAME"), 
    host = base::Sys.getenv("DB_LOCAL_HOST"), 
    port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), 
    password = base::Sys.getenv("DB_PASSWORD")
  )
  
  # Set foreign key checks to false when dropping tables
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))
  
  # Show all tables in DB
  table_result <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SHOW TABLES;"))
  
  ###################
  #
  # DROP ALL TABLES
  #
  ##################  
  purrr::walk(
    base::seq_len(base::nrow(table_result)),
    function(t){
      #t=1;
      table_name <- table_result$Tables_in_sigrepo[t]
      drop_table_sql <- base::sprintf("DROP TABLE IF EXISTS `%s`;", table_name)
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))
    }
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))   
  
}

# Function to generate schema for the database ####
generate_db_schema <- function(){
  
  ## Establish database connection
  conn <- DBI::dbConnect(
    drv = RMySQL::MySQL(),
    dbname = base::Sys.getenv("DB_NAME"), 
    host = base::Sys.getenv("DB_LOCAL_HOST"), 
    port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), 
    password = base::Sys.getenv("DB_PASSWORD")
  )
  
  # Set foreign key checks to false when dropping tables
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SET FOREIGN_KEY_CHECKS=0;"))
    
  # Show all tables in DB
  table_result <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SHOW TABLES;"))
  
  ###################
  #
  # DROP ALL TABLES
  #
  ##################  
  purrr::walk(
    base::seq_len(base::nrow(table_result)),
    function(t){
      #t=1;
      table_name <- table_result$Tables_in_sigrepo[t]
      drop_table_sql <- base::sprintf("DROP TABLE IF EXISTS `%s`;", table_name)
      base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = drop_table_sql))
    }
  )
  
  ############# 
  #
  # COLLECTION ACCESS ####
  #
  ############# 
  print("Create schema for 'collection_access' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/collection_access.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  # COLLECTION  ####
  #
  ############# 
  print("Create schema for 'collection' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/collection.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  KEYWORDS ####
  #
  ############# 
  print("Create schema for 'keywords' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/keywords.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))

  #############
  #
  # GENESET RESOURCES ####
  #
  #############
  print("Create schema for 'geneset_resources' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/geneset_resources.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))

  #############
  #
  # GENESET ENTRIES ####
  #
  #############
  print("Create schema for 'geneset_entries' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/geneset_entries.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  ORGANISMS ####
  #
  ############# 
  print("Create schema for 'organisms' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/organisms.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  PHENOTYPES ####
  #
  ############# 
  print("Create schema for 'phenotypes' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/phenotypes.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  PLATFORMS ####
  #
  ############# 
  print("Create schema for 'platforms' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/platforms.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  PROTEOMICS FEATURES ####
  #
  ############# 
  print("Create schema for 'proteomics_features' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/proteomics_features.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  SAMPLE TYPES ####
  #
  ############# 
  print("Create schema for 'sample_types' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/sample_types.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  SIGNATURE ACCESS ####
  #
  ############# 
  print("Create schema for 'signature_access' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/signature_access.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  # SIGNATURE COLLECTION ACCESS ####
  #
  ############# 
  print("Create schema for 'signature_collection_access' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/signature_collection_access.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  # SIGNATURE FEATURE SET ####
  #
  ############# 
  print("Create schema for 'signature_feature_set' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/signature_feature_set.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  # SIGNATURES ####
  #
  ############# 
  print("Create schema for 'signatures' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/signatures.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  TRANSCRIPTOMICS FEATURES ####
  #
  ############# 
  print("Create schema for 'transcriptomics_features' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/transcriptomics_features.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  ############# 
  #
  #  USERS ####
  #
  ############# 
  print("Create schema for 'users' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "mysql/schema/users.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))

  #############
  #
  # FEDERATION NODES ####
  #
  #############
  print("Create schema for 'federation_nodes' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "federation/schema/federation_nodes.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))

  #############
  #
  # FEDERATED SIGNATURES ####
  #
  #############
  print("Create schema for 'federated_signatures' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "federation/schema/federated_signatures.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))

  #############
  #
  # FEDERATED COLLECTIONS ####
  #
  #############
  print("Create schema for 'federated_collections' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "federation/schema/federated_collections.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))

  #############
  #
  # FEDERATION SYNC LOG ####
  #
  #############
  print("Create schema for 'federation_sync_log' table in the database...")
  sql_file <- base::file.path(sigrepo_server_path, "federation/schema/federation_sync_log.sql")
  sql_query <- base::paste0(base::readLines(sql_file), collapse = "\n")
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sql_query))
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Print message
  print("Finished creating table schema for the database.")
  
}

# Function to generate a list of reference tables for the database ####
generate_db_tables <- function(conn_handler){
  
  ############# 
  #
  #  ORGANISMS ####
  #
  ############# 
  print("Upload organisms to the database...")
  organism_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/organisms.csv"))
  SigRepo::addOrganism(conn_handler = conn_handler, organism_tbl = organism_tbl)
  
  ############# 
  #
  #  PLATFORMS ####
  #
  ############# 
  print("Upload platforms to the database...")
  platform_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/platforms.csv"))
  SigRepo::addPlatform(conn_handler = conn_handler, platform_tbl = platform_tbl)
  
  ############# 
  #
  #  PHENOTYPES ####
  #
  ############# 
  print("Upload phenotypes to the database...")
  phenotype_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/phenotypes.csv"), header = TRUE)
  SigRepo::addPhenotype(conn_handler = conn_handler, phenotype_tbl = phenotype_tbl)
  
  ############# 
  #
  #  SAMPLE TYPES ####
  #
  ############# 
  print("Upload sample types to the database...")
  sample_type_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/sample_types.csv"), header = TRUE)
  SigRepo::addSampleType(conn_handler = conn_handler, sample_type_tbl = sample_type_tbl)
  
  ############# 
  #
  #  TRANSCRIPTOMICS ####
  #
  ############# 
  print("Upload human transcriptomics features to the database...")
  transcriptomics_human_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Transcriptomics_Homo_Sapiens.csv"), header = TRUE) 
  SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = transcriptomics_human_gene_tbl)
  
  print("Upload mouse transcriptomics features to the database...")
  transcriptomics_mouse_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Transcriptomics_Mus_Musculus.csv"), header = TRUE) 
  SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = transcriptomics_mouse_gene_tbl)
  
  ############# 
  #
  #  PROTEOMICS ####
  #
  ############# 
  print("Upload human proteomics features to the database...")
  proteomics_human_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Proteomics_Homo_Sapiens.csv"), header = TRUE) 
  SigRepo::addProteomicsFeatureSet(conn_handler = conn_handler, feature_set = proteomics_human_gene_tbl)
  
  ############# 
  #
  #  USERS ####
  #
  #############   
  print("Upload users to the database...")
  user_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/users.csv"), header = TRUE)
  SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)
  
  # Print message
  print("Finished uploading tables to the database.")
  
}

#* Initiate database with schemas and reference tables
#* @param admin_key
#' @post /init_db
init_db <- function(res, admin_key){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  ## Init database
  base::tryCatch({
    
    print("Initiate schema for the database...")
    generate_db_schema()
    
    print("Upload reference tables to the database...")
    generate_db_tables(conn_handler = conn_handler)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("Finish initialized the database.")
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }, error = function(err){
    
    # print the error message
    print(err)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("ERROR: %s", err)
    res$serializer <- serializers[["json"]]
    res$status <- 500
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  })
  
}

#* Reset schemas and reference tables in the database
#* @param admin_key
#' @post /reset_db
reset_db <- function(res, admin_key){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  ## Reset database
  base::tryCatch({
    
    print("Reset tables in the database...")
    reset_db_tables(conn_handler = conn_handler)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("Finish reset the database.")
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }, error = function(err){
    
    # print the error message
    print(err)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("ERROR: %s", err)
    res$serializer <- serializers[["json"]]
    res$status <- 500
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  })
  
}

#* Initiate schema for the database
#* @param admin_key
#' @post /init_db_schema
init_db_schema <- function(res, admin_key){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  ## Init database
  base::tryCatch({
    
    print("Initiate schema for the database...")
    generate_db_schema()
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("Finish initialized schema for the database.")
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }, error = function(err){
    
    # print the error message
    print(err)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("ERROR: %s", err)
    res$serializer <- serializers[["json"]]
    res$status <- 500
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  })
  
}

#* Initiate reference tables in the database
#* @param admin_key
#' @post /init_db_tables
init_db_tables <- function(res, admin_key){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  ## Init database
  base::tryCatch({
    
    print("Upload reference tables to the database...")
    generate_db_tables(conn_handler = conn_handler)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("Finish initialized reference tables for the database.")
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }, error = function(err){
    
    # print the error message
    print(err)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("ERROR: %s", err)
    res$serializer <- serializers[["json"]]
    res$status <- 500
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  })
  
}

#* Extract data from biomaRt package and update transcriptomics feature set in the database.
#* @param admin_key
#* @param organism
#' @post /update_transcriptomics
update_transcriptomics <- function(res, admin_key, organism = NULL){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  if(base::length(organism) > 0 && base::all(!organism %in% c("", NA))){
    organism <- base::sapply(base::seq_along(organism), function(i){
      x <- base::strsplit(organism[i], ",", fixed = TRUE)[[1]] |> base::trimws() |> base::as.character()
    }) |> base::as.vector() |> base::sort() |> base::unique() 
  }
  
  ############# 
  #
  #  ORGANISMS ####
  #
  ############# 
  print("Getting organisms from the database...")
  organism_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler, organism = organism) |>
    dplyr::filter(!.data$biomart_db %in% c(NA, "") & !.data$biomart_dataset %in% c(NA, ""))
  
  # Check if table is empty
  if(base::nrow(organism_tbl) == 0){
    ## Initialize the serializers
    MESSAGES <- base::sprintf("There are no organisms returned from the search parameters.")
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  }
  
  ############# 
  #
  #  TRANSCRIPTOMICS ####
  #
  ############# 
  print("Updating transcriptomics features to the database for each given organism...")
  for(s in base::seq_len(base::nrow(organism_tbl))){
    #s=1;
    base::tryCatch({
      SigRepo::updateTranscriptomicsFeatureSet(
        conn_handler = conn_handler,
        organism = organism_tbl$organism[s]
      )
    }, error = function(e){
      ## Initialize the serializers
      MESSAGES <- base::sprintf(base::as.character(e))
      res$serializer <- serializers[["json"]]
      res$status <- 200
      warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    })   
  }
  
  ## Initialize the serializers
  MESSAGES <- base::sprintf("Finish updating transcriptomics feature set.")
  res$serializer <- serializers[["json"]]
  res$status <- 200
  warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
  return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  
}

#* Retrieve FTP UniProt data from NCBI and update proteomics feature set in the database
#* @param admin_key
#* @param organism
#' @post /update_proteomics
update_proteomics <- function(res, admin_key, organism = NULL){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Check parameters and trim white spaces
  if(base::length(organism) > 0 && base::all(!organism %in% c("", NA))){
    organism <- base::sapply(base::seq_along(organism), function(i){
      x <- base::strsplit(organism[i], ",", fixed = TRUE)[[1]] |> base::trimws() |> base::as.character()
    }) |> base::as.vector() |> base::sort() |> base::unique() 
  }
  
  ############# 
  #
  #  ORGANISMS ####
  #
  ############# 
  print("Getting organisms from the database...")
  organism_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler, organism = organism) |> 
    dplyr::filter(!.data$prot_organism_code %in% c(NA, "") & !.data$prot_organism_code %in% c(NA, ""))
  
  # Check if table is empty
  if(base::nrow(organism_tbl) == 0){
    ## Initialize the serializers
    MESSAGES <- base::sprintf("There are no organisms returned from the search parameters.")
    res$serializer <- serializers[["json"]]
    res$status <- 200
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  }
  
  ############# 
  #
  #  PROTEOMICS ####
  #
  ############# 
  print("Updating proteomics features in the database for each given organism...")
  for(s in base::seq_len(base::nrow(organism_tbl))){
    #s=1;
    base::tryCatch({
      SigRepo::updateProteomicsFeatureSet(
        conn_handler = conn_handler,
        organism = organism_tbl$organism[s]
      )        
    }, error = function(e){
      ## Initialize the serializers
      MESSAGES <- base::sprintf(base::as.character(e))
      res$serializer <- serializers[["json"]]
      res$status <- 200
      warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }) 
  }
  
  ## Initialize the serializers
  MESSAGES <- base::sprintf("Finish updating proteomics feature set.")
  res$serializer <- serializers[["json"]]
  res$status <- 200
  warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
  return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
  
}

#* Show a list of tables in the database
#* @param admin_key
#' @get /show_db_tables
show_db_tables <- function(res, admin_key){
  
  # parameters
  variables <- c('admin_key')
  
  # Check parameters
  if(base::missing(admin_key)){
    
    missing_variables <- c(base::missing(admin_key))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }  
  
  base::tryCatch({ 
    
    ## Establish database connection
    conn <- DBI::dbConnect(
      drv = RMySQL::MySQL(),
      dbname = base::Sys.getenv("DB_NAME"), 
      host = base::Sys.getenv("DB_LOCAL_HOST"), 
      port = base::as.integer(base::Sys.getenv("DB_PORT")),
      user = base::Sys.getenv("DB_USER"), 
      password = base::Sys.getenv("DB_PASSWORD")
    )
    
    # Show all tables in DB
    table_result <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SHOW TABLES;"))
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return table
    if(base::nrow(table_result) > 0){
      return(jsonlite::toJSON(table_result, pretty=TRUE))
    }else{
      warn_tbl <- base::data.frame(MESSAGES = "Currently, there are no tables existed in the database.")
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }, error = function(err){
    
    # print the error message
    print(err)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("Something went wrong. Contact admin for support.")
    res$serializer <- serializers[["json"]]
    res$status <- 500
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  })
  
}

#* Retrieve a specific table from the database
#* @param admin_key
#* @param db_table_name
#* @param search_var
#* @param search_val
#' @get /retrieve_db_table
retrieve_db_table <- function(res, admin_key, db_table_name, search_var = "", search_val = ""){
  
  # parameters
  variables <- c("admin_key", "db_table_name")
  
  # Check parameters
  if(base::missing(admin_key) || base::missing(db_table_name)){
    
    missing_variables <- c(base::missing(admin_key), base::missing(db_table_name))
    error_message <- sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
    ## Initialize the serializers
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # CHECK ADMIN KEY ####
  if(admin_key == ""){
    
    error_message <- "admin_key cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else if(admin_key != base::Sys.getenv("ADMIN_KEY")){
    
    error_message <- "Invalid admin key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }  
  
  # Check parameters and trim white spaces
  db_table_name <- base::trimws(db_table_name[1]); 
  filter_coln_var <- if(search_var[1] %in% c(NA, "")){ NULL }else{ base::trimws(search_var[1]) } 
  filter_coln_val <- if(search_val[1] %in% c(NA, "")){ NULL }else{ base::trimws(base::strsplit(search_val[1], ",", fixed=TRUE)[[1]]) }
  
  # Whether to add label to search values
  if(base::length(filter_coln_var) > 0 && base::length(filter_coln_val) > 0)
    base::names(filter_coln_val) <- filter_coln_var
  
  # Look up table
  base::tryCatch({ 
    
    ## Establish database connection
    conn <- DBI::dbConnect(
      drv = RMySQL::MySQL(),
      dbname = base::Sys.getenv("DB_NAME"), 
      host = base::Sys.getenv("DB_LOCAL_HOST"), 
      port = base::as.integer(base::Sys.getenv("DB_PORT")),
      user = base::Sys.getenv("DB_USER"), 
      password = base::Sys.getenv("DB_PASSWORD")
    )
    
    print(base::sprintf("Check if '%s' table exists in the database", db_table_name))
    SigRepo::checkDBTable(conn = conn, db_table_name = db_table_name)
    
    print(base::sprintf("Check if values exist in '%s' table of the database", db_table_name))
    table_result <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      filter_coln_var = filter_coln_var,
      filter_coln_val = filter_coln_val
    )
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return table
    if(base::nrow(table_result) > 0){
      return(jsonlite::toJSON(table_result, pretty=TRUE))
    }else{
      warn_tbl <- base::data.frame(MESSAGES = base::sprintf("There are no values returned from the search parameters.\n"))
      return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    }
    
  }, error = function(err){
    
    # print the error message
    print(err)
    
    ## Initialize the serializers
    MESSAGES <- base::sprintf("ERROR: %s", err)
    res$serializer <- serializers[["json"]]
    res$status <- 500
    warn_tbl <- base::data.frame(MESSAGES = MESSAGES)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  })
  
}

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
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
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
    res$status <- 404
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
      filter_coln_val = base::list("api_key" = api_key),
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
    base::saveRDS(difexp[[1]], file = base::file.path(difexp_dir, base::paste0(signature_hashkey, ".RDS")))
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
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
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
    res$status <- 404
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
      filter_coln_val = base::list("api_key" = api_key),
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
  
  # Get difexp file
  difexp_file <- base::file.path(difexp_dir, base::paste0(signature_hashkey, ".RDS"))
  
  # Check if file exists
  if(base::file.exists(difexp_file)){
    difexp <- base::readRDS(difexp_file)
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
    error_message <- base::sprintf('Missing required parameter(s): %s', base::paste0(variables[base::which(missing_variables==TRUE)], collapse=", "))
    
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
    res$status <- 404
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
      filter_coln_val = base::list("api_key" = api_key),
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
  
  # Get difexp file
  difexp_file <- base::file.path(difexp_dir, base::paste0(signature_hashkey, ".RDS"))
  
  # Check if file exists
  if(base::file.exists(difexp_file)){
    base::unlink(difexp_file)
  }
  
  # Return messages
  tbl <- base::data.frame(MESSAGES = base::sprintf("difexp file has been removed for signature_hashkey = '%s'", signature_hashkey))
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}

#* Activate registered users in the database
#* @param user_name
#* @param api_key
#' @get /activate_user
activate_user <- function(res, user_name, api_key){
  
  variables <- c("user_name", "api_key")
  
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
  user_name <- base::trimws(user_name[1])
  api_key <- base::trimws(api_key[1]) 
  
  # Check user_name ####
  if(user_name %in% c(NA, "")){
    
    error_message <- "user_name cannot be empty."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }else{
    
    # Check user table
    check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
    
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
    
  }else if(!api_key %in% base::Sys.getenv("SENDMAIL_KEY")){
    
    error_message <- "Invalid Sendmail API Key."
    res$serializer <- serializers[["json"]]
    res$status <- 404
    warn_tbl <- base::data.frame(MESSAGES = error_message)
    return(jsonlite::toJSON(warn_tbl, pretty=TRUE))
    
  }
  
  # Activate user
  SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, active = TRUE)
  
  # Send email to users to notify their account are activated
  api_url <- base::sprintf("https://montilab.bu.edu/SigRepo/send_notifications/activate_user?user_name=%s&api_key=%s", user_name, api_key)
  
  # Send email to users through montilab server API
  res <- httr::GET(url = api_url)
  
  # Check status code
  if(res$status_code != 200){
    MESSAGES <- base::sprintf("Something went wrong with the API. Cannot activate user. Please contact admin for support.")
  }else{
    MESSAGES <- base::sprintf("User = '%s' has been activated. A notified email has been sent to user.", user_name)
  }
  
  # Return message ####
  res$serializer <- serializers[["json"]]
  res$status <- 200
  tbl <- base::data.frame(MESSAGES = MESSAGES)
  return(jsonlite::toJSON(tbl, pretty=TRUE))
  
}

#* Get federation node info
#' @get /federation/node-info
federation_node_info <- function(res) {
  payload <- base::data.frame(
    node_id = base::Sys.getenv("FEDERATION_NODE_ID", unset = base::Sys.getenv("DB_HOST", unset = "sigrepo-node")),
    node_name = base::Sys.getenv("FEDERATION_NODE_NAME", unset = "SigRepo Node"),
    lab_name = base::Sys.getenv("FEDERATION_LAB_NAME", unset = "Unknown Lab"),
    base_url = base::Sys.getenv("FEDERATION_PUBLIC_BASE_URL", unset = ""),
    api_url = base::sprintf(
      "%s/api",
      base::Sys.getenv("FEDERATION_PUBLIC_BASE_URL", unset = "")
    ),
    auth_mode = base::Sys.getenv("FEDERATION_AUTH_MODE", unset = "public"),
    stringsAsFactors = FALSE
  )

  json_response(res, 200, payload)
}

#* Register or update a federation node
#* @param admin_key
#* @param node_id
#* @param node_name
#* @param lab_name
#* @param base_url
#* @param api_url
#* @param auth_mode
#* @param active
#' @post /federation/catalog/register-node
register_federation_node <- function(res, admin_key, node_id, node_name, lab_name = NULL, base_url, api_url, auth_mode = "public", active = TRUE) {
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  required_values <- list(
    node_id = node_id,
    node_name = node_name,
    base_url = base_url,
    api_url = api_url
  )

  missing_required <- names(required_values)[vapply(required_values, function(x) {
    is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))
  }, logical(1))]

  if (length(missing_required) > 0) {
    return(json_error(
      res,
      400,
      base::sprintf("Missing required parameter(s): %s", base::paste(missing_required, collapse = ", "))
    ))
  }

  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  query <- "
    INSERT INTO federation_nodes
      (node_id, node_name, lab_name, base_url, api_url, auth_mode, active, last_seen_at, last_sync_at)
    VALUES
      (?, ?, ?, ?, ?, ?, ?, NOW(), NULL)
    ON DUPLICATE KEY UPDATE
      node_name = VALUES(node_name),
      lab_name = VALUES(lab_name),
      base_url = VALUES(base_url),
      api_url = VALUES(api_url),
      auth_mode = VALUES(auth_mode),
      active = VALUES(active),
      last_seen_at = NOW()
  "

  DBI::dbExecute(
    conn,
    query,
    params = list(
      trimws(as.character(node_id[1])),
      trimws(as.character(node_name[1])),
      if (!is.null(lab_name) && length(lab_name) > 0) trimws(as.character(lab_name[1])) else NA_character_,
      trimws(as.character(base_url[1])),
      trimws(as.character(api_url[1])),
      trimws(as.character(auth_mode[1])),
      normalize_flag(active, default = TRUE)
    )
  )

  json_response(
    res,
    200,
    base::data.frame(
      MESSAGES = base::sprintf("Federation node '%s' registered or updated.", trimws(as.character(node_id[1]))),
      stringsAsFactors = FALSE
    )
  )
}

#* List federation nodes
#* @param active_only
#' @get /federation/catalog/nodes
list_federation_nodes <- function(res, active_only = TRUE) {
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  query <- "SELECT * FROM federation_nodes"
  params <- list()

  if (normalize_flag(active_only, default = TRUE) == 1) {
    query <- paste(query, "WHERE active = 1")
  }

  query <- paste(query, "ORDER BY node_name ASC")
  node_tbl <- DBI::dbGetQuery(conn, query, params = params)
  json_response(res, 200, node_tbl)
}

#* Upsert federated signature metadata
#* @param admin_key
#* @param node_id
#* @param origin_signature_id
#* @param signature_name
#* @param description
#* @param assay_type
#* @param organism
#* @param sample_type
#* @param platform
#* @param visibility
#* @param has_difexp
#* @param signature_size
#* @param owner_label
#* @param payload_endpoint
#* @param updated_at
#' @post /federation/catalog/upsert-signature
upsert_federated_signature <- function(
  res,
  admin_key,
  node_id,
  origin_signature_id,
  signature_name,
  description = NULL,
  assay_type = NULL,
  organism = NULL,
  sample_type = NULL,
  platform = NULL,
  visibility = TRUE,
  has_difexp = FALSE,
  signature_size = NULL,
  owner_label = NULL,
  payload_endpoint = NULL,
  updated_at = NULL
) {
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  required_values <- list(
    node_id = node_id,
    origin_signature_id = origin_signature_id,
    signature_name = signature_name
  )
  missing_required <- names(required_values)[vapply(required_values, function(x) {
    is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))
  }, logical(1))]

  if (length(missing_required) > 0) {
    return(json_error(
      res,
      400,
      base::sprintf("Missing required parameter(s): %s", base::paste(missing_required, collapse = ", "))
    ))
  }

  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  query <- "
    INSERT INTO federated_signatures
      (node_id, origin_signature_id, signature_name, description, assay_type, organism,
       sample_type, platform, visibility, has_difexp, signature_size, owner_label,
       payload_endpoint, updated_at, last_indexed_at)
    VALUES
      (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, NOW())
    ON DUPLICATE KEY UPDATE
      signature_name = VALUES(signature_name),
      description = VALUES(description),
      assay_type = VALUES(assay_type),
      organism = VALUES(organism),
      sample_type = VALUES(sample_type),
      platform = VALUES(platform),
      visibility = VALUES(visibility),
      has_difexp = VALUES(has_difexp),
      signature_size = VALUES(signature_size),
      owner_label = VALUES(owner_label),
      payload_endpoint = VALUES(payload_endpoint),
      updated_at = VALUES(updated_at),
      last_indexed_at = NOW()
  "

  DBI::dbExecute(
    conn,
    query,
    params = list(
      trimws(as.character(node_id[1])),
      trimws(as.character(origin_signature_id[1])),
      trimws(as.character(signature_name[1])),
      if (!is.null(description) && length(description) > 0) as.character(description[1]) else NA_character_,
      if (!is.null(assay_type) && length(assay_type) > 0) as.character(assay_type[1]) else NA_character_,
      if (!is.null(organism) && length(organism) > 0) as.character(organism[1]) else NA_character_,
      if (!is.null(sample_type) && length(sample_type) > 0) as.character(sample_type[1]) else NA_character_,
      if (!is.null(platform) && length(platform) > 0) as.character(platform[1]) else NA_character_,
      normalize_flag(visibility, default = TRUE),
      normalize_flag(has_difexp, default = FALSE),
      if (!is.null(signature_size) && length(signature_size) > 0 && nzchar(as.character(signature_size[1]))) as.integer(signature_size[1]) else NA_integer_,
      if (!is.null(owner_label) && length(owner_label) > 0) as.character(owner_label[1]) else NA_character_,
      if (!is.null(payload_endpoint) && length(payload_endpoint) > 0) as.character(payload_endpoint[1]) else NA_character_,
      if (!is.null(updated_at) && length(updated_at) > 0 && nzchar(as.character(updated_at[1]))) as.character(updated_at[1]) else NA_character_
    )
  )

  json_response(
    res,
    200,
    base::data.frame(
      MESSAGES = base::sprintf(
        "Federated signature '%s' from node '%s' indexed.",
        trimws(as.character(origin_signature_id[1])),
        trimws(as.character(node_id[1]))
      ),
      stringsAsFactors = FALSE
    )
  )
}

#* List federated signatures
#* @param node_id
#* @param organism
#* @param assay_type
#* @param signature_name
#' @get /federation/catalog/signatures
list_federated_signatures <- function(res, node_id = NULL, organism = NULL, assay_type = NULL, signature_name = NULL) {
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  query <- "
    SELECT fs.*, fn.node_name, fn.lab_name, fn.base_url, fn.api_url
    FROM federated_signatures fs
    INNER JOIN federation_nodes fn ON fs.node_id = fn.node_id
    WHERE fn.active = 1
  "
  params <- list()

  if (!is.null(node_id) && length(node_id) > 0 && nzchar(trimws(as.character(node_id[1])))) {
    query <- paste(query, "AND fs.node_id = ?")
    params <- c(params, trimws(as.character(node_id[1])))
  }
  if (!is.null(organism) && length(organism) > 0 && nzchar(trimws(as.character(organism[1])))) {
    query <- paste(query, "AND fs.organism = ?")
    params <- c(params, trimws(as.character(organism[1])))
  }
  if (!is.null(assay_type) && length(assay_type) > 0 && nzchar(trimws(as.character(assay_type[1])))) {
    query <- paste(query, "AND fs.assay_type = ?")
    params <- c(params, trimws(as.character(assay_type[1])))
  }
  if (!is.null(signature_name) && length(signature_name) > 0 && nzchar(trimws(as.character(signature_name[1])))) {
    query <- paste(query, "AND fs.signature_name LIKE ?")
    params <- c(params, base::sprintf("%%%s%%", trimws(as.character(signature_name[1]))))
  }

  query <- paste(query, "ORDER BY fs.signature_name ASC")
  signature_tbl <- DBI::dbGetQuery(conn, query, params = params)
  json_response(res, 200, signature_tbl)
}

#* Upsert federated collection metadata
#* @param admin_key
#* @param node_id
#* @param origin_collection_id
#* @param collection_name
#* @param description
#* @param visibility
#* @param signature_count
#* @param owner_label
#* @param payload_endpoint
#* @param updated_at
#' @post /federation/catalog/upsert-collection
upsert_federated_collection <- function(
  res,
  admin_key,
  node_id,
  origin_collection_id,
  collection_name,
  description = NULL,
  visibility = TRUE,
  signature_count = NULL,
  owner_label = NULL,
  payload_endpoint = NULL,
  updated_at = NULL
) {
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  required_values <- list(
    node_id = node_id,
    origin_collection_id = origin_collection_id,
    collection_name = collection_name
  )
  missing_required <- names(required_values)[vapply(required_values, function(x) {
    is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(as.character(x[1])))
  }, logical(1))]

  if (length(missing_required) > 0) {
    return(json_error(
      res,
      400,
      base::sprintf("Missing required parameter(s): %s", base::paste(missing_required, collapse = ", "))
    ))
  }

  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  query <- "
    INSERT INTO federated_collections
      (node_id, origin_collection_id, collection_name, description, visibility,
       signature_count, owner_label, payload_endpoint, updated_at, last_indexed_at)
    VALUES
      (?, ?, ?, ?, ?, ?, ?, ?, ?, NOW())
    ON DUPLICATE KEY UPDATE
      collection_name = VALUES(collection_name),
      description = VALUES(description),
      visibility = VALUES(visibility),
      signature_count = VALUES(signature_count),
      owner_label = VALUES(owner_label),
      payload_endpoint = VALUES(payload_endpoint),
      updated_at = VALUES(updated_at),
      last_indexed_at = NOW()
  "

  DBI::dbExecute(
    conn,
    query,
    params = list(
      trimws(as.character(node_id[1])),
      trimws(as.character(origin_collection_id[1])),
      trimws(as.character(collection_name[1])),
      if (!is.null(description) && length(description) > 0) as.character(description[1]) else NA_character_,
      normalize_flag(visibility, default = TRUE),
      if (!is.null(signature_count) && length(signature_count) > 0 && nzchar(as.character(signature_count[1]))) as.integer(signature_count[1]) else NA_integer_,
      if (!is.null(owner_label) && length(owner_label) > 0) as.character(owner_label[1]) else NA_character_,
      if (!is.null(payload_endpoint) && length(payload_endpoint) > 0) as.character(payload_endpoint[1]) else NA_character_,
      if (!is.null(updated_at) && length(updated_at) > 0 && nzchar(as.character(updated_at[1]))) as.character(updated_at[1]) else NA_character_
    )
  )

  json_response(
    res,
    200,
    base::data.frame(
      MESSAGES = base::sprintf(
        "Federated collection '%s' from node '%s' indexed.",
        trimws(as.character(origin_collection_id[1])),
        trimws(as.character(node_id[1]))
      ),
      stringsAsFactors = FALSE
    )
  )
}

#* List federated collections
#* @param node_id
#* @param collection_name
#' @get /federation/catalog/collections
list_federated_collections <- function(res, node_id = NULL, collection_name = NULL) {
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  query <- "
    SELECT fc.*, fn.node_name, fn.lab_name, fn.base_url, fn.api_url
    FROM federated_collections fc
    INNER JOIN federation_nodes fn ON fc.node_id = fn.node_id
    WHERE fn.active = 1
  "
  params <- list()

  if (!is.null(node_id) && length(node_id) > 0 && nzchar(trimws(as.character(node_id[1])))) {
    query <- paste(query, "AND fc.node_id = ?")
    params <- c(params, trimws(as.character(node_id[1])))
  }
  if (!is.null(collection_name) && length(collection_name) > 0 && nzchar(trimws(as.character(collection_name[1])))) {
    query <- paste(query, "AND fc.collection_name LIKE ?")
    params <- c(params, base::sprintf("%%%s%%", trimws(as.character(collection_name[1]))))
  }

  query <- paste(query, "ORDER BY fc.collection_name ASC")
  collection_tbl <- DBI::dbGetQuery(conn, query, params = params)
  json_response(res, 200, collection_tbl)
}
