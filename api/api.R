
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

# Function to generate schema for the database ####
generate_db_schema <- function(conn_handler){
  
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
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))   
  
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
  SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = transcriptomics_human_gene_tbl)
  
  print("Upload mouse transcriptomics features to the database...")
  transcriptomics_mouse_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Transcriptomics_Mus_Musculus.csv"), header = TRUE) 
  SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "transcriptomics", feature_set = transcriptomics_mouse_gene_tbl)
  
  ############# 
  #
  #  PROTEOMICS ####
  #
  ############# 
  print("Upload human proteomics features to the database...")
  proteomics_human_gene_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/Proteomics_Homo_Sapiens.csv"), header = TRUE) 
  SigRepo::addRefFeatureSet(conn_handler = conn_handler, assay_type = "proteomics", feature_set = proteomics_human_gene_tbl)
  
  ############# 
  #
  #  USERS ####
  #
  #############   
  print("Upload users to the database...")
  user_tbl <- utils::read.csv(base::file.path(sigrepo_server_path, "mysql/data/users.csv"), header = TRUE)
  SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)
  
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
    generate_db_schema(conn_handler = conn_handler)
    
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
    generate_db_schema(conn_handler = conn_handler)
    
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
retrieve_db_table <- function(res, admin_key, db_table_name, search_var="", search_val=""){
  
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
