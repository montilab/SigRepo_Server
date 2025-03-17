#' @title updateSignature
#' @description Update a signature in the database
#' @param conn_handler An established connection handler using SigRepo::newConnhandler()
#' @param signature_id a unique signature id in the database that needs to be updated
#' @param omic_signature An R6 class object from the OmicSignature package
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#'  
#' @examples 
#' 
#' # Create a db connection
#' conn_handler <- SigRepo::newConnHandler(
#'  dbname = "sigrepo", 
#'  host = "montilab.bu.edu", 
#'  port = 3306, 
#'  user = "guest", 
#'  password = "guest"
#' )
#' 
#' # Get a list of signatures that belongs to user = 'guest'
#' signature_tbl <- SigRepo::searchSignature(
#'  conn_handler = conn_handler,
#'  user_name = "guest"
#' )
#' 
#' # Update the desired signature (NOT RUN)
#' # SigRepo::updateSignature(
#' #  conn_handler = conn_handler,
#' #  signature_id = signature_tbl$signature_id,
#' #  omic_signature = omic_signature 
#' # )
#' 
#' @export
updateSignature <- function(
    conn_handler,
    signature_id,
    omic_signature,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = c("SELECT", "INSERT", "DELETE"),
    required_role = "editor"
  )

  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Get unique signature id
  signature_id <- base::unique(signature_id) 
  
  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Check signature_id
  if(!length(signature_id) == 1 || all(signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'signature_id' must have a length of 1 and cannot be empty.")
  }
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "signature_id",
    filter_coln_val = list("signature_id" = signature_id),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("There is no signature_id = '%s' in the 'signatures' table of the SigRepo Database.", signature_id))
    
  }else{
    
    # If user is not admin, check if it has access to signature
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the signature
      signature_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = db_table_name,
        return_var = "*",
        filter_coln_var = c("signature_id", "user_name"), 
        filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(nrow(signature_user_tbl) == 0){
        
        # Get access signature table
        signature_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "signature_access",
          return_var = "*",
          filter_coln_var = c("signature_id", "user_name", "access_type"),
          filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name, "access_type" = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(nrow(signature_access_tbl) == 0){
          
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(base::sprintf("User = '%s' does not have permission to update signature_id = '%s' in the database.", user_name, signature_id))
          
        }
      }
    }
    
    # Create an original omic_signature object in case updating failed ####
    orig_omic_signature <- SigRepo::getSignature(conn_handler = conn_handler, signature_name = signature_tbl$signature_name[1], verbose = FALSE)[[1]]
    
    # Reset the options message
    SigRepo::print_messages(verbose = verbose)
    
    # 1. Delete signature from signatures table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      delete_coln_var = "signature_id",
      delete_coln_val = signature_tbl$signature_id[1],
      check_db_table = FALSE
    )
    
    # 2. Delete signature from signature_feature_set table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_feature_set",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_tbl$signature_id[1],
      check_db_table = TRUE
    )
    
    # 3. If signature has difexp, remove it
    if(signature_tbl$has_difexp[1] == 1){
      # Get API URL
      api_url <- base::sprintf("http://%s:%s/delete_difexp?api_key=%s&signature_hashkey=%s", conn_handler$host[1], conn_handler$api_port[1], conn_info$api_key[1], signature_tbl$signature_hashkey[1])
      # Delete difexp from database
      res <- httr::DELETE(url = api_url)
      # Check status code
      if(res$status_code != 200){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))
        # Show message
        base::stop("Something went wrong with API. Cannot upload difexp table into the SigRepo Database. Please contact admin for more details.\n")
      }
    }
    
    # 4. Updating metadata with new omic_signature object in the database ####
    
    # Check and create signature metadata table ####
    metadata_tbl <- SigRepo::createSignatureMetadata(
      conn_handler = conn_handler,
      omic_signature = omic_signature
    )
    
    # Reset the options message
    SigRepo::print_messages(verbose = verbose)
    
    # Add additional variables in signature metadata table ####
    # Keep its original id and name of the user who owned the signature
    metadata_tbl <- metadata_tbl %>% 
      dplyr::mutate(
        signature_id = signature_tbl$signature_id,
        user_name = signature_tbl$user_name
      )
    
    # Create a new hash key for the signature ####
    metadata_tbl <- SigRepo::createHashKey(
      table = metadata_tbl,
      hash_var = "signature_hashkey",
      hash_columns = c("signature_name", "user_name"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    metadata_tbl <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = "date_created",
      check_db_table = FALSE
    )
    
    # If signature has difexp, save a copy with its signature hash key ####
    # This action must be performed before a signature is imported into the database.
    # This helps to make sure data is properly stored to prevent any interruptions in-between.
    if(metadata_tbl$has_difexp == TRUE){
      # Extract difexp from omic_signature ####
      difexp <- omic_signature$difexp
      # Save difexp to local storage ####
      data_path <- base::system.file("inst/data/difexp", package = "SigRepo")
      if(!base::dir.exists(data_path)){
        base::dir.create(path = base::file.path(base::system.file("inst", package = "SigRepo"), "data/difexp"), showWarnings = FALSE, recursive = TRUE, mode = "0777")
      }
      base::saveRDS(difexp, file = base::file.path(data_path, paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
      # Get API URL
      api_url <- base::sprintf("http://%s:%s/store_difexp?api_key=%s&signature_hashkey=%s", conn_handler$host[1], conn_handler$api_port[1], conn_info$api_key[1], metadata_tbl$signature_hashkey[1])
      # Store difexp in database
      res <- 
        httr::POST(
          url = api_url,
          body = list(
            difexp = httr::upload_file(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")), "application/rds")
          )
        )
      # Check status code
      if(res$status_code != 200){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))
        # Show message
        base::stop("Something went wrong with API. Cannot upload difexp table into the SigRepo Database. Please contact admin for more details.\n")
      }else{
        # Remove files from file system 
        base::unlink(base::file.path(data_path, base::paste0(metadata_tbl$signature_hashkey[1], ".RDS")))
      }
    }
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      table = metadata_tbl,
      check_db_table = FALSE
    ) 
    
    # 5. Importing signature set into database after signature was imported successfully ####
    
    # Get the signature assay type
    assay_type <- metadata_tbl$assay_type[1]
    
    # Add signature set to database based on assay types
    if(assay_type == "transcriptomics"){
      
      # If there is a error during the process, restore the signature to its origin structure and output the messages
      base::tryCatch({
        SigRepo::addTranscriptomicsSignatureSet(
          conn_handler = conn_handler,
          signature_id = metadata_tbl$signature_id[1],
          organism_id = metadata_tbl$organism_id[1],
          signature_set = omic_signature$signature,
          verbose = verbose
        )
      }, error = function(e){
        # Update the signature back to its origin form
        SigRepo::updateSignature(conn_handler = conn_handler, signature_id = signature_id, omic_signature = orig_omic_signature, verbose = FALSE)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(base::paste0(e, "\n"))
      }) 
      
    }else if(assay_type == "proteomics"){
      
    }else if(assay_type == "metabolomics"){
      
    }else if(assay_type == "methylomics"){
      
    }else if(assay_type == "genetic_variations"){
      
    }else if(assay_type == "dna_binding_sites"){
      
    }
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))    
    
    # Return message
    SigRepo::verbose(base::sprintf("\tsignature_id = '%s' has been updated.", metadata_tbl$signature_id[1]))
    
  }
}




