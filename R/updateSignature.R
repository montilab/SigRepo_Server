#' @title updateSignature
#' @description Update a signature in the database
#' @param conn_handler An established connection handler using SigRepo::newConnhandler()
#' @param signature_id a unique signature id in the database that needs to be updated
#' @param omic_signature An R6 class object from the OmicSignature package
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
#' signature_tbl <- sigRepo::searchSignature(
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
    omic_signature
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )

  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Check signature_id
  if(!length(signature_id) == 1 || signature_id %in% c(NA, "")){
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn)) 
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
    base::suppressMessages(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(sprintf("There is no signature_id = '%s' in the 'signatures' table of the SigRepo Database.", signature_id))
    
  }else{
    
    # If user is not admin, check if it has access to the signature
    if(user_role != "admin"){
      
      # Check if user was the one who uploaded the signature
      signature_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signatures",
        return_var = "*",
        filter_coln_var = c("signature_id", "user_name"), 
        filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(nrow(signature_tbl) == 0){
        
        signature_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = db_table_name,
          return_var = "*",
          filter_coln_var = c("signature_id", "user_name", "access_type"),
          filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name, access_type = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(nrow(signature_access_tbl) == 0){
          
          # Disconnect from database ####
          base::suppressMessages(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(sprintf("User = '%s' does not have permission to update signature_id = '%s' in the database.", user_name, signature_id))
          
        }
      }
    }
    
    # 1. Delete the signature from the database ####
    base::suppressMessages(
      SigRepo::deleteSignature(
        conn_handler = conn_handler,
        signature_id = signature_tbl$signature_id
      )
    )
    
    # 2. Uploading signature metadata into database ####
    
    # Check and create signature metadata table ####
    metadata_tbl <- SigRepo::createSignatureMetadata(
      conn_handler = conn_handler,
      omic_signature = omic_signature
    )
    
    # Create a signature hash key ####
    signature_hashkey <- digest::digest(paste0(metadata_tbl$signature_name, metadata_tbl$organism_id, metadata_tbl$direction_type, metadata_tbl$assay_type, metadata_tbl$phenotype_id, user_name), algo = "md5", serialize = FALSE)
    
    # Add additional variables in signature metadata table ###
    metadata_tbl <- metadata_tbl %>% 
      dplyr::mutate(
        signature_id = signature_tbl$signature_id,
        user_name = user_name,
        signature_hashkey = signature_hashkey
      )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = "date_created",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      table = metadata_tbl,
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
      base::saveRDS(difexp, file = file.path(data_path, paste0(signature_hashkey, ".RDS")))
    }
    
    # 3. Adding user to signature access table after signature was imported successfully in step (2) ####

    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addUserToSignature(
        conn_handler = conn_handler,
        signature_id = signature_tbl$signature_id,
        user_name = user_name,
        access_type = signature_access_tbl$access_type
      )
    }, error = function(e){
      # Delete signature
      SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id)
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }, warning = function(w){
      base::message(w, "\n")
    }) 
    
    # 4. Importing signature set into database after signature was imported successfully in step (2) ####

    # Get the signature assay type
    assay_type <- signature_tbl$assay_type
    
    # Add signature set to database based on assay types
    if(assay_type == "transcriptomics"){
      
      # If there is a error during the process, remove the signature and output the message
      base::tryCatch({
        SigRepo::addTranscriptomicsSignatureSet(
          conn_handler = conn_handler,
          signature_id = signature_tbl$signature_id,
          organism_id = signature_tbl$organism_id,
          signature_set = omic_signature$signature
        )
      }, error = function(e){
        # Delete signature
        SigRepo::deleteSignature(conn_handler = conn_handler, signature_id = signature_tbl$signature_id)
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(e, "\n")
      }, warning = function(w){
        base::message(w, "\n")
      }) 
      
    }else if(assay_type == "proteomics"){
      
    }else if(assay_type == "metabolomics"){
      
    }else if(assay_type == "methylomics"){
      
    }else if(assay_type == "genetic_variations"){
      
    }else if(assay_type == "dna_binding_sites"){
      
    }
    
    # Return message
    base::message(sprintf("signature_id = '%s' has been updated.", signature_id))
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))  
    
  }
}

  

