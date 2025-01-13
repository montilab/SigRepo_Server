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
#' signatures <- sigRepo::getSignatures(
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
  
  # Check user connection and permissions
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler,
    action_type = "UPDATE",
    required_role = "editor"
  )

  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Check signature_id ####
  base::stopifnot("'signature_id' cannot be empty." = (length(signature_id) == 1 && !signature_id %in% c(NA, "")))
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn,
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "signature_id",
    filter_coln_val = list("signature_id" = signature_id),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, throw an error message ####
  if(nrow(signature_tbl) == 0){
    
    base::stop(sprintf("There is no signature_id = '%s' existed in the 'signatures' table of the SigRepo Database.\n", signature_id))
    
  }else{
    
    # If user_role is not admin, check user access to the signature ####
    if(user_role != "admin"){
      
      # Check user access ####
      signature_access_tbl <- SigRepo::lookup_table_sql(
        conn = conn_info$conn,
        db_table_name = "signature_access", 
        return_var = "*", 
        filter_coln_var = c("signature_id", "user_name"),
        filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
        check_db_table = TRUE
      ) 
      
      # If user does not have owner or editor permission, throw an error message
      if(!signature_access_tbl$access_type[1] %in% c("admin", "owner", "editor"))
        base::stop(sprintf("User = '%s' has no permission to update signature_id = '%s' in the database.\n", user_name, signature_id))
      
    }
    
    # Create signature metadata table ####
    metadata_tbl <- SigRepo::getSignatureMetadata(
      conn = conn_info$conn,
      omic_signature = omic_signature
    )
    
    # Create a signature hash key ####
    signature_hashkey <- digest::digest(paste0(metadata_tbl$signature_name, metadata_tbl$organism_id, metadata_tbl$direction_type, metadata_tbl$assay_type, metadata_tbl$phenotype_id, user_name), algo = "md5", serialize = FALSE)
    
    # Delete the signature from the database ####
    base::suppressMessages(
      SigRepo::deleteSignature(
        conn_handler = conn_handler,
        signature_id = signature_id
      )
    )
    
    # Add additional variables in signature metadata table ###
    metadata_tbl <- metadata_tbl %>% 
      dplyr::mutate(
        signature_id = signature_id,
        user_name = user_name,
        signature_hashkey = signature_hashkey
      )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn_info$conn,
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = "date_created",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn_info$conn,
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
    
    # 2. Importing signature set into database 
    
    # Get the signature assay type
    assay_type <- metadata_tbl$assay_type
    
    # Add signature set to database based on assay types
    if(assay_type == "transcriptomics"){
      
      SigRepo::addTranscriptomicsSignatureSet(
        conn = conn_info$conn,
        signature_id = signature_id,
        organism_id = metadata_tbl$organism_id,
        signature_set = omic_signature$signature
      )
      
    }else if(assay_type == "proteomics"){
      
    }else if(assay_type == "metabolomics"){
      
    }else if(assay_type == "methylomics"){
      
    }else if(assay_type == "genetic_variations"){
      
    }else if(assay_type == "dna_binding_sites"){
      
    }
    
    # 3. Adding user to signature access table after signature
    # was imported successfully in step (1)
    SigRepo::addUserToSignature(
      conn = conn_info$conn,
      signature_id = signature_id,
      user_name = user_name,
      access_type = ifelse(user_role %in% "admin", "admin", "owner")
    )
    
    # Return message
    base::message(sprintf("signature_id = '%s' has been updated.", signature_id))
    
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
  
}

  

