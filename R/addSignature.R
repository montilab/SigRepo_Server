#' @title addSignature
#' @description Add signature to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param omic_signature An R6 class object from OmicSignature package
#' @export
addSignature <- function(
    conn_handler,
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
  
  # Create signature metadata table ####
  metadata_tbl <- SigRepo::createSignatureMetadata(
    conn_handler = conn_handler, 
    omic_signature = omic_signature
  )
  
  # Create a hash key to look up whether signature is already existed in the database ####
  signature_hashkey <- digest::digest(paste0(metadata_tbl$signature_name, metadata_tbl$organism_id, metadata_tbl$direction_type, metadata_tbl$assay_type, metadata_tbl$phenotype_id, user_name), algo = "md5", serialize = FALSE)
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "signature_hashkey",
    filter_coln_val = list("signature_hashkey" = signature_hashkey),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, throw an error message ####
  if(nrow(signature_tbl) > 0){
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))
    
    # Show message
    base::warning("\tYou already uploaded a signature with similar contents into the SigRepo Database.\n",
                  "\tUse searchSignatures() to see more details about the signature.\n")
    
  }else{
    
    # 1. Uploading signature metadata into database
    base::message("Uploading signature metadata into the database...\n")
    
    # Add additional variables in signature metadata table ###
    metadata_tbl <- metadata_tbl %>% 
      dplyr::mutate(
        user_name = user_name,
        signature_hashkey = signature_hashkey
      )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn, 
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = c("signature_id", "date_created"),
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
    # This helps to make sure data is stored properly if there are interruptions in-between.
    if(metadata_tbl$has_difexp == TRUE){
      # 1.1 Saving signature difexp into database
      base::message("Saving signature difexp into the database...\n")
      # Extract difexp from omic_signature ####
      difexp <- omic_signature$difexp
      # Save difexp to local storage ####
      data_path <- base::system.file("inst/data/difexp", package = "SigRepo")
      base::saveRDS(difexp, file = file.path(data_path, paste0(signature_hashkey, ".RDS")))
    }
    
    # Look up signature id for the next step ####
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      return_var = "*", 
      filter_coln_var = "signature_hashkey",
      filter_coln_val = list("signature_hashkey" = signature_hashkey),
      check_db_table = FALSE
    ) 
    
    # 2. Adding user to signature access table after signature
    # was imported successfully in step (1)
    base::message("Adding user to signature access in the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addUserToSignature(
        conn_handler = conn_handler,
        signature_id = signature_tbl$signature_id,
        user_name = user_name,
        access_type = "owner"
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
    
    # 3. Importing signature set into database after signature
    # was imported successfully in step (1)
    base::message("Adding signature set into the database...\n")
    
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
    base::message("Finished uploading.\n")
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))    
    
  } 
}  



