#' @title addTranscriptomicsSignatureSet
#' @description Add signature feature set to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param signature_id A signature name
#' @param signature_set A data frame containing the appropriate column names:
#' feature_name, probe_id, score, direction
#' 
#' @noRd
#' 
#' @export
addTranscriptomicsSignatureSet <- function(
    conn_handler,
    signature_id,
    signature_set
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # Check signature_id ####
  stopifnot("'signature_id' cannot be empty." = 
              (length(signature_id) == 1 && !signature_id %in% c(NA, "")))
  
  # Define table in database ####
  db_table_name <- "signature_feature_set"
  
  # Define reference table in database
  ref_table <- "transcriptomics_features"
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = "signature_id",
    filter_coln_val = list("signature_id" = signature_id),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(nrow(signature_tbl) == 0){
    
    base::stop(sprintf("There is no signature_id = '%s' existed in the 'signatures' table of the SigRepo Database.\n", signature_id))
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
    
  }else{
    
    # If user_role is not admin, check user access to the signature ####
    if(!user_role %in% "admin"){
      
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
      if(nrow(signature_access_tbl) > 0 && !signature_access_tbl$access_type[1] %in% c("admin", "owner", "editor")){
        base::stop(sprintf("User = '%s' has no permission to add signature feature set for signature_id = '%s' in the database.\n", user_name, signature_id))
        # Disconnect from database ####
        base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
      }
      
    }
    
    # Create signature set table to look up feature id
    table <- signature_set %>% 
      dplyr::mutate(
        signature_id = signature_id,
        organism_id = organism_id,
        assay_type = "transcriptomics"
      ) 
    
    # Create a hash key to look up feature id in the transcriptiomics reference table ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "feature_hashkey",
      hash_columns = c("feature_name", "organism_id"),
      hash_method = "md5"
    )
    
    # Look up feature id by its hash key
    lookup_hashkey <- unique(table$feature_hashkey)
    
    lookup_feature_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = ref_table, 
      return_var = c("feature_id", "feature_name", "organism_id", "feature_hashkey"),
      filter_coln_var = "feature_hashkey",
      filter_coln_val = list("feature_hashkey" = lookup_hashkey),
      check_db_table = TRUE
    )
    
    # If any ID is missing, produce an error message
    if(nrow(lookup_feature_id_tbl) != length(lookup_hashkey)){
      
      SigRepo::addTranscriptomicsErrorMessage(
        db_table_name = ref_table,
        unknown_features = table$feature_name[which(!table$feature_hashkey %in% lookup_feature_id_tbl$feature_hashkey)]
      )
      
      # Disconnect from database ####
      base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
      
    }
    
    # Add feature id to table
    table <- table %>% dplyr::left_join(lookup_feature_id_tbl)
    
    # Create a hash key to look up signature feature set in database ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "sig_feature_hashkey",
      hash_columns = c("signature_id", "feature_id", "assay_type"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn_info$conn,
      db_table_name = db_table_name,
      table = table, 
      check_db_table = TRUE
    )
    
    # Remove duplicates from table before inserting into database ####
    table <- SigRepo::removeDuplicates(
      conn = conn_info$conn,
      db_table_name = db_table_name,
      table = table,
      coln_var = "sig_feature_hashkey",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn_info$conn,
      db_table_name = db_table_name, 
      table = table,
      check_db_table = FALSE
    )  
    
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
  
}

