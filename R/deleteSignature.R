#' @title deleteSignature
#' @description Delete a signature from the signature table of the database
#' @param conn An established connection to the database using newConnhandler() 
#' @param omic_signature The name of the signature being deleted from the database 
#' @export
#' ex deleteSignature(conn, 'LLFS_Aging_Gene_2023' )
deleteSignature <- function(conn, omic_signature) {
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "UPDATE",
    required_role = "editor"
  )
  
  # Database name for signature table
  db_table_name <- "signatures"
  
  # grabbing signatures in database
  db_signatures <- DBI::dbGetQuery(conn = conn, 
                                   statement = sprintf("SELECT signature_name FROM %s", 
                                                       db_table_name))
  
  
  # Verify if the signature_name exists in the database
  if (!(omic_signature %in% db_signatures$signature_name)) {
    stop("The signature provided is not a valid signature in the database.")
  }
  
  # Retrieve additional information about the signature
  signature_id <- DBI::dbGetQuery(conn = conn, 
                                  statement = sprintf("SELECT signature_id FROM %s
                                                      WHERE signature_name = '%s'", 
                                                      db_table_name, signature_name))
  
  # Retrieve dif_exp column 
  dif_exp <- DBI::dbGetQuery(conn = conn, 
                             statement = sprintf("SELECT has_difexp FROM %s
                                                 WHERE signature_name = '%s'", 
                                                 db_table_name, signature_name))
  
  signature_hashkey <- DBI::dbGetQuery(conn = conn, 
                                       statement = sprintf("SELECT signature_hashkey FROM %s WHERE signature_name = '%s'", 
                                                           db_table_name, signature_name))
  
  # If differential expression exists, delete associated RDS file
  if (dif_exp$has_difexp == TRUE) {
    data_path <- system.file("data/difexp", package = "SigRepo")
    file_path <- sprintf("%s/%s.RDS", data_path, signature_hashkey$signature_hashkey)
    if (file.exists(file_path)) {
      unlink(file_path)
    }
  }
  
  # Delete the signature from the database
  DBI::dbExecute(conn = conn, 
                 statement = sprintf("DELETE FROM %s
                                     WHERE signature_name = '%s'", 
                                     db_table_name, signature_name))
  

  db_table_name_two <- "signature_feature_set"
  
 # Delete Signature Feature Set, 
  
  DBI::dbExecute(conn = conn,
                 statement = sprintf("DELETE FROM %s 
                                     WHERE sig_feature_hashkey = %s",
                                     db_table_name_two, 
                                     signature_hashkey$signature_hashkey))
  
 # Update user access 
  
  db_table_name_three <- "sinature_access"
    
  DBI::dbExecute(conn = conn,
                 statement = springf("DELETE FROM %s WHERE signature_id = %s", db_table_name_three, signature_id ))
  

  
  # Optionally disconnect the database (if you want this function to handle it)
  DBI::dbDisconnect(conn)
  
  message(sprintf("Signature '%s' and associated data have been deleted from the SigRepo Database.", signature_name))
}


  
  
  

