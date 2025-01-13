#' @title deleteSignature
#' @description Delete a signature from the signature table of the database
#' @param conn_handler An established connection to the database using newConnhandler() 
#' @param signature_id The name of the signature being deleted from the database 
#' @export
deleteSignature <- function(
    conn_handler, 
    signature_id
){

  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler,
    action_type = "UPDATE",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # Check signature_id ####
  stopifnot("'signature_id' cannot be empty." = (length(signature_id) == 1 && !signature_id %in% c(NA, "")))
 
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
      if(nrow(signature_access_tbl) == 0 || !signature_access_tbl$access_type[1] %in% c("admin", "owner", "editor")){
        base::stop(sprintf("User = '%s' has no permission to delete signature_id = '%s' in the database.\n", user_name, signature_id))
        # Disconnect from database ####
        base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
      }
      
    }
    
    # Check if signature has difexp, if it does, remove it
    if(signature_tbl$has_difexp[1] == 1){
      
      data_path <- base::system.file("inst/data/difexp", package = "SigRepo")
      file_path <- base::file.path(data_path, paste0(signature_tbl$signature_hashkey[1], ".RDS"))
      
      if(base::file.exists(file_path)) {
        base::unlink(file_path)
      }
      
    }
    
    # Delete signature from signatures table in the database ####
    SigRepo::delete_table_sql(
      conn = conn_info$conn,
      db_table_name = "signatures",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = FALSE
    )
    
    # Delete signature from signature_feature_set table in the database ####
    SigRepo::delete_table_sql(
      conn = conn_info$conn,
      db_table_name = "signature_feature_set",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = TRUE
    )
    
    # Delete user from signature_access table in the database ####
    SigRepo::delete_table_sql(
      conn = conn_info$conn,
      db_table_name = "signature_access",
      delete_coln_var = "signature_id",
      delete_coln_val = signature_id,
      check_db_table = TRUE
    )  
    
    # Return message
    base::message(sprintf("signature_id = '%s' has been deleted.", signature_id))
    
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
  
}








