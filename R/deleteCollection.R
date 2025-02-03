#' @title deleteCollection
#' @description Delete a signature from the signature table of the database
#' @param conn_handler An established connection to the database using newConnhandler() 
#' @param collection_id The name of the signature being deleted from the database 
#' @export
deleteCollection <- function(
    conn_handler, 
    collection_id
){
  
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
  
  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Check collection_id
  if(!length(collection_id) == 1 || collection_id %in% c(NA, "")){
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'collection_id' must have a length of 1 and cannot be empty.")
  }
  
  # Check if signature exists ####
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "collection_id",
    filter_coln_val = list("collection_id" = collection_id),
    check_db_table = TRUE
  )
  
  # If signature exists, return the signature table else throw an error message
  if(nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(sprintf("There is no collection_id = '%s' existed in the 'signatures' table of the SigRepo Database.", collection_id))
    
  }else{
    
    # If user is not admin, check if it has access to signature
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the signature
      signature_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = db_table_name,
        return_var = "*",
        filter_coln_var = c("collection_id", "user_name"), 
        filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(nrow(signature_tbl) == 0){
        
        signature_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "signature_access",
          return_var = "*",
          filter_coln_var = c("collection_id", "user_name", "access_type"),
          filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name, access_type = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user has access, get the signature metadata table 
        if(nrow(signature_access_tbl) > 0){
          
          signature_tbl <- SigRepo::lookup_table_sql(
            conn = conn,
            db_table_name = db_table_name,
            return_var = "*",
            filter_coln_var = "collection_id",
            filter_coln_val = list("collection_id" = signature_access_tbl$collection_id),
            check_db_table = FALSE
          )
          
        }else{
          
          # Disconnect from database ####
          base::suppressMessages(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(sprintf("User = '%s' does not have permission to delete collection_id = '%s' from the database.", user_name, collection_id))
          
        }
      }
    }
    
    # Check if signature has difexp, remove it
    if(signature_tbl$has_difexp[1] == 1){
      data_path <- base::system.file("inst/data/difexp", package = "SigRepo")
      file_path <- base::file.path(data_path, paste0(signature_tbl$signature_hashkey[1], ".RDS"))
      
      if(base::file.exists(file_path)) {
        base::unlink(file_path)
      }
    }
    
    # Return message
    base::message(sprintf("Remove collection_id = '%s' from 'signatures' table in the database.", collection_id))
    
    # Delete signature from signatures table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      delete_coln_var = "collection_id",
      delete_coln_val = collection_id,
      check_db_table = FALSE
    )
    
    # Return message
    base::message(sprintf("Remove features belongs to collection_id = '%s' from 'signature_feature_set' table of the database.", collection_id))

    # Delete signature from signature_feature_set table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_feature_set",
      delete_coln_var = "collection_id",
      delete_coln_val = collection_id,
      check_db_table = TRUE
    )

    # Return message
    base::message(sprintf("Remove collection_id = '%s' from 'signature_access' table of the database.", collection_id))

    # Delete user from signature_access table in the database ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "signature_access",
      delete_coln_var = "collection_id",
      delete_coln_val = collection_id,
      check_db_table = TRUE
    )
    
    # Return message
    base::message(sprintf("collection_id = '%s' has been deleted.", collection_id))
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn)) 
    
  } 
}








