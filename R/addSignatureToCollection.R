#' @title addSignatureToCollection
#' @description Add a list of signatures to a collection in the database
#' @param conn_handler A handler uses to establish connection to  
#' a remote database obtained from SigRepo::newConnhandler() 
#' @param collection_id ID of collection in the database
#' @param signature_id A list of signature IDs to be added to a collection 
#' in the database
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
addSignatureToCollection <- function(
    conn_handler,
    collection_id,
    signature_id,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
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
  
  # Check collection_id
  if(!length(collection_id) == 1 || all(collection_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'collection_id' must have a length of 1 and cannot be empty.")
  }
  
  # Check signature_id
  if(length(signature_id) == 0 || all(signature_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'signature_id' cannot be empty.")
  }
  
  # Check if signature exists in the signatures table of the database
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = "signature_id", 
    filter_coln_val = list("signature_id" = signature_id),
    check_db_table = FALSE
  )
  
  # If any signatures does not exit in the database, throw an error message
  if(nrow(signature_tbl) != length(unique(signature_id))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("signature_id = %s does not exist in the signatures table of the SigRepo Database.", base::paste0("'", signature_id[which(!signature_id %in% signature_tbl$signature_id)], "'", collapse = ", ")))
  }
  
  # If user is not admin, check if user has access to the signature as owner or editor
  if(user_role != "admin"){
    
    # Check if user is the one who uploaded the signature
    signature_user_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = c("signature_id", "user_name"), 
      filter_coln_val = list("signature_id" = signature_id, "user_name" = user_name),
      filter_var_by = "AND",
      check_db_table = FALSE
    )
    
    # If not, check if user was added as an owner or editor
    if(nrow(signature_user_tbl) == 0 || (nrow(signature_user_tbl) != length(unique(signature_id)))){
      
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
      if(nrow(signature_access_tbl) == 0 || (nrow(signature_access_tbl) != length(unique(signature_id)))){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn)) 
        # Show message
        base::stop(base::sprintf("User = '%s' does not have the permission to add signature_id = % to collection_id = '%s' in the database.", user_name, base::paste0("'", signature_id[which(!signature_id %in% signature_access_tbl$signature_id)], "'", collapse = ", "), collection_id))
      }
    }
  }
  
  # Check if collection exists ####
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "collection",
    return_var = "*",
    filter_coln_var = "collection_id",
    filter_coln_val = list("collection_id" = collection_id),
    check_db_table = TRUE
  )
  
  # If collection not exists, throw an error message
  if(nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("There is no collection_id = '%s' in the 'collection' table of the SigRepo Database.", collection_id))
    
  }else{
    
    # If user is not admin, check if user has access to the collection as owner or editor
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the collection
      collection_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "collection",
        return_var = "*",
        filter_coln_var = c("collection_id", "user_name"), 
        filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(nrow(collection_user_tbl) == 0){
        
        signature_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "collection_access",
          return_var = "*",
          filter_coln_var = c("collection_id", "user_name", "access_type"),
          filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name, "access_type" = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(nrow(signature_access_tbl) == 0){
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          # Show message
          base::stop(sprintf("User = '%s' does not have permission to add signature_id = % to collection_id = '%s' in the database.", user_name, paste0("'", signature_id, "'", collapse = ", "), collection_id))
        }
      }
    }
    
    # Create user collection access table
    table <- base::data.frame(
      collection_id = collection_id,
      signature_id = signature_id,
      stringsAsFactors = FALSE
    )
    
    # Create a hash key to look up values in database ####
    table <- SigRepo::createHashKey(
      table = table,
      hash_var = "signature_collection_hashkey",
      hash_columns = c("collection_id", "signature_id"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = 'signature_collection_access',
      table = table, 
      check_db_table = TRUE
    )
    
    # Remove duplicates from table before inserting into database ####
    table <- SigRepo::removeDuplicates(
      conn = conn,
      db_table_name = "signature_collection_access",
      table = table,
      coln_var = "signature_collection_hashkey",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = "signature_collection_access", 
      table = table,
      check_db_table = FALSE
    )

    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 

  }  
}  


