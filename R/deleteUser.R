#' @title deleteUser
#' @description Delete user information in the database
#' @param conn_handler An established database connection using SigRepo::newConnhandler() 
#' @param user_name Name of a user to be deleted (required).
#' @export
deleteUser <- function(
    conn_handler,
    user_name
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Check user_name ####
  if(!length(user_name) == 1 || all(user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("'user_name' must have a length of 1 and cannot be empty.")
  }
  
  # Create a list of variables to check database ####
  db_table_name <- "users"
  
  # Get user table
  table <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "user_name", 
    filter_coln_val = list("user_name" = user_name),
    check_db_table = FALSE
  )

  # Check if user exists in the database
  if(nrow(table) == 0){
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(sprintf("There is no user = '%s' existed in the 'users' table of the SigRepo Database.", user_name))
  }
  
  # Remove user from user table 
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # DROP USER FROM DATABASE
  purrr::walk(
    base::seq_len(nrow(table)),
    function(u){
      #u=1;
      # CHECK IF USER EXIST IN DATABASE
      check_user_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_name[u])))
      
      # CREATE USER IF NOT EXIST
      if(nrow(check_user_tbl) == 0){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("DROP USER '%s'@'%%';", table$user_name[u])))
      }
    }
  )

  # Remove user from signature table
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = "signatures", 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # Remove user from signature access table
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = "signature_access", 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # Remove user from collection table
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = "collection", 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # Remove user from collection access table
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = "collection_access", 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn)) 
  
}



