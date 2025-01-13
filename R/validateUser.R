#' @title validateUser
#' @description Validate a user in the database
#' @param conn_handler A handler uses to establish connection to a remote database 
#' obtained from SigRepo::newConnhandler() (required)
#' @export
validateUser <- function(
    conn_handler
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Get user_name ####
  user_name <- conn_info$user[1]   
  
  # Look up user 
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn,
    db_table_name = "users", 
    return_var = c("user_name"),
    filter_coln_var = c("user_name", "user_email", "user_affliation", "user_role", "api_key"),
    filter_coln_val = list("user_name" = user_name),
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
  
  # Return table
  return(user_tbl)
  
}



