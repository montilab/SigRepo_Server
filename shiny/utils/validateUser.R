validateUser <- function(
    conn_handler
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Get user_name ####
  user_name <- conn_info$user[1]   
  
  # Look up user 
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "users", 
    return_var = "*",
    filter_coln_var = "user_name",
    filter_coln_val = base::list("user_name" = user_name),
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(user_tbl)
  
}
