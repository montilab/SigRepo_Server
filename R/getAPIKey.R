#' @title getAPIKey
#' @description Add an API Key for one or more users in the database
#' @param conn_handler An established database connection using newConnhandler() 
#' @export
getAPIKey <- function(
    conn_handler
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "editor"
  )

  # Look up api key
  api_key_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn, 
    db_table_name = "users", 
    return_var = c("user_name", "api_key"), 
    filter_coln_var = "user_name", 
    filter_coln_val = list("user_name" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
  
  # Return table
  return(api_key_tbl)
  
}



