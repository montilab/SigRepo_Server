#' @title getAPIKey
#' @description Add an API Key for one or more users in the database
#' @param conn_handler An established database connection using newConnhandler() 
#' @export
getAPIKey <- function(
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

  # Look up api key
  api_key_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_name", "api_key"), 
    filter_coln_var = "user_name", 
    filter_coln_val = list("user_name" = user), 
    check_db_table = TRUE
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return table
  return(api_key_tbl)
  
}



