#' @title getAPIKey
#' @description Add an API Key for one or more users in the database
#' @param conn An established database connection using newConnhandler() 
#' @export
getAPIKey <- function(
    conn
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "user"
  )

  # Look up api key
  api_key_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_id", "api_key"), 
    filter_coln_var = "user_id", 
    filter_coln_val = list("user_id" = conn_info$user), 
    check_db_table = FALSE
  )
  
  # Return api table
  return(api_key_tbl)
  
}



