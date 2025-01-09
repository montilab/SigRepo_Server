#' @title lookupUser
#' @description lookup a user in the database
#' @param conn An established database connection using newConnhandler() 
#' @param username A data frame containing appropriate column names
#' @param password A data frame containing appropriate column names
#' @param api_key A data frame containing appropriate column names
#' @export
#' @import sodium
lookupUser <- function(
    conn,
    username, 
    password,
    api_key = TRUE
){
  
  # Table name in database
  table <- "users"
  
  # Check if table exists in database
  checkTable(conn = conn, table = table)
  
  # Check username
  if(length(username) == 0)
    stop("'username' cannot be empty")

  # Check password
  if(length(password) == 0)
    stop("'password' cannot be empty")
  
  # Lookup user in the database
  user_tbl <- SigRepoR::lookup_multiple_id_sql(
    conn = conn, 
    table = table, 
    id_var = c("user_id", "user_password_hashkey"), 
    coln_var = c("user_id"), 
    coln_val = c(username)
  )
  
  # Get the hash password
  hash_password <- user_tbl$user_password_hashkey
  
  # Check password
  password_match <- sodium::password_verify(hash_password, password)
  
  # Return the status of the match
  return(password_match)

  # close connection

  DBI::dbDisconnect(conn_info$conn)
  
}



