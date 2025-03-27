#' @title searchUser
#' @description Get phenotypes in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param user_name A list of user names to search by. Default is NULL which
#' includes all users in the database
#' @param verbose A logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
searchUser <- function(
    conn_handler,
    user_name = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Look up signatures
  if(length(user_name) == 0 || all(user_name %in% c("", NA))){
    
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = c("user_name", "user_first", "user_last", "user_affiliation", "user_role", "user_email"), 
      check_db_table = TRUE
    )  
    
  }else{
    
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = c("user_name", "user_first", "user_last", "user_affiliation", "user_role", "user_email"), 
      filter_coln_var = "user_name", 
      filter_coln_val = list("user_name" = user_name),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return tabl
  return(user_tbl)

}







