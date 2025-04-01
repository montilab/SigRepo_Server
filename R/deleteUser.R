#' @title deleteUser
#' @description Delete user information in the database
#' @param conn_handler An established database connection using SigRepo::newConnhandler() 
#' @param user_name Name of a user to be deleted (required).
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#'
#' @export
deleteUser <- function(
    conn_handler,
    user_name,
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
    required_role = "admin"
  )
  
  # Check user_name ####
  if(!length(user_name) == 1 || all(user_name %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("\n'user_name' must have a length of 1 and cannot be empty.\n")
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
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop(base::sprintf("\nThere is no user = '%s' existed in the 'users' table of the SigRepo database.\n", user_name))
  }
  
  # DROP USER FROM DATABASE
  purrr::walk(
    base::seq_len(nrow(table)),
    function(u){
      #u=1;
      # CHECK IF USER EXIST IN DATABASE
      check_user_tbl <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_name[u])))
      # CREATE USER IF NOT EXIST
      if(nrow(check_user_tbl) == 0){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = base::sprintf("DROP USER '%s'@'%%';", table$user_name[u])))
      }
    }
  )

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return message
  SigRepo::verbose(base::sprintf("user_name = '%s' has been removed.", user_name))
  
}



