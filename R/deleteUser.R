#' @title deleteUser
#' @description Delete user information in the database
#' @param conn_handler An established database connection using SigRepo::newConnhandler() 
#' @param user_name A data frame containing appropriate column names:
#' user_name, user_password, user_email, user_first, user_last, user_affiliation, user_role
#' @export
deleteUser <- function(
    conn_handler,
    user_name
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "users"

  # IF USER IS NOT ROOT AND NOT EXIST IN DATABASE, CREATE USER AND GRANT USER PERMISSIONS TO DATABASE
  purrr::walk(
    seq_len(nrow(table)),
    function(u){
      #u=1;
      # CHECK IF USER EXIST IN DATABASE
      check_user_tbl <- suppressWarnings(
        DBI::dbGetQuery(conn = conn_info$conn, statement = sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_name[u]))
      )
      
      # CREATE USER IF NOT EXIST
      if(nrow(check_user_tbl) == 0){
        suppressWarnings(
          DBI::dbGetQuery(conn = conn_info$conn, statement = sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", table$user_name[u], table$user_password[u]))
        )
      }
      
      # GRANT USER PERMISSIONS TO DATABASE BASED ON THEIR ROLES
      if(table$user_role[u] == "admin"){
        suppressWarnings(DBI::dbGetQuery(conn = conn_info$conn, statement = sprintf("GRANT CREATE, ALTER, DROP, SELECT, INSERT, UPDATE, DELETE, SHOW DATABASES, CREATE USER ON *.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn_info$conn, statement = "FLUSH PRIVILEGES;"))
      }else if(table$user_role[u] == "editor"){
        suppressWarnings(DBI::dbGetQuery(conn = conn_info$conn, statement = sprintf("GRANT SELECT, INSERT, UPDATE, DELETE, SHOW DATABASES ON *.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn_info$conn, statement = "FLUSH PRIVILEGES;"))        
      }else if(table$user_role[u] == "viewer"){
        suppressWarnings(DBI::dbGetQuery(conn = conn_info$conn, statement = sprintf("GRANT SELECT, SHOW DATABASES ON *.* TO '%s'@'%%';", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn_info$conn, statement = "FLUSH PRIVILEGES;"))        
      }
    }
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
  
}



