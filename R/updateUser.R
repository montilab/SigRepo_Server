#' @title updateUser
#' @description Update user information in the database
#' @param conn_handler An established database connection using SigRepo::newConnhandler() 
#' @param user_name A data frame containing appropriate column names:
#' user_name, user_password, user_email, user_first, user_last, user_affiliation, user_role
#' @export
updateUser <- function(
    conn_handler,
    user_name,
    user_email = NULL,
    user_affiliation = NULL,
    user_first = NULL,
    user_last = NULL,
    user_role = c("admin", "editor", "viewer")
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Check user_name ####
  stopifnot("'user_name' cannot be empty." = (length(user_name) == 1 && !user_name %in% c(NA, "")))
  
  # Create a list of variables to check database ####
  db_table_name <- "users"
  table <- user_tbl
  
  # Create a hash key for user password
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "user_password_hashkey",
    hash_columns = "user_password",
    hash_method = "sodium"
  )
  
  # Create an api key for each user
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "api_key",
    hash_columns = c("user_name", "user_password", "user_email"),
    hash_method = "md5"
  )
  
  # Create a hash key to check for duplicates
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "user_hashkey",
    hash_columns = "user_name",
    hash_method = "md5"
  )
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn_info$conn,
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = NULL,
    check_db_table = TRUE
  )
  
  # Check for duplicated emails ####
  table <- SigRepo::removeDuplicates(
    conn = conn_info$conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_email",
    check_db_table = FALSE
  )
  
  # Remove duplicated hash keys from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn_info$conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_hashkey",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn_info$conn,
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
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



