#' @title addUser
#' @description Add user information to database
#' @param conn An established database connection using newConnhandler() 
#' @param user_tbl A data frame containing appropriate column names:
#' user_id, user_password, user_email, user_first, user_last, user_affiliation, user_role
#' @export
addUser <- function(
    conn,
    user_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "users"
  table <- user_tbl
  
  # Create a hash key for user password
  table <- SigRepoR::createHashKey(
    table = table,
    hash_var = "user_password_hashkey",
    hash_columns = "user_password",
    hash_method = "sodium"
  )
  
  # Create an api key for each user
  table <- SigRepoR::createHashKey(
    table = table,
    hash_var = "api_key",
    hash_columns = c("user_id", "user_password", "user_email", "user_role"),
    hash_method = "md5"
  )
  
  # Create a hash key to check for duplicates
  table <- SigRepoR::createHashKey(
    table = table,
    hash_var = "user_hashkey",
    hash_columns = c("user_id", "user_email", "user_role"),
    hash_method = "md5"
  )
  
  # Check table against database table ####
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = NULL,
    check_db_table = FALSE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepoR::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_hashkey",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepoR::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
  # IF USER IS NOT ROOT, GRANT USER PERMISSIONS TO DATABASE IF IT DOES NOT EXIST
  purrr::walk(
    seq_len(nrow(table)),
    function(u){
      #u=1;
      if(table$user_id[u] != "root"){    
        
        check_user_tbl <- suppressWarnings(
          DBI::dbGetQuery(conn = conn, statement = sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_id[u]))
        )
        
        # Create user if not existed in database
        if(nrow(check_user_tbl) == 0){
          suppressWarnings(
            DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", table$user_id[u], table$user_password[u]))
          )
        }
        
        # Give permissions to users based on their user role
        if(table$user_role[u] == "admin"){
          suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT CREATE, ALTER, DROP, SELECT, INSERT, UPDATE, DELETE, SHOW DATABASES, CREATE USER ON *.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_id[u])))
          suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))
        }else if(table$user_role[u] == "editor"){
          suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT, INSERT, UPDATE, DELETE, SHOW DATABASES ON *.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_id[u])))
          suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
        }else if(table$user_role[u] == "viewer"){
          suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT, SHOW DATABASES ON *.* TO '%s'@'%%';", table$user_id[u])))
          suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
        }
      }
      
    }
  )
}



