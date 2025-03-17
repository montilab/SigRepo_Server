#' @title addUser
#' @description Add user information to database
#' @param conn_handler An established database connection using SigRepo::newConnhandler() 
#' @param user_tbl A data frame containing appropriate column names:
#' user_name, user_password, user_email, user_first, user_last, user_affiliation, user_role
#' #' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
addUser <- function(
    conn_handler,
    user_tbl,
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
  
  # Create a list of variables to check database ####
  required_column_fields <- c("user_name", "user_password", "user_email", "user_role")
  user_role_options <- c("admin", "editor", "viewer")
  db_table_name <- "users"
  table <- user_tbl
  
  # Check required column fields
  if(any(!required_column_fields %in% colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("The table is missing the following required column names: %s.\n", base::paste0(required_column_fields[which(!required_column_fields %in% colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(any(is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("All required column names: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
  }
  
  # Check user roles ####
  if(any(!table$user_role %in% user_role_options)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("The 'user_role' column must contain one of the following roles: %s.\n", base::paste0(user_role_options, collapse = "/")))
  }
  
  # Check user emails ####
  check_emails <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(table$user_email), ignore.case = TRUE)
 
  # If any emails do not have correct format, throw an error message
  if(any(check_emails == FALSE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("Invalid email format.\n")
  }
  
  # Check for duplicated emails ####
  SigRepo::checkDuplicatedEmails(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_email",
    check_db_table = FALSE
  )
  
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
    hash_columns = c("user_name", "user_email", "user_role"),
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
    conn = conn,
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = NULL,
    check_db_table = FALSE
  )
  
  # Remove duplicated hash keys ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "user_name",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
  # IF USER IS NOT ROOT AND NOT EXIST IN DATABASE, CREATE USER AND GRANT USER PERMISSIONS TO DATABASE
  purrr::walk(
    base::seq_len(nrow(table)),
    function(u){
      #u=1;
      # CHECK IF USER EXIST IN DATABASE
      check_user_tbl <- base::suppressWarnings(
        DBI::dbGetQuery(conn = conn, statement = sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_name[u]))
      )
      
      # CREATE USER IF NOT EXIST
      if(nrow(check_user_tbl) == 0){
        base::suppressWarnings(
          DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", table$user_name[u], table$user_password[u]))
        )
      }
      
      # GRANT USER PERMISSIONS TO DATABASE BASED ON THEIR ROLES
      if(table$user_role[u] == "admin"){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT CREATE, ALTER, DROP, SELECT, INSERT, UPDATE, DELETE, SHOW DATABASES, CREATE USER ON *.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))
      }else if(table$user_role[u] == "editor"){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT, SHOW DATABASES ON *.* TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT INSERT, UPDATE, DELETE ON `sigrepo`.`signatures` TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
      }else if(table$user_role[u] == "viewer"){
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT, SHOW DATABASES ON *.* TO '%s'@'%%';", table$user_name[u])))
        base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
      }
    }
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return message
  SigRepo::verbose("Finished uploading.\n")
  
}



