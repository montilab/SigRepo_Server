#' @title updateUser
#' @description Update a user information in the database
#' @param conn_handler An established database connection using SigRepo::newConnhandler() 
#' @param user_name Name of a user to be updated (required).
#' @param password Password of a user to be updated. Default is NULL.
#' @param email Email of a user to be updated. Default is NULL.
#' @param affiliation First name of a user to be updated. Default is NULL.
#' @param last_name Last name of a user to be updated. Default is NULL.
#' @param role Role of a user to be updated. Choices are admin/editor/viewer.
#' 
#' @export
updateUser <- function(
    conn_handler,
    user_name,
    password = NULL,
    email = NULL,
    affiliation = NULL,
    first_name = NULL,
    last_name = NULL,
    role = NULL
){
  
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
    base::suppressMessages(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("'user_name' must have a length of 1 and cannot be empty.")
  }
  
  # Check role ####
  if(length(role[1]) > 0 && any(!role[1] %in% c("admin", "editor", "viewer"))){
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))     
    # Show message
    base::stop("'role' must have a length of 1 and have one of the three roles: admin/editor/viewer.")
  }
  
  # Check email ####
  if(length(email[1]) > 0){
    # Check email format ####
    check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(email[1]), ignore.case = TRUE)
    
    # If any emails do not have correct format, throw an error message
    if(any(check_email == FALSE)){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop("Invalid email format.\n")
    }
    
    # Get email table
    email_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      return_var = "*",
      filter_coln_var = "user_email", 
      filter_coln_val = list("user_email" = email[1]),
      check_db_table = FALSE
    ) 
    
    # Check if email not belongs to user
    if(nrow(email_tbl) > 0 && !trimws(tolower(user_name[1])) %in% trimws(tolower(email_tbl$user_name))){
      # Disconnect from database ####
      base::suppressMessages(DBI::dbDisconnect(conn))     
      # Show message
      base::stop("Someone else with email = '%s' is already existed in the database. Please try another email.")
    }
  }
  
  # Get table name ####
  db_table_name <- "users"
  
  # Get user table
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name,
    return_var = "*",
    filter_coln_var = "user_name", 
    filter_coln_val = list("user_name" = user_name),
    check_db_table = FALSE
  )
  
  # If user does not have permission, throw an error message
  if(nrow(user_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(sprintf("Cannot update user = '%s' as user does not exist in the 'users' table of the database.", user_name))
    
  }
  
  # Remove user from database
  SigRepo::delete_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    delete_coln_var = "user_name", 
    delete_coln_val = user_name,
    check_db_table = FALSE
  )
  
  # Create updated variables
  table <- user_tbl %>% 
    dplyr::mutate(
      user_password_hashkey = ifelse(length(password[1]) == 0 || all(password[1] %in% c("", NA)), user_password_hashkey, sodium::password_store(password[1])),
      user_email = ifelse(length(email[1]) == 0 || all(email[1] %in% c("", NA)), user_email, email[1]),
      user_first = ifelse(length(first_name[1]) == 0 || all(first_name[1] %in% c("", NA)), user_first, first_name[1]),
      user_last = ifelse(length(last_name[1]) == 0 || all(last_name[1] %in% c("", NA)), user_last, last_name[1]),
      user_affiliation = ifelse(length(affiliation[1]) == 0 || all(affiliation[1] %in% c("", NA)), user_affiliation, affiliation[1]),
      user_role = ifelse(length(role[1]) == 0 || all(role[1] %in% c("", NA)), user_role, role[1])
    )
  
  # Create an api key 
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "api_key",
    hash_columns = c("user_name", "user_email", "user_role"),
    hash_method = "md5"
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
    seq_len(nrow(table)),
    function(u){
      #u=1;
      # CHECK IF USER EXIST IN DATABASE
      check_user_tbl <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("SELECT host, user FROM mysql.user WHERE user = '%s' AND host = '%%';", table$user_name[u])))
      
      # CREATE USER IF NOT EXIST
      if(nrow(check_user_tbl) == 0){
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'%%' IDENTIFIED BY '%s';", table$user_name[u], table$user_password[u])))
      }
      
      # GRANT USER PERMISSIONS TO DATABASE BASED ON THEIR ROLES
      if(table$user_role[u] == "admin"){
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT CREATE, ALTER, DROP, SELECT, INSERT, UPDATE, DELETE, SHOW DATABASES, CREATE USER ON *.* TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))
      }else if(table$user_role[u] == "editor"){
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT, SHOW DATABASES ON *.* TO '%s'@'%%';", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT INSERT, UPDATE, DELETE ON sigrepo.`signatures` TO '%s'@'%%' WITH GRANT OPTION;", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
      }else if(table$user_role[u] == "viewer"){
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT, SHOW DATABASES ON *.* TO '%s'@'%%';", table$user_name[u])))
        suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;"))        
      }
    }
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn)) 
  
}



