#' @title newConnHandler
#' @description Establish a new connection handle to the host
#' @param driver driver of the database. Default is RMySQL::MySQL()
#' @param dbname table schema of which you want the handle to point.
#' @param host host server in which you want to connect
#' @param port host port in which you wish to use
#' @param user the user who is establishing the connection
#' @param password password associated with the user
#' @return a MySQL connection Handle.
#' @export
#' @importFrom DBI dbConnect
#' @importFrom RMySQL MySQL
newConnHandler <- function(
    driver = RMySQL::MySQL(),
    dbname = 'sigrepo', 
    host = "montilab.bu.edu", 
    port = 3306, 
    user = "guest", 
    password = "guest"
){
  
  # Check driver
  if(!is(driver, "MySQLDriver"))
    stop("'driver' must be a mySQL class object from RMySQL package")
  
  # Check dbname
  stopifnot("'dbname' cannot be empty." = 
              (length(dbname) == 1 && !dbname %in% c(NA, "")))
  
  # Check host
  stopifnot("'host' cannot be empty." = 
              (length(host) == 1 && !host %in% c(NA, "")))
  
  # Check port
  stopifnot("'port' cannot be empty and must be a numeric value." = 
              (length(port) == 1 && !as.numeric(port) %in% c(NA, "")))
  
  # Check user
  stopifnot("'user' cannot be empty." = 
              (length(user) == 1 && !user %in% c(NA, "")))
  
  # Check password
  stopifnot("'password' cannot be empty." = 
              (length(password) == 1 && !password %in% c(NA, "")))
  
  # Check connection
  conn <- tryCatch({
    DBI::dbConnect(
      drv = driver,
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
    )
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  # If user is root, validate if root exists in the users table of the database
  if(user == "root"){
    
    # Look up user in the database
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = "user_id", 
      filter_coln_var = "user_id", 
      filter_coln_val = list("user_id" = user), 
      check_db_table = TRUE
    )
    
    # If user has a root access but does not exist in users table of the database
    # Then add root to users table with the default settings below
    if(nrow(user_tbl) == 0){
      
      table <- data.frame(
        user_id = user,
        user_password = password,
        user_email = paste0(user, "@", host), 
        user_first = "", 
        user_last = "", 
        user_affiliation = "",
        user_role = "admin",
        stringsAsFactors = FALSE
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
        hash_columns = c("user_id", "user_password", "user_email", "user_role"),
        hash_method = "md5"
      )
      
      # Create a hash key to check for duplicates
      table <- SigRepo::createHashKey(
        table = table,
        hash_var = "user_hashkey",
        hash_columns = c("user_id", "user_email", "user_role"),
        hash_method = "md5"
      )
      
      # Insert table into database ####
      SigRepo::insert_table_sql(
        conn = conn, 
        db_table_name = "users", 
        table = table,
        check_db_table = FALSE
      )  
      
    }
  }
  
  return(conn)
  
}
