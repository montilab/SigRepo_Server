#' @title newConnHandler
#' @description Establish a new connection handle to the host
#' @importFrom DBI dbConnect
#' @importFrom RMySQL MySQL
#' @param driver driver of the database. Default is RMySQL::MySQL()
#' @param dbname table schema of which you want the handle to point.
#' @param host host server in which you want to connect
#' @param port host port in which you wish to use
#' @param user the user who is establishing the connection
#' @param password password associated with the user
#' @return a MySQL connection Handle.
#' @export
newConnHandler <- function(
    driver = RMySQL::MySQL(),
    dbname = Sys.getenv("DBNAME"), 
    host = Sys.getenv("HOST"), 
    port = as.integer(Sys.getenv("PORT")), 
    user = Sys.getenv("USER"), 
    password = Sys.getenv("PASSWORD")
){
  
  if(!is(driver, "MySQLDriver"))
    stop("'driver' must be a mySQL class object")
  
  stopifnot("'dbname' cannot be empty." = 
              (length(dbname) > 0 && (!dbname %in% c(NA, ""))))

  stopifnot("'host' cannot be empty." = 
              (length(host) > 0 && (!host %in% c(NA, ""))))
  
  stopifnot("'port' cannot be empty and must be a numeric value." = 
              (length(port) > 0 && is.numeric(port) &&
                 (!port %in% c(NA, ""))))

  stopifnot("'user' cannot be empty." = 
              (length(user) > 0 && (!user %in% c(NA, ""))))
  
  stopifnot("'password' cannot be empty." = 
              (length(password) > 0 && (!password %in% c(NA, ""))))

  tryCatch({
    DBI::dbConnect(
      drv = driver,
      dbname = dbname,
      host = host,
      port = port,
      user = user,
      password = password
    )
  }, error = function(e){
    stop(e)
  }, warning = function(w){
    message(w, "\n")
  })
  
}
