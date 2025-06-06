# Function to create and return a database connection handler
devtools::load_all()


create_conn_handler <- function() {
  # Ensure necessary environment variables are set
  required_vars <- c("DBNAME", "HOST", "PORT", "USER", "PASSWORD")
  
  missing_vars <- required_vars[!required_vars %in% names(Sys.getenv())]
  if (length(missing_vars) > 0) {
    stop(paste("Missing environment variables:", paste(missing_vars, collapse = ", ")))
  }
  
  # Create and return the connection handler
  conn_handler <- SigRepo::newConnHandler(
    dbname = Sys.getenv("DBNAME"), 
    host = Sys.getenv("HOST"), 
    port = as.integer(Sys.getenv("PORT")), 
    user = Sys.getenv("USER"), 
    password = Sys.getenv("PASSWORD")
  )
  
  return(conn_handler)
}

conn_handler <- create_conn_handler()
