#' @title searchPlatform
#' @description Get signatures to database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param platform_id a list of platfrom ids (platform names) in the database
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
searchPlatform <- function(
    conn_handler,
    platform_id = NULL,
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
    required_role = "viewer"
  )
  
  # Look up signatures
  if(length(platform_id) == 0 || all(platform_id %in% c("", NA))){
    
    platform_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "platforms", 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    platform_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "platforms", 
      return_var = "*", 
      filter_coln_var = "platform_id", 
      filter_coln_val = list("platform_id" = platform_id),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(platform_tbl)

}







