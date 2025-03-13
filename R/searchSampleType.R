#' @title searchSampleType
#' @description Get sample types in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param sample_type a list of sample types to search by. Default is NULL which
#' includes all sample types in the database.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#'
#' @export
searchSampleType <- function(
    conn_handler,
    sample_type = NULL,
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
  if(length(sample_type) == 0 || all(sample_type %in% c("", NA))){
    
    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = "*", 
      filter_coln_var = "sample_type", 
      filter_coln_val = list("sample_type" = sample_type),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(sample_type_tbl)

}







