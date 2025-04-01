#' @title searchOrganism
#' @description Get organisms in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param organism a list of organisms to search by. Default is NULL which
#' includes all organisms in the database.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#'
#' @export
searchOrganism <- function(
    conn_handler,
    organism = NULL,
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
  if(length(organism) == 0 || all(organism %in% c("", NA))){
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = "organism", 
      filter_coln_var = "*", 
      filter_coln_val = list("organism" = organism),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return table
  return(organism_tbl)

}







