#' @title searchOrganism
#' @description Get organisms in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param organism a list of organisms to search by. Default is NULL which
#' includes all organisms in the database.
#' @export
searchOrganism <- function(
    conn_handler,
    organism = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # Look up signatures
  if(length(organism) == 0 || all(organism %in% c("", NA))){
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "organisms", 
      return_var = "organism", 
      check_db_table = TRUE
    )  
    
  }else{
    
    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "organisms", 
      return_var = "organism", 
      filter_coln_var = "organism", 
      filter_coln_val = list("organism" = organism),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
  
  # Return table
  return(organism_tbl)

}







