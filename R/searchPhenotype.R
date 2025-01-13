#' @title searchPhenotype
#' @description Get phenotypes in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param phenotypes a list of phenotypes to search by. Default is NULL which
#' includes all phenotypes in the database
#' @export
searchPhenotype <- function(
    conn_handler,
    phenotype = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # Look up signatures
  if(length(phenotype) == 0 || all(phenotype %in% c("", NA))){
    
    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "phenotypes", 
      return_var = "phenotype", 
      check_db_table = TRUE
    )  
    
  }else{
    
    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "phenotypes", 
      return_var = "phenotype", 
      filter_coln_var = "phenotype", 
      filter_coln_val = list("phenotype" = phenotype),
      check_db_table = TRUE
    ) 
    
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
  
  # Return tabl
  return(phenotype_tbl)

}







