#' @title getPhenotypes
#' @description Get phenotypes from the database
#' @param conn An established connection to database using newConnhandler() 
#' @param filter_by author id used to submit the signature
#' @export
getPhenotypes <- function(
    conn,
    filter_by = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "guest"
  )
  
  # Look up signatures
  if(length(filter_by) == 0){
    
    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    

    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = "*", 
      filter_coln_var = "phenotype", 
      filter_coln_val = list("phenotype" = filter_by),
      check_db_table = TRUE
    ) 
    
  }
  
  return(phenotype_tbl)

  # close connection

  DBI::dbDisconnect(conn_info$conn)

}







