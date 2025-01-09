#' @title getSampleTypes
#' @description Get signatures to database
#' @param conn An established connection to database using newConnhandler() 
#' @param filter_by author id used to submit the signature
#' @export
getSampleTypes <- function(
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
      filter_coln_val = list("sample_type" = filter_by),
      check_db_table = TRUE
    ) 
    
  }
  
  return(sample_type_tbl)


  # close connection

  DBI::dbDisconnect(conn_info$conn)

}







