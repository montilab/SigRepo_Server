#' @title getPlatforms
#' @description Get signatures to database
#' @param conn An established connection to database using newConnhandler() 
#' @param filter_by author id used to submit the signature
#' @export
getPlatforms <- function(
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
      filter_coln_val = list("platform_id" = filter_by),
      check_db_table = TRUE
    ) 
    
  }
  
  return(platform_tbl)

}







