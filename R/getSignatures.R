#' @title getSignatures
#' @description Get signatures to database
#' @param conn An established connection to database using newConnhandler() 
#' @param author_id An author id used to submit the signature
#' @export
getSignatures <- function(
    conn,
    author_id = "all"
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "guest"
  )
  
  # Check author_id
  stopifnot("'author_id' cannot be empty." = 
              (length(author_id) > 0 && all(!author_id %in% c(NA, ""))))
  
  # Look up signatures
  if("all" %in% author_id){
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = "user_id", 
      filter_coln_val = list("user_id" = author_id),
      check_db_table = TRUE
    ) 
    
  }
  
  return(signature_tbl)

}







