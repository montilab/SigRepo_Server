#' @title addSignatureAccess
#' @description Add signature access information to database
#' @param conn An established database connection using newConnhandler() 
#' @param access_tbl A data frame containing the appropriate column names:
#' signature_id, user_name, access_type
#' @export
addSignatureAccess <- function(
    conn,
    access_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )

  # Get table name in database
  db_table_name <- "signature_access" 
  table <- access_tbl
  
  # Create a hash key to look up values in database ####
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "access_sig_hashkey",
    hash_columns = c("signature_id", "user_name", "access_type"),
    hash_method = "md5"
  )

  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = NULL,
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "access_sig_hashkey",
    check_db_table = FALSE
  )

  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  

  DBI:dbDisconnect(conn_info$conn)
  
}



