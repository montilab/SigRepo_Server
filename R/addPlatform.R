#' @title addPlatform
#' @description Add platform to database
#' @param conn An established connection to database using newConnhandler() 
#' @param platform_tbl An data frame containing appropriate column names: 
#' platform_id, platform_name, seq_technology, organisms to be uploaded into 
#' the database.
#' @export
addPlatform <- function(
    conn,
    platform_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "platforms"
  table <- platform_tbl
  
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
    coln_var = "platform_id",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  ) 

# close connection
DBI::dbDisconnect(conn_info$conn)

}



