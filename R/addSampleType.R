#' @title addSampleType
#' @description Add sample_type to database
#' @param conn An established connection to database using newConnhandler() 
#' @param sample_type_tbl An data frame containing appropriate column names:
#' sample_type, brenda_accession
#' @export
addSampleType <- function(
    conn,
    sample_type_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "sample_types"
  table <- sample_type_tbl
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "sample_type_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "sample_type",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  ) 
  
}



