#' @title addSampleType
#' @description Add sample_type to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param sample_type_tbl An data frame containing appropriate column names:
#' sample_type, brenda_accession
#' @export
addSampleType <- function(
    conn_handler,
    sample_type_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  required_column_fields <- c("sample_type")
  db_table_name <- "sample_types"
  table <- sample_type_tbl
  
  # Check required column fields
  if(any(!required_column_fields %in% colnames(table))){
    base::stop(sprintf("the table is missing the following required column names: %s.\n", paste0(required_column_fields[which(!required_column_fields %in% colnames(table))], collapse = ", ")))
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
  }
  
  # Make sure required column fields do not have any empty values ####
  if(any(is.na(table[,required_column_fields]) == TRUE)){
    base::stop(sprintf("All required column names: %s cannot contain any empty values.\n", paste0(required_column_fields, collapse = ", ")))
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn_info$conn))    
  }
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn_info$conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "sample_type_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn_info$conn, 
    db_table_name = db_table_name,
    table = table,
    coln_var = "sample_type",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn_info$conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  ) 
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))  
  
}



