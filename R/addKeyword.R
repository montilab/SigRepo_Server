#' @title addKeyword
#' @description Add keywords to database
#' @param conn An established connection to database using newConnhandler() 
#' @param keyword_tbl A data frame containing appropriate column names: keyword
#' @export
addKeyword <- function(
    conn,
    keyword_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "user"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "keywords"
  table <- keyword_tbl
  
  # Check table against database table ####
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "keyword_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepoR::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "keyword",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepoR::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
}



