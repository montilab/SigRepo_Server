#' @title addOrganism
#' @description Add organism to database
#' @param conn An established connection to database using newConnhandler() 
#' @param organism_tbl An data frame containing appropriate column names: organism
#' @export
addOrganism <- function(
    conn,
    organism_tbl
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "organisms"
  table <- organism_tbl
  
  # Check table against database table ####
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "organism_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepoR::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "organism",
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



