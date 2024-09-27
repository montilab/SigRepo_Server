#' @title addSampleType
#' @description Add platform to database
#' @param conn An established connection to database using newConnhandler() 
#' @param sample_type_tbl An data frame containing appropriate column names:
#' sample_type, brenda_accession
#' @export
addSampleType <- function(
    conn,
    sample_type_tbl
){
  
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Create a list of variables to check database
  database <- conn_info$dbname
  db_table_name <- "sample_types"
  table <- sample_type_tbl
  require_tbl_colnames <- "sample_type"
  include_tbl_colnames <- NULL
  exclude_db_colnames <- "sample_type_id"
  
  # Check if table exists in database
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    database = database,
    db_table_name = db_table_name,
    table = table,
    require_tbl_colnames = require_tbl_colnames,
    include_tbl_colnames = include_tbl_colnames,
    exclude_db_colnames = exclude_db_colnames
  )
  
  # Get SQL statement to insert table into database
  statement <- SigRepoR::insert_table_sql(conn = conn, db_table_name = db_table_name, table = table)
  
  # Insert table into database
  tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
}



