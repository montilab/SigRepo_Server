#' @title addKeyword
#' @description Add keywords to database
#' @param conn An established connection to database using newConnhandler() 
#' @param keyword_tbl A data frame containing appropriate column names: keyword
#' @export
addKeyword <- function(
    conn,
    keyword_tbl
){
  
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Create a list of variables to check database
  database <- conn_info$dbname 
  db_table_name <- "keywords"
  table <- keyword_tbl
  require_tbl_colnames <- "keyword"
  include_tbl_colnames <- NULL
  exclude_db_colnames <- "keyword_id"
  
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



