#' @title addSignatureAccess
#' @description Add signature access information to database
#' @param conn An established database connection using newConnhandler() 
#' @param signature_id A data frame containing appropriate column names
#' @param user_id A data frame containing appropriate column names
#' @param access_type A data frame containing appropriate column names
#' @export
addUserToSignature <- function(
    conn,
    signature_id,
    user_id,
    access_type = c("admin", "owner", "viewer")
){
      
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Create access_tbl
  table <- data.frame(
    signature_id = signature_id,
    user_id = user_id,
    access_type = access_type
  )
  
  # Create a list of variables to check database
  database <- conn_info$dbname
  db_table_name <- "signature_access"
  table <- table
  require_tbl_colnames <- c("signature_id", "user_id", "access_type")
  include_tbl_colnames = NULL
  exclude_db_colnames = NULL
  
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

  # Get SQL statement
  statement <- insert_table_sql(conn = conn, db_table_name = db_table_name, table = table)
  
  # Insert table into database
  tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
}



