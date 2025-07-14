#' @title addPlatform
#' @description Add platform to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param platform_tbl An data frame containing appropriate column names: 
#' platform_id, platform_name, seq_technology, organisms to be uploaded into 
#' the database.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @examples
#' 
#' # creating a dataframe
#' 
#' # platform_tbl <- data.frame(
#' # platform_id = "GPLXXXX",
#' # platform_name = "GPLXXXX",
#' # seq_technology = "RNA-seq",
#' # organisms = "Mus musculus")
#' 
#' # SigRepo::addPlatform(
#' # conn_handler = conn,
#' # platform_tbl = platform_tbl,
#' # verbose = FALSE)
#' 
#' @export
addPlatform <- function(
    conn_handler,
    platform_tbl,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  required_column_fields <- "platform_id"
  db_table_name <- "platforms"
  table <- platform_tbl
  
  # Check required column fields
  if(any(!required_column_fields %in% colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("\nTable is missing the following required column names: %s.\n", paste0(required_column_fields[which(!required_column_fields %in% colnames(table))], collapse = ", ")))
  }
  
  # Make sure required column fields do not have any empty values ####
  if(any(is.na(table[,required_column_fields]) == TRUE)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    # Show message
    base::stop(base::sprintf("All required column names: %s cannot contain any empty values.\n", paste0(required_column_fields, collapse = ", ")))
  }
  
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

  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return message
  SigRepo::verbose("Finished uploading.\n")
  
}



