#' @title addOrganism
#' @description Add organism to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param organism_tbl A data frame object contains appropriate column names: organism
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
addOrganism <- function(
    conn_handler,
    organism_tbl,
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
  required_column_fields <- "organism"
  db_table_name <- "organisms"
  table <- organism_tbl
  
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
    base::stop(base::sprintf("\nAll required column names: %s cannot contain any empty values.\n", paste0(required_column_fields, collapse = ", ")))
  }
  
  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "organism_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn, 
    db_table_name = db_table_name,
    table = table,
    coln_var = "organism",
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
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Return message
  SigRepo::verbose("Finished uploading.\n")
  
}



