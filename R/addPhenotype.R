#' @title addPhenotype
#' @description Add phenotype to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param phenotype_tbl An data frame containing appropriate column names: phenotype
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @examples
#' 
#' 
#' # creating a dataframe
#' 
#' # phenotype_tbl <- data.frame(
#' # phenotype = "phenotype1")
#' 
#' # SigRepo::addPhenotype(
#' # conn_handler = conn,
#' # phenotype_tbl = phenotype_tbl,
#' # verbose = FALSE)
#'  
#' @export
addPhenotype <- function(
    conn_handler,
    phenotype_tbl,
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
    required_role = "editor"
  )
  
  # Create a list of variables to check database ####
  required_column_fields <- "phenotype"
  db_table_name <- "phenotypes"
  table <- phenotype_tbl
  
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
    exclude_coln_names = "phenotype_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn, 
    db_table_name = db_table_name,
    table = table,
    coln_var = "phenotype",
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



