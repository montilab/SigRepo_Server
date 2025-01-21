#' @title searchSignature
#' @description Get a list of signatures available in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param signature_name The name of the signatures to be looked up by.
#' @param user_name The name of the user to be looked up by.
#' @param organism The organism to be looked up by.
#' @param phenotype The phenotype to be looked up by.
#' @param sample_type The sample type to be looked up by.
#' 
#' @examples 
#' 
#' # Create a db connection
#' conn_handler <- SigRepo::newConnHandler(
#'  dbname = "sigrepo", 
#'  host = "montilab.bu.edu", 
#'  port = 3306, 
#'  user = "guest", 
#'  password = "guest"
#' )
#' 
#' # Get a list of signatures available in the database
#' signature_list <- sigRepo::searchSignatures(conn_handler = conn_handler)
#' 
#' @export
searchSignature <- function(
    conn_handler,
    signature_name = NULL,
    user_name = NULL,
    organism = NULL,    
    phenotype = NULL,
    sample_type = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # Look up signatures
  if(length(user_name) == 0 || all(user_name %in% c("", NA))){
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = "user_name", 
      filter_coln_val = list("user_name" = user_name),
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if signature exists
  if(nrow(signature_tbl) == 0){
    
    base::stop(sprintf("There are no signatures returned from the search parameters.\n"))
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
    
  }else{
  
    # Look up organism id ####
    lookup_organism_id <- signature_tbl$organism_id
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    ) 
    
    # Look up phenotype id ####
    lookup_phenotype_id <- signature_tbl$phenotype_id
    
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "phenotypes", 
      return_var = c("phenotype_id", "phenotype"), 
      filter_coln_var = "phenotype_id", 
      filter_coln_val = list("phenotype_id" = lookup_phenotype_id),
      check_db_table = TRUE
    ) 
    
    # Look up sample_type_id ####
    lookup_sample_type_id <- signature_tbl$sample_type_id
    
    sample_type_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "sample_types", 
      return_var = c("sample_type_id", "sample_type"), 
      filter_coln_var = "sample_type_id", 
      filter_coln_val = list("sample_type_id" = lookup_sample_type_id),
      check_db_table = TRUE
    ) 
    
    # Add variables to table
    signature_tbl <- signature_tbl %>% 
      dplyr::left_join(organism_id_tbl) %>% 
      dplyr::left_join(phenotype_id_tbl) %>% 
      dplyr::left_join(sample_type_id_tbl)
    
    # Rename table with appropriate column names 
    coln_names <- colnames(signature_tbl) %>% 
      base::replace(., base::match(c("organism_id", "phenotype_id", "sample_type_id"), .), c("organism", "phenotype", "sample_type"))
    
    # Get a list of filtered variables
    filter_var <- c("signature_name", "organism", "phenotype", "sample_type")
    filter_val <- c(signature_name, organism, phenotype, sample_type) %>% base::trimws()
    
    # Filter table with given search variables
    for(r in base::seq_along(filter_var)){
      #r=1;
      filter_status <- ifelse(length(filter_val[r]) == 0 || filter_val[r] %in% c("", NA), FALSE, TRUE)
      if(filter_status == TRUE){
        signature_tbl <- signature_tbl %>% dplyr::filter(trimws(tolower(!!!syms(filter_var[r]))) %in% trimws(tolower(filter_val[r])))
      }
    }
    
    # Extract the table with appropriate column names ####
    signature_tbl <- signature_tbl %>% dplyr::select(all_of(coln_names))
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
    
    # Return table
    return(signature_tbl)

  }
}







