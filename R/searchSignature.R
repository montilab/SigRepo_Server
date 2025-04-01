#' @title searchSignature
#' @description Get a list of signatures available in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param signature_id ID of the signatures to be looked up by.
#' @param signature_name Name of the signatures to be looked up by.
#' @param user_name The name of the user to be looked up by.
#' @param organism The organism to be looked up by.
#' @param phenotype The phenotype to be looked up by.
#' @param sample_type The sample type to be looked up by.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
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
#' signature_tbl <- sigRepo::searchSignature(
#'   conn_handler = conn_handler
#' )
#' 
#' @export
searchSignature <- function(
    conn_handler,
    signature_id = NULL,
    signature_name = NULL,
    user_name = NULL,
    organism = NULL,    
    phenotype = NULL,
    sample_type = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # If user_role is not admin, check user access to the signature ####
  if(length(user_name) > 0 && all(!user_name %in% c("", NA))){
    
    # Check user access ####
    signature_access_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signature_access", 
      return_var = "*", 
      filter_coln_var = c("user_name", "access_type"),
      filter_coln_val = list("user_name" = user_name, "access_type" = c("owner", "editor", "viewer")),
      filter_var_by = "AND",
      check_db_table = TRUE
    ) 
    
    # If user does not have owner or editor permission, throw an error message
    if(nrow(signature_access_tbl) == 0){
  
      # Look up signatures
      signature_tbl <- SigRepo::lookup_table_sql(
        conn = conn, 
        db_table_name = "signatures", 
        return_var = "*", 
        filter_coln_var = "signature_id", 
        filter_coln_val = list("signature_id" = ""),
        check_db_table = TRUE
      )
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      
      # Show message
      SigRepo::verbose(base::sprintf("There are no signatures returned from the search parameters.\n"))
      
      # Return table
      return(signature_tbl)
      
    }
    
    # Look up signatures
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = "signature_id", 
      filter_coln_val = list("signature_id" = unique(signature_access_tbl$signature_id)),
      check_db_table = TRUE
    ) 
    
  }else{
    
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if signature exists
  if(nrow(signature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))     
    
    # Show message
    SigRepo::verbose(base::sprintf("There are no signatures returned from the search parameters.\n"))
    
    # Return table
    return(signature_tbl)
    
  }else{
    
    # Look up organism id ####
    lookup_organism_id <- signature_tbl$organism_id
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    ) 
    
    # Look up phenotype id ####
    lookup_phenotype_id <- signature_tbl$phenotype_id
    
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "phenotypes", 
      return_var = c("phenotype_id", "phenotype"), 
      filter_coln_var = "phenotype_id", 
      filter_coln_val = list("phenotype_id" = lookup_phenotype_id),
      check_db_table = TRUE
    ) 
    
    # Look up sample_type_id ####
    lookup_sample_type_id <- signature_tbl$sample_type_id
    
    sample_type_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "sample_types", 
      return_var = c("sample_type_id", "sample_type"), 
      filter_coln_var = "sample_type_id", 
      filter_coln_val = list("sample_type_id" = lookup_sample_type_id),
      check_db_table = TRUE
    ) 
    
    # Add variables to table
    signature_tbl <- signature_tbl %>% 
      dplyr::left_join(organism_id_tbl, by = "organism_id") %>% 
      dplyr::left_join(phenotype_id_tbl, by = "phenotype_id") %>% 
      dplyr::left_join(sample_type_id_tbl, by = "sample_type_id")
    
    # Rename table with appropriate column names 
    coln_names <- base::colnames(signature_tbl) %>% 
      base::replace(., base::match(c("organism_id", "phenotype_id", "sample_type_id"), .), c("organism", "phenotype", "sample_type"))
    
    # Get a list of filtered variables
    filter_var_list <- list(
      "signature_id" = base::unique(signature_id),
      "signature_name" = base::unique(signature_name), 
      "organism" = base::unique(organism), 
      "phenotype" = base::unique(phenotype), 
      "sample_type" = base::unique(sample_type)
    )
    
    # Filter table with given search variables
    for(r in base::seq_along(filter_var_list)){
      #r=1;
      filter_status <- ifelse(length(filter_var_list[[r]]) == 0 || all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
      if(filter_status == TRUE){
        filter_var <- base::names(filter_var_list)[r]
        filter_val <- filter_var_list[[r]][which(!filter_var_list[[r]] %in% c(NA, ""))]
        signature_tbl <- signature_tbl %>% dplyr::filter(base::trimws(base::tolower(!!!syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
      }
    }
    
    # Check if signature is empty, throw an error message
    if(nrow(signature_tbl) == 0){
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      
      # Show message
      SigRepo::verbose(base::sprintf("There are no signatures returned from the search parameters.\n"))
      
      # Return table
      return(signature_tbl)
      
    }
    
    # Extract the table with appropriate column names ####
    signature_tbl <- signature_tbl %>% dplyr::select(all_of(coln_names))
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Return table
    return(signature_tbl)
    
  }
}







