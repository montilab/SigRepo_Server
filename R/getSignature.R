#' @title getSignature
#' @description Get a list of signatures uploaded by a specified user in the database.
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param user_name The name of the user who uploaded the signature into the database. 
#' @param signature_name The name of a signature that belongs to a specific user 
#' (\code{user_name}) who previously uploaded the signature into the database. 
#' @export
#' @import doParallel plyr
getSignature <- function(
    conn_handler,
    signature_name = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # If user_role is not admin, check user access to the signature ####
  if(user_role != "admin"){
    
    # Check user access ####
    signature_access_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "signature_access", 
      return_var = "*", 
      filter_coln_var = c("user_name", "access_type"),
      filter_coln_val = list("user_name" = user_name, access_type = c("owner", "editor", "viewer")),
      filter_var_by = "AND",
      check_db_table = TRUE
    ) 
    
    # If user does not have owner or editor permission, throw an error message
    if(nrow(signature_access_tbl) == 0){
      
      base::stop(sprintf("There are no signatures that belongs to User = '%s' in the database.\n", user_name))
      
      # Disconnect from database ####
      DBI::dbDisconnect(conn_info$conn)
      
    }
    
    # Look up signatures
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = c("signature_id", "user_name"), 
      filter_coln_val = list("signature_id" = unique(signature_access_tbl$signature_id), "user_name" = user_name),
      filter_var_by = "AND",
      check_db_table = TRUE
    ) 

  }else{
    
    # Look up signatures
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      check_db_table = TRUE
    ) 

  }
  
  # Get a list of filtered variables
  filter_var <- c("signature_name")
  filter_val <- c(signature_name) %>% base::trimws()
  
  # Filter table with given search variables
  for(r in base::seq_along(filter_var)){
    #r=1;
    filter_status <- ifelse(length(filter_val[r]) == 0 || filter_val[r] %in% c("", NA), FALSE, TRUE)
    if(filter_status == TRUE){
      signature_tbl <- signature_tbl %>% dplyr::filter(tolower(!!!syms(filter_var[r])) %in% tolower(filter_val[r]))
    }
  }
  
  # Check if signature exists
  if(nrow(signature_tbl) == 0){
    
    base::stop(sprintf("There are no signatures returned from the search parameters.\n"))
    
    # Disconnect from database ####
    DBI::dbDisconnect(conn_info$conn)
    
  }else{
    
    # Look up organism ####
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
    
    # Extract the table with appropriate column names ####
    signature_tbl <- signature_tbl %>% dplyr::select(all_of(coln_names))
    
    # Check ncores
    ncores <-  parallel::detectCores()
    
    # Sets up the parallel backend which will be utilized by Plyr.
    parallel <- FALSE
    progress <- "text"
    
    # Set-up the number of cores to run parallel tasks ####
    if(ncores > 1){
      doParallel::registerDoParallel(cores = ncores - 1)
      parallel <- TRUE
      progress <- "none"
    }

    # Create an omic signature object for each signature id ####
    omic_signature_list <- list()
    
    for(r in 1:nrow(signature_tbl)){
      #r=1;
      db_signature_tbl <- signature_tbl %>% dplyr::slice(r)
      
      omic_signature <- SigRepo::createOmicSignature(
        conn_handler = conn_handler,
        db_signature_tbl = db_signature_tbl
      )
  
      # Create OmicSignature object
      omic_signature_list <- c(
        omic_signature_list, 
        omic_signature
      )
    }
    
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
  
  # Return table
  return(omic_signature_list)
  
}







