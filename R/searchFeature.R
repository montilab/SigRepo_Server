#' @title searchFeature
#' @description Search for a list of features based on an assay type in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param assay_type Type of assays: transcriptomics, proteomics, metabolomics, 
#' methylomics, genetic_variations, dna_binding_sites (required)
#' @param feature_name A list of feature names to look up.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
searchFeature <- function(
    conn_handler,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    feature_name = NULL,
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
  
  # Check assay_type
  assay_type <- base::tryCatch({
    base::match.arg(assay_type)  
  }, error = function(e){
    # Disconnect from database
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  })  
  
  # Get reference table
  if(assay_type == "transcriptomics"){
    ref_table <- "transcriptomics_features"
  }else if(assay_type == "proteomics"){
    ref_table <- "proteomics_features"
  }else if(assay_type == "metabolomics"){
    ref_table <- "metabolomics_features"
  }else if(assay_type == "methylomics"){
    ref_table <- "methylomics_features"
  }else if(assay_type == "genetic_variations"){
    ref_table <- "genetic_variations_features"
  }else if(assay_type == "dna_binding_sites"){
    ref_table <- "dna_binding_sites_features"
  }
  
  # Look up features by organism
  if(length(feature_name) == 0 || all(feature_name %in% c("", NA))){
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      filter_coln_var = "feature_name",
      filter_coln_val = list("feature_name" = feature_name),
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if feature exists
  if(nrow(feature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("\nThere are no features returned from the search parameters.\n"))
    
  }else{
    
    # look up organism
    lookup_organism_id <- unique(feature_tbl$organism_id)
    
    # Look up organism id
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    )  
    
    # Add variables to table
    feature_tbl <- feature_tbl %>% 
      dplyr::left_join(organism_id_tbl, by = "organism_id") 
    
    # Rename table with appropriate column names 
    coln_names <- base::colnames(feature_tbl) %>% 
      base::replace(., base::match(c("organism_id"), .), c("organism"))
    
    # Extract the table with appropriate column names ####
    feature_tbl <- feature_tbl %>% dplyr::select(all_of(coln_names))
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    
    # Return table
    return(feature_tbl)
    
  }
}







