#' @title searchFeature
#' @description Get a list of features available in the database
#' @param conn_handler A handler uses to establish connection to a remote database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param assay_type The assay type uses to filter the features by (required). 
#' Type of assays: transcriptomics, proteomics, metabolomics, methylomics, 
#' genetic_variations, dna_binding_sites. 
#' Default: transcriptomics.
#' @param feature_name a list of features to search by. Default is NULL which
#' includes all features in the database.
#' @export
searchFeature <- function(
    conn_handler,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    feature_name = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # Check assay_type
  assay_type <- base::match.arg(assay_type)  
  
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
      conn = conn_info$conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      filter_coln_var = c("feature_name"),
      filter_coln_val = list("feature_name" = feature_name),
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if feature exists
  if(nrow(feature_tbl) == 0){
    
    base::stop(sprintf("There are no features returned from the search parameters.\n"))
    
    # Disconnect from database ####
    DBI::dbDisconnect(conn_info$conn)
    
  }else{
    
    # Look up organism id ####
    lookup_organism_id <- feature_tbl$organism_id
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism_id", 
      filter_coln_val = list("organism_id" = lookup_organism_id),
      check_db_table = TRUE
    ) 
    
    # Add variables to table
    feature_tbl <- feature_tbl %>% dplyr::left_join(feature_tbl)
    
    # Rename table with appropriate column names 
    coln_names <- colnames(feature_tbl) %>% 
      base::replace(., base::match(c("organism_id"), .), c("organism"))
    
    # Extract the table with appropriate column names ####
    signature_tbl <- feature_tbl %>% dplyr::select(all_of(coln_names))
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
    
    # Return table
    return(feature_tbl)
    
  }
}







