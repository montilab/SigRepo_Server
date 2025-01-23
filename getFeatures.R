#' @title getFeatures
#' @description Get signatures to database
#' @param conn_handler An established connection to database using SigRepo::newConnhandler() 
#' @param organism An organism in which the features are relating to
#' @param filter_by A list of features to look up.
#' @export
searchFeatures <- function(
    conn_handler,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites")
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
  
  # Check organism
  stopifnot("'organism' cannot be empty." = (length(organism) > 0 && all(!organism %in% c(NA, "", NULL))))
  
  # look up organism
  lookup_organism <- unique(organism)
  
  # Look up organism id
  organism_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn, 
    db_table_name = "organisms", 
    return_var = c("organism_id", "organism"), 
    filter_coln_var = "organism", 
    filter_coln_val = list("organism" = lookup_organism),
    check_db_table = TRUE
  ) 
  
  if(nrow(organism_id_tbl) != length(lookup_organism)){
    SigRepo::showOrganismErrorMessage(
      db_table_name = "organisms",
      unknown_values = lookup_organism[which(!lookup_organism %in% organism_id_tbl$organism)]
    )
  }
  
  # Look up features by organism
  if(length(filter_by) == 0){
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      filter_coln_var = c("organism_id"),
      filter_coln_val = list("organism_id" = organism_id_tbl$organism_id),
      check_db_table = TRUE
    )  
    
  }else{
    
    feature_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      filter_coln_var = c("feature_name", "organism_id"),
      filter_coln_val = list("feature_name" = filter_by, "organism_id" = organism_id_tbl$organism_id),
      filter_var_by = "AND",
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if feature exists
  if(nrow(feature_tbl) > 0){
    
    # Add variables to table
    feature_tbl <- feature_tbl %>% 
      dplyr::left_join(organism_id_tbl) %>% 
      dplyr::mutate(assay_type = assay_type) %>% 
      dplyr::select(assay_type, organism, organism_id, everything())
    
  }else{
    
    # Add variables to table
    feature_tbl <- feature_tbl %>% 
      dplyr::mutate(organism = lookup_organism, assay_type = assay_type) %>% 
      dplyr::select(assay_type, organism, organism_id, everything())
    
  }
  
  # Disconnect from database ####
  DBI::dbDisconnect(conn_info$conn)
  
  # Return table
  return(feature_tbl)

}







