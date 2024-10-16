#' @title getFeatures
#' @description Get signatures to database
#' @param conn An established connection to database using newConnhandler() 
#' @param filter_by author id used to submit the signature
#' @export
getFeatures <- function(
    conn,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    filter_by = NULL
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "guest"
  )
  
  # Check assay_type
  assay_type <- match.arg(assay_type)  
  
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
  
  # Look up signatures
  if(length(filter_by) == 0){
    
    feature_tbl <- SigRepoR::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      check_db_table = TRUE
    )  
    
  }else{
    
    feature_tbl <- SigRepoR::lookup_table_sql(
      conn = conn, 
      db_table_name = ref_table, 
      return_var = "*", 
      filter_coln_var = "feature_name", 
      filter_coln_val = list("feature_name" = filter_by),
      check_db_table = TRUE
    ) 
    
  }
  
  return(feature_tbl)

}







