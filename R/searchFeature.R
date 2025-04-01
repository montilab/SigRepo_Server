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
    
    # Get reference table
    if(assay_type == "transcriptomics"){
      feature_tbl <- SigRepo::retrieveTranscriptomicsFeatureSet(conn = conn, feature_tbl = feature_tbl)
    }else if(assay_type == "proteomics"){
    }else if(assay_type == "metabolomics"){
    }else if(assay_type == "methylomics"){
    }else if(assay_type == "genetic_variations"){
    }else if(assay_type == "dna_binding_sites"){
    }
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    
    # Return table
    return(feature_tbl)
    
  }
}







