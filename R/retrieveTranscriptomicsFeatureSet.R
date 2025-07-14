#' @title retrieveTranscriptomicsFeatureSet
#' @description Search for a list of features based on an assay type in the database
#' @param conn An established connection to the database obtained from SigRepo::conn_init()
#' @param feature_tbl A data frame contains a list of feature to look up by
#' 
#' @keywords internal 
#' 
#' @export
retrieveTranscriptomicsFeatureSet <- function(
    conn,
    feature_tbl
){
  
  # Check if feature_tbl is data.frame
  if(!is(feature_tbl, "data.frame")){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show error message
    base::stop("'feature_tbl' must be a data frame object")

  }
  
  # Check if feature exists
  if(nrow(feature_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("\nThere are no features returned from the search parameters.\n"))
    
  }else{
    
    # Create a list of variables to check database ####
    required_column_fields <- c("feature_name", "organism_id")

    # Check required column fields
    if(any(!required_column_fields %in% colnames(feature_tbl))){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      # Show message
      base::stop(base::sprintf("\nTable is missing the following required column names: %s.\n", base::paste0(required_column_fields[which(!required_column_fields %in% colnames(feature_tbl))], collapse = ", ")))
    }
    
    # Make sure required column fields do not have any empty values ####
    if(any(is.na(feature_tbl[, required_column_fields, drop = FALSE]) == TRUE)){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))     
      # Show message
      base::stop(base::sprintf("\nAll required column names: %s cannot contain any empty values.\n", base::paste0(required_column_fields, collapse = ", ")))
    }
    
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
    
    # Return table
    return(feature_tbl)
    
  }
}







