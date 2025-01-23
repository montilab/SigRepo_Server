
#' @title showOrganismErrorMessage
#' @description Error message for trying to add unknown organisms into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
showOrganismErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(sprintf("\tThe following organisms: %s do not existed in the '%s' table of the database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name),
             "\tYou can use 'searchOrganisms()' to see a list of available organisms in the database.\n",
             "\tTo add these organisms into the database, please contact our admin at montilab@bu.edu for support.\n")
  
}

#' @title showPlatformErrorMessage
#' @description Error message for trying to add unknown platforms into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
showPlatformErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(sprintf("\tThe following platforms: %s do not existed in the '%s' table of the database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name), 
             "\tYou can use 'searchPlatforms()' to see a list of available platforms in the database.\n",
             "\tTo add these platforms into the database, please contact our admin at montilab@bu.edu for support.\n")
  
}

#' @title showSampleTypeErrorMessage
#' @description Error message for trying to add unknown sample types into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
showSampleTypeErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(sprintf("\tThe following sample types: %s do not existed in the '%s' table of the database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name),
             "\tYou can use 'searchSampleTypes()' to see a list of available sample types in the database.\n",
             "\tTo add these sample types into the database, please contact our admin at montilab@bu.edu for support.\n")
  
}

#' @title showTranscriptomicsErrorMessage
#' @description Error message for trying to add unknown Transcriptomics Features into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export 
showTranscriptomicsErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::stop(sprintf("\tThe following features: %s do not existed in the '%s' table of the database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name), 
             sprintf("\tYou can use 'searchFeatures()' to see a list of available features in the database.\n"),
             "\tTo add these features into the database, please contact our admin at montilab@bu.edu for support.\n")
  
}



