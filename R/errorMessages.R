
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
  
  base::stop(
    sprintf("\nThe following organisms do not existed in the '%s' table of the database:\n%s", db_table_name, paste0("'", unknown_values, "'", collapse = "\n")), 
    sprintf("\nYou can use 'searchOrganism()' to see a list of available organisms in the database.\n"),
    sprintf("\nTo add these organisms into the database, please contact our admin at montilab@bu.edu for support.\n")
  )
  
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
  
  base::stop(
    sprintf("\nThe following platforms do not existed in the '%s' table of the database:\n%s", db_table_name, paste0("'", unknown_values, "'", collapse = "\n")), 
    sprintf("\nYou can use 'searchPlatform()' to see a list of available platforms in the database.\n"),
    sprintf("\nTo add these platforms into the database, please contact our admin at montilab@bu.edu for support.\n")
  )
  
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
  
  base::stop(
    sprintf("\nThe following sample types do not existed in the '%s' table of the database:\n%s", db_table_name, paste0("'", unknown_values, "'", collapse = "\n")), 
    sprintf("\nYou can use 'searchSampleType()' to see a list of available sample types in the database.\n"),
    sprintf("\nTo add these sample types into the database, please contact our admin at montilab@bu.edu for support.\n")
  )
  
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
  
  base::stop(
    sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s", db_table_name, paste0("'", unknown_values, "'", collapse = "\n")), 
    sprintf("\nYou can use 'searchFeature()' to see a list of available features in the database."),
    sprintf("\nTo add these features into the database, please contact our admin at montilab@bu.edu for support.\n")
  )
  
}



