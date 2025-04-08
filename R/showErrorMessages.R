
#' @title showOrganismErrorMessage
#' @description Error message for trying to add unknown organisms to the database
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
    base::sprintf("\nThe following organisms do not existed in the '%s' table of the database:\n%s", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchOrganism()' to see a list of available organisms in the database."),
    base::sprintf("\nTo add these organisms to our database, please contact our admin for support.\n")
  ) 
  
}

#' @title showPlatformErrorMessage
#' @description Error message for trying to add unknown platforms to the database
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
    base::sprintf("\nThe following platforms do not existed in the '%s' table of the database:\n%s", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchPlatform()' to see a list of available platforms in the database."),
    base::sprintf("\nTo add these platforms to our database, please contact our admin for support.\n")
  )
  
}

#' @title showSampleTypeErrorMessage
#' @description Error message for trying to add unknown sample types to the database
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
    base::sprintf("\nThe following sample types do not existed in the '%s' table of the database:\n%s", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchSampleType()' to see a list of available sample types in the database."),
    base::sprintf("\nTo add these sample types to our database, please contact our admin for support.\n")
  )
  
}

#' @title showTranscriptomicsErrorMessage
#' @description Error message for trying to add unknown Transcriptomics Features to the database
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
  
  base::warning(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchFeature()' to see a list of available features in the database."),
    base::sprintf("\nTo add these features to our database, please contact our admin for support.\n")
  )
  

}

#' @title showProteomicsErrorMessage
#' @description Error message for trying to add unknown Protoeomics Features to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export 
showProteomicsErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::warning(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchFeature()' to see a list of available features in the database."),
    base::sprintf("\nTo add these features to our database, please contact our admin for support.\n")
  )
  
  
}

#' @title showMetabalomicsErrorMessage
#' @description Error message for trying to add unknown Metabalomics Features to the database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export 
showMetabalomicsErrorMessage <- function(
    db_table_name,
    unknown_values
){
  
  base::warning(
    base::sprintf("\nThe following features do not existed in the '%s' table of the database:\n%s", db_table_name, base::paste0("'", unknown_values, "'", collapse = "\n")), 
    base::sprintf("\nYou can use 'searchFeature()' to see a list of available features in the database."),
    base::sprintf("\nTo add these features to our database, please contact our admin for support.\n")
  )
  
  
}




