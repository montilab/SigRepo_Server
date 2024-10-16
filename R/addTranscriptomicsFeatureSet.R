#' @title addTranscriptomicsFeatureSet
#' @description Add Transcriptomics Feature Set into database
#' @param conn An established connection to database using newConnhandler() 
#' @param feature_set A data frame containing appropriate column names: 
#' feature_name, organism, description, synonyms, n_synonyms, ensemble_ids, 
#' n_ensemble_ids, transcript_biotypes, chromosome_name, start_position, 
#' end_position
#' @export
addTranscriptomicsFeatureSet <- function(
    conn,
    feature_set
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "admin"
  )
  
  # Create a list of variables to check database ####
  db_table_name <- "transcriptomics_features"
  table <- feature_set
  
  # Get organism id ####
  coln_var <- "organism"
  coln_var_id <- "organism_id"
  
  # Look up table
  lookup_id_tbl <- SigRepoR::getVariableID(
    conn = conn, 
    db_table_name = "organisms",
    table = table,
    coln_var = coln_var, 
    coln_var_id = coln_var_id,
    check_db_table = TRUE
  )
  
  ## Add ID to table
  table <- table %>% dplyr::mutate(id = trimws(tolower(!!!syms(coln_var)))) %>% 
    dplyr::left_join(
      lookup_id_tbl %>% dplyr::mutate(id = trimws(tolower(!!!syms(coln_var)))) %>% dplyr::select(-all_of(coln_var)), 
      by = "id"
    )
  
  # If any ID is missing, produce an error message
  if(any(table$organism_id %in% c("", NA)))
    SigRepoR::addOrganismErrorMessage(
      db_table_name = 'organisms',
      unknown_values = table$organism[which(table$organism_id %in% c("", NA))]
    )
  
  # Create a hash key to look up values in database ####
  table <- SigRepoR::createHashKey(
    table = table,
    hash_var = "feature_hashkey",
    hash_columns = c("feature_name", "organism_id"),
    hash_method = "md5"
  )
  
  # Check table against database table ####
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "feature_id",
    check_db_table = FALSE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepoR::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "feature_hashkey",
    check_db_table = FALSE
  )
  
  # Insert table into database ####
  SigRepoR::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
}


