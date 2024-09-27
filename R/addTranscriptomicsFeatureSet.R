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
  
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Create a list of variables to check database
  database <- conn_info$dbname 
  db_table_name <- "transcriptomics_features"
  table <- feature_set
  require_tbl_colnames <- c("feature_name", "organism_id")
  include_tbl_colnames <- NULL
  exclude_db_colnames <- c("feature_id")
  
  # Look up organism id from the the database
  unique_organisms <- unique(table$organism)
  
  # SQL statement to look up organism in database
  statement <- SigRepoR::lookup_table_sql(
    table = "organisms", 
    return_var = c("organism_id", "organism"), 
    filter_coln_var = "organism", 
    filter_coln_val = list("organism" = unique_organisms)
  ) 
  
  # Get query table
  organism_id_tbl <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(nrow(organism_id_tbl) != length(unique_organisms))
    stop(sprintf("Organisms: %s is/are currently not existed in our database.\n", paste0(unique_organisms[which(!unique_organisms %in% organism_id_tbl$organism)], collapse=", ")),
         "You can use 'getOrganisms()' function to see a list of available organisms in our database.\n",
         "To add an organism to our database, please contact our admin for more details.\n")
  
  # Add organism id to the table
  table <- table %>% 
    dplyr::mutate(organism = trimws(tolower(organism))) %>% 
    dplyr::left_join(
      organism_id_tbl %>% 
        dplyr::mutate(organism = trimws(tolower(organism))),
      by = "organism"
    ) %>% 
    dplyr::select(-"organism")
  
  # Check if table exists in database
  table <- SigRepoR::checkTableInput(
    conn = conn,
    database = database,
    db_table_name = db_table_name,
    table = table,
    require_tbl_colnames = require_tbl_colnames,
    include_tbl_colnames = include_tbl_colnames,
    exclude_db_colnames = exclude_db_colnames
  )
  
  # Get SQL statement to insert table into database
  statement <- SigRepoR::insert_table_sql(conn = conn, db_table_name = db_table_name, table = table)
  
  # Insert table into database
  tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
}


