#' @title addSignatureFeatureSet
#' @description Add signature feature set to database
#' @param conn An established connection to database using newConnhandler() 
#' @param signature_id An unique signature name
#' @param assay_type Type of assays: transcriptomics, proteomics, metabolomics, 
#' methylomics, genetic_variations, dna_binding_sites
#' @param sig_feature_set A data frame containing appropriate column names:
#' feature_name, origin_feature_id, score, direction
#' @export
addSignatureFeatureSet <- function(
    conn,
    signature_id,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    sig_feature_set
){
  
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Check assay_type
  assay_type <- match.arg(assay_type)
  
  # Check signature_id
  stopifnot("'signature_id' cannot be empty." = 
              (length(signature_id) > 0 && !signature_id %in% c(NA, "")))
  
  # Look up signature id in database
  statement <- SigRepoR::lookup_table_sql(
    table = "signatures", 
    return_var = "signature_id", 
    filter_coln_var = "signature_id", 
    filter_coln_val = list("signature_id" = signature_id)
  )
  
  # Get query table
  signature_id_tbl <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(nrow(signature_id_tbl) == 0)
    stop(sprintf("signature_id = `%s` is currently not existed in our database.\n", signature_id),
         "You can use 'getSignatures()' to see a list of available signatures in our database.\n",
         "To add an signature to our database, please contact our admin for more details.\n")
  
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
  
  # Check if symbol column field exists in the table
  if(any(!"feature_name" %in% colnames(sig_feature_set)))
    stop(sprintf("'sig_feature_set' must have the following column names: feature_name"))
  
  # Look up feature id from the reference database
  unique_features <- unique(table$feature_name)
  
  # SQL statement to look up table
  statement <- SigRepoR::lookup_table_sql(
    table = ref_table, 
    return_var = c("feature_id", "feature_name"),
    filter_coln_var = "feature_name", 
    filter_coln_val = list("feature_name" = unique_features)
  ) 
  
  # Get query table
  feature_id_tbl <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(nrow(feature_id_tbl) != length(unique_features))
    stop(sprintf("Feature IDs: `%s` is/are currently not existed in our database.\n", paste0(unique_features[which(!unique_features %in% feature_id_tbl$feature_name)]), collapse=", "),
         "You can use 'getRefFeatureSet()' function to see a list of reference feature sets that are available in our database.\n",
         "To add a feature to our your reference feature set, please contact our admin for more details.\n")
  
  # Add feature id to the table
  table <- table %>% 
    dplyr::mutate(unique_id = trimws(tolower(feature_name))) %>% 
    dplyr::left_join(
      feature_id_tbl %>% 
        dplyr::mutate(unique_id = trimws(tolower(feature_name))),
      by = "unique_id"
    ) %>% 
    dplyr::mutate(
      signature_id = signature_id,
      assay_type = assay_type
    )
  
  # Create a list of variables to check database
  database <- conn_info$dbname
  db_table_name <- "signature_feature_set"
  table <- table
  require_tbl_colnames <- NULL
  include_tbl_colnames <- NULL
  exclude_db_colnames = NULL
  
  # Check if table exists in database
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    database = database,
    db_table_name = db_table_name,
    table = table,
    require_tbl_colnames = require_tbl_colnames,
    include_tbl_colnames =  include_tbl_colnames,
    exclude_db_colnames = exclude_db_colnames
  )
  
  # Get SQL statement
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



