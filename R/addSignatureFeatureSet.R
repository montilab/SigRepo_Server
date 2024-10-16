#' @title addSignatureFeatureSet
#' @description Add signature feature set to database
#' @param conn An established connection to database using newConnhandler() 
#' @param signature_id An unique signature id
#' @param organism_id An unique organism id
#' @param assay_type Type of assays: transcriptomics, proteomics, metabolomics, 
#' methylomics, genetic_variations, dna_binding_sites
#' @param feature_set A data frame containing the appropriate column names:
#' feature_name, origin_feature_id, score, direction
#' @export
addSignatureFeatureSet <- function(
    conn,
    signature_id,
    organism_id,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    feature_set
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "user"
  )
  
  # Check assay_type
  assay_type <- match.arg(assay_type)  
  
  # Check signature_id
  stopifnot("'signature_id' cannot be empty." = 
              (length(signature_id) == 1 && !signature_id %in% c(NA, "")))
  
  # Check organism_id
  stopifnot("'organism_id' cannot be empty." = 
              (length(organism_id) == 1 && !organism_id %in% c(NA, "")))
  
  # Create a list of variables to check database ####
  db_table_name <- "signature_feature_set"
  
  # Create signature feature set table
  table <- feature_set %>% 
    dplyr::mutate(
      signature_id = signature_id,
      organism_id = organism_id,
      assay_type = assay_type
    )
  
  # Create a hash key to look up feature id in reference table ####
  table <- SigRepoR::createHashKey(
    table = table,
    hash_var = "feature_hashkey",
    hash_columns = c("feature_name", "organism_id"),
    hash_method = "md5"
  )
  
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
  
  # Get feature id ####
  coln_var <- "feature_hashkey"
  coln_var_id <- "feature_id"
  
  # Look up table
  lookup_id_tbl <- SigRepoR::getVariableID(
    conn = conn, 
    db_table_name = ref_table,
    table = table,
    coln_var = coln_var, 
    coln_var_id = coln_var_id,
    check_db_table = TRUE
  )
  
  ## Add ID to table
  table <- table %>% dplyr::mutate(id = trimws(tolower(!!!syms(coln_var)))) %>% 
    dplyr::left_join(
      lookup_id_tbl %>% dplyr::mutate(id = trimws(tolower(!!!syms(coln_var)))), #%>% dplyr::select(-all_of(coln_var)), 
      by = "id"
    )
  
  # If any ID is missing, produce an error message
  if(any(table$feature_id %in% c("", NA)))
    SigRepoR::addTranscriptomicsFeatureErrorMessage(
      db_table_name = ref_table,
      organism_id = organism_id,
      unknown_features = table$feature_name[which(table$feature_id %in% c("", NA))]
    )
  
  # Create a hash key to look up signature feature set in database ####
  table <- SigRepoR::createHashKey(
    table = table,
    hash_var = "sig_feature_hashkey",
    hash_columns = c("signature_id", "orig_feature_id", "feature_id", "assay_type"),
    hash_method = "md5"
  )
  
  # Check table against database table ####
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "sig_feature_id",
    check_db_table = FALSE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepoR::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "sig_feature_hashkey",
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
