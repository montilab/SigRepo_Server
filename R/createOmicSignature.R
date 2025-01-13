#' @title createOmicSignature
#' @description Get the signature set uploaded by a specific user in the database.
#' @param conn_handler A handler uses to establish connection to the database
#' obtained from SigRepo::newConnhandler() (required)
#' @param db_signature_tbl The name of a signature that belongs to a specific user 
#' (\code{user_name}) who previously uploaded the signature into the database (required. 
#' 
#' @noRd
#' 
#' @export
#' @import OmicSignature
createOmicSignature <- function(
    conn_handler,
    db_signature_tbl
){
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "SELECT",
    required_role = "editor"
  )
  
  # Check if table is a data frame object and not empty
  if(!is(db_signature_tbl, "data.frame") || length(db_signature_tbl) == 0){
    
    stop(sprintf("'db_signature_tbl' must be a list object and cannot be empty.\n"))
    
    # Disconnect from database ####
    DBI::dbDisconnect(conn_info$conn)
    
  }
  
  # Get assay_type
  assay_type <- db_signature_tbl$assay_type  
  
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
  
  # Create metadata
  metadata <- db_signature_tbl %>% base::as.list()
  
  # Clean-up covariates ###
  if(length(metadata$covariates) > 0 & all(!metadata$covariates %in% c("", NA))){
    metadata$covariates <- base::strsplit(metadata$covariates, split = ",", fixed = TRUE) %>% base::unlist() %>% base::trimws()
  }
  
  # Clean-up keywords ###
  if(length(metadata$keywords) > 0 & all(!metadata$keywords %in% c("", NA))){
    metadata$keywords <- base::strsplit(metadata$keywords, split = ",", fixed = TRUE) %>% base::unlist() %>% base::trimws()
  }
  
  # Clean-up others ###
  if(length(metadata$others) > 0 & all(!metadata$others %in% c("", NA))){
    metadata$others <- base::strsplit(metadata$others, split = ";", perl = TRUE) %>% base::unlist() %>% base::trimws() %>% 
      purrr::lmap(., function(x){ 
        list_name <- base::gsub(pattern = "(.*?):(.*?)<(.*?)>", "\\1", x, perl = TRUE) %>% base::trimws()
        list_value <- base::gsub(pattern = "(.*?):(.*?)<(.*?)>", "\\3", x, perl = TRUE) %>% 
          base::strsplit(., split = ",", fixed = TRUE) %>% base::unlist() %>% base::trimws()
        list_obj <- list(object = list_value)
        names(list_obj) <- list_name
        return(list_obj)
      }) 
  }
  
  # If signature has difexp, get a copy by its signature hash key ####
  if(db_signature_tbl$has_difexp == TRUE){
    # data path to difexp in local storage ####
    data_path <- base::system.file("inst/data/difexp", package = "SigRepo")
    # Read in difexp from local storage ####
    difexp <- base::readRDS(file.path(data_path, paste0(db_signature_tbl$signature_hashkey, ".RDS")))
  }else{
    difexp = NULL
  }
  
  # Create signature set
  signature <- SigRepo::lookup_table_sql(
    conn = conn_info$conn, 
    db_table_name = "signature_feature_set", 
    return_var = "*", 
    filter_coln_var = c("signature_id"), 
    filter_coln_val = list("signature_id" = db_signature_tbl$signature_id),
    check_db_table = TRUE
  )
  
  # Look up feature_id ####
  lookup_feature_id <- signature$feature_id
  
  feature_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn, 
    db_table_name = ref_table, 
    return_var = c("feature_id", "feature_name"), 
    filter_coln_var = "feature_id", 
    filter_coln_val = list("feature_id" = lookup_feature_id),
    check_db_table = TRUE
  ) 
  
  # Add variables to table
  signature <- signature %>% dplyr::left_join(feature_id_tbl)
  
  # Rename table with appropriate column names 
  coln_names <- colnames(signature) %>% 
    base::replace(., base::match(c("feature_id"), .), c("feature_name"))
  
  # Extract the table with appropriate column names ####
  signature <- signature %>% dplyr::select(all_of(coln_names))
  
  # Create the OmicSignature object
  OmS <- OmicSignature::OmicSignature$new(
    metadata = metadata,
    signature = signature,
    difexp = difexp
  )
  
  # Return to using just a single core
  doParallel::registerDoParallel(cores = 1)
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
  
  # Return OmicSignature
  return(OmS)
  
}







