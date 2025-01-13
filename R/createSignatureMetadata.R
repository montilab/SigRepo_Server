#' @title createSignatureMetadata
#' @description Create a metadata object for a signature
#' @param conn_handler An established connection to database using SigRepo::conn_init() 
#' @param omic_signature An R6 class object from OmicSignature package
#' 
#' @noRd
#' 
#' @export
createSignatureMetadata <- function(
    conn_handler,
    omic_signature
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn_handler = conn_handler, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Check if omic_signature is a valid R6 object ####
  # If yes, return whether it has difexp included ####
  has_difexp <- SigRepo::checkOmicSignature(
    omic_signature = omic_signature
  )
  
  # Extract metadata from omic_signature ####
  metadata <- omic_signature$metadata
  
  # Get signature_name ####
  signature_name <- metadata$signature_name[1]

  # Get direction_type ####
  direction_type <- metadata$direction_type[1]

  # Get assay_type ####
  assay_type <- metadata$assay_type[1]

  # Look up organism id ####
  lookup_organism <- metadata$organism[1]

  organism_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn, 
    db_table_name = "organisms", 
    return_var = c("organism_id", "organism"), 
    filter_coln_var = "organism", 
    filter_coln_val = list("organism" = lookup_organism),
    check_db_table = TRUE
  ) 
  
  # If organism is not existed in database, throw an error message
  if(nrow(organism_id_tbl) == 0){
    SigRepo::addOrganismErrorMessage(
      db_table_name = "organisms",
      unknown_values = lookup_organism
    )
  }else{
    organism_id <- organism_id_tbl$organism_id[1]
  }
  
  # Look up phenotype id #####
  lookup_phenotype <- metadata$phenotype[1]
  
  phenotype_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn_info$conn,
    db_table_name = "phenotypes", 
    return_var = c("phenotype", "phenotype_id"), 
    filter_coln_var = "phenotype", 
    filter_coln_val = list("phenotype" = lookup_phenotype),
    check_db_table = TRUE
  ) 
  
  # If phenotype is not existed in database, add to database
  if(nrow(phenotype_id_tbl) == 0){
    # Add phenotype to database
    base::suppressMessages(
      SigRepo::addPhenotype(
        conn_handler = conn_handler,
        phenotype_tbl = base::data.frame(phenotype = lookup_phenotype)
      )
    )
    # Get the updated phenotype id
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "phenotypes", 
      return_var = c("phenotype", "phenotype_id"), 
      filter_coln_var = "phenotype", 
      filter_coln_val = list("phenotype" = lookup_phenotype),
      check_db_table = FALSE
    ) 
    phenotype_id <- phenotype_id_tbl$phenotype_id[1]
  }else{
    phenotype_id <- phenotype_id_tbl$phenotype_id[1]
  }
  
  # Look up platform id ####
  if("platform" %in% names(metadata)){
    
    lookup_platform <- metadata$platform[1]
    
    if(lookup_platform %in% c("", NA, NULL)){
      platform_id <- 'GPLXXXXX'
    }
    
    # SQL statement to look up platform in database
    platform_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "platforms", 
      return_var = "platform_id",
      filter_coln_var = "platform_id", 
      filter_coln_val = list("platform_id" = lookup_platform),
      check_db_table = TRUE
    ) 
    
    if(nrow(platform_id_tbl) == 0){
      SigRepo::addPlatformErrorMessage(
        db_table_name = "platforms",
        unknown_values = lookup_platform
      )
    }else{
      platform_id <- platform_id_tbl$platform_id[1]
    }
    
  }else{
    
    platform_id <- 'GPLXXXXX'
    
  }
  
  # Look up sample_type id ####
  if("sample_type" %in% names(metadata)){
    
    lookup_sample_type <- metadata$sample_type[1]
    
    if(lookup_sample_type %in% c("", NA, NULL)){
      
      lookup_sample_type <- 'unknown'
      
      sample_type_tbl <- SigRepo::lookup_table_sql(
        conn = conn_info$conn,
        db_table_name = "sample_types", 
        return_var = "sample_type_id", 
        filter_coln_var = "sample_type", 
        filter_coln_val = list("sample_type" = lookup_sample_type),
        check_db_table = TRUE
      ) 
      
      if(nrow(sample_type_tbl) == 0){
        SigRepo::addSampleTypeErrorMessage(
          db_table_name = "sample_types",
          unknown_values = lookup_sample_type
        )
      }else{
        sample_type_id <- sample_type_tbl$sample_type_id[1]
      }
      
    }else{
      
      sample_type_tbl <- SigRepo::lookup_table_sql(
        conn = conn_info$conn,
        db_table_name = "sample_types", 
        return_var = "sample_type_id", 
        filter_coln_var = "sample_type", 
        filter_coln_val = list("sample_type" = lookup_sample_type),
        check_db_table = TRUE
      ) 
      
      if(nrow(sample_type_tbl) == 0){
        SigRepo::addSampleTypeErrorMessage(
          db_table_name = "sample_types",
          unknown_values = lookup_sample_type
        )
      }else{
        sample_type_id <- sample_type_tbl$sample_type_id[1]
      }
      
    }
    
  }else{
    
    lookup_sample_type <- 'unknown'
    
    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "sample_types", 
      return_var = "sample_type_id", 
      filter_coln_var = "sample_type", 
      filter_coln_val = list("sample_type" = lookup_sample_type),
      check_db_table = TRUE
    ) 
    
    if(nrow(sample_type_tbl) == 0){
      SigRepo::addSampleTypeErrorMessage(
        db_table_name = "sample_types",
        unknown_values = lookup_sample_type
      )
    }else{
      sample_type_id <- sample_type_tbl$sample_type_id[1]
    }
    
  }    
  
  # covariates ####
  if("covariates" %in% names(metadata)){
    if(length(metadata$covariates) == 0){
      covariates <- 'NULL'
    }else{
      covariates <- metadata$covariates[1]
    }
  }else{
    covariates <- 'NULL'
  }
  
  # description ####
  if("description" %in% names(metadata)){
    if(length(metadata$description) == 0){
      description <- 'NULL'
    }else{
      description <- metadata$description[1]
    }
  }else{
    description <- 'NULL'
  }
  
  # score_cutoff ####
  if("score_cutoff" %in% names(metadata)){
    if(length(metadata$score_cutoff) == 0){
      score_cutoff <- 'NULL'
    }else{
      score_cutoff <-  metadata$score_cutoff[1]
    }
  }else{
    score_cutoff <- 'NULL'
  }
  
  # logfc_cutoff ####
  if("logfc_cutoff" %in% names(metadata)){
    if(length(metadata$logfc_cutoff) == 0){
      logfc_cutoff <- 'NULL'
    }else{
      logfc_cutoff <- metadata$logfc_cutoff[1]
    }
  }else{
    logfc_cutoff <- 'NULL'
  }
  
  # p_value_cutoff ####
  if("p_value_cutoff" %in% names(metadata)){
    if(length(metadata$p_value_cutoff) == 0){
      p_value_cutoff <- 'NULL'
    }else{
      p_value_cutoff <- metadata$p_value_cutoff[1]
    }
  }else{
    p_value_cutoff <- 'NULL'
  }
  
  # adj_p_cutoff ####
  if("adj_p_cutoff" %in% names(metadata)){
    if(length(metadata$adj_p_cutoff) == 0){
      adj_p_cutoff <- 'NULL'
    }else{
      adj_p_cutoff <- metadata$adj_p_cutoff[1]
    }
  }else{
    adj_p_cutoff <- 'NULL'
  }
  
  # cutoff_description ####
  if("cutoff_description" %in% names(metadata)){
    if(length(metadata$cutoff_description) == 0){
      cutoff_description <- 'NULL'
    }else{
      cutoff_description <- metadata$cutoff_description[1]
    }
  }else{
    cutoff_description <- 'NULL'
  }
  
  # keywords ####
  if("keywords" %in% names(metadata)){
    if(length(metadata$keywords) == 0){
      keywords <- 'NULL'
    }else{
      keywords <- paste0(metadata$keywords, collapse = ",")
      # Create keyword table
      keyword_tbl <- data.frame(
        keyword = metadata$keywords,
        stringsAsFactors = FALSE
      )
      # Add keyword to database
      base::suppressMessages(
        SigRepo::addKeyword(
          conn_handler = conn_handler, 
          keyword_tbl = keyword_tbl
        )
      )
    }
  }else{
    keywords <- 'NULL'
  }
  
  # PMID ####
  if("PMID" %in% names(metadata)){
    if(length(metadata$PMID) == 0){
      PMID <- 'NULL'
    }else{
      PMID <- paste0(metadata$PMID, collapse = ", ")
    }
  }else{
    PMID <- 'NULL'
  }
  
  # year ####
  if("year" %in% names(metadata)){
    if(length(metadata$year) == 0){
      year <- 'NULL'
    }else{
      year <- paste0(metadata$year, collapse = ", ")
    }
  }else{
    year <- 'NULL'
  }
  
  # others ####
  if("others" %in% names(metadata)){
    if(length(metadata$others) == 0){
      others <- 'NULL'
    }else{
      others <- seq_along(metadata$others) %>% 
        purrr::map_chr(
          function(l){
            #l=1;
            list_name <- names(metadata$others)[l]
            paste0(list_name, ": ", paste0("<", metadata$others[[l]], ">", collapse = ","))
          }
        ) %>% paste0(., collapse = ";")
    }
  }else{
    others <- 'NULL'
  } 

  # Create signature metadata table ####
  metadata_tbl <- data.frame(
    signature_name = signature_name,
    organism_id = organism_id,
    direction_type = direction_type,
    assay_type = assay_type,
    phenotype_id = phenotype_id,
    platform_id = platform_id,
    sample_type_id = sample_type_id,
    covariates = covariates,
    description = description,
    score_cutoff = score_cutoff,
    logfc_cutoff = logfc_cutoff,
    p_value_cutoff = p_value_cutoff,
    adj_p_cutoff = adj_p_cutoff,
    cutoff_description = cutoff_description,
    keywords = keywords,
    PMID = PMID,
    year = year,
    others = others,
    has_difexp = has_difexp
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn_info$conn)) 
  
  # Return the metadata tbl ####
  return(metadata_tbl)
  
}



  
  

