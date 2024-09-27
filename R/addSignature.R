#' @title addSignature
#' @description Add signature to database
#' @param conn An established connection to database using newConnhandler() 
#' @param omic_signature An R6 class object from OmicSignature package
#' @export
addSignature <- function(
    conn,
    omic_signature
){
  
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Check if omic_signature is an OmicSignature class object
  if(!is(omic_signature, "OmicSignature"))
    stop("'omic_signature' must be an R6 class object from OmicSignature package.") 
  
  # Check metadata and signature
  if(!"metadata" %in% names(omic_signature))
    stop("'omic_signature' must contain a metadata object.\n")
  
  if(!"signature" %in% names(omic_signature))
    stop("'omic_signature' must contain a signature object.\n")
  
  # Extract metadata and signature table from omic_signature ####
  metadata <- omic_signature$metadata 
  signature <- omic_signature$signature 
  
  if(!is(metadata, "list"))
    stop("'metadata' in OmicSignature must be a list.")
  
  if(!is(signature, "data.frame"))
    stop("'signature' in OmicSignature must be a data frame.")
  
  # Check difexp is provided
  if("difexp" %in% names(omic_signature)){
    difexp <- omic_signature$difexp
    if(is.null(difexp)){
      difexp <- NULL
    }else{
      if(!is(difexp, "data.frame")) 
        stop("'difexp' in OmicSignature must be a data frame object.")
    }
  }else{
    difexp <- NULL
  }
  
  # Check required metadata fields
  metadata_fields <- c('signature_name', 'organism', 'direction_type', 'assay_type')
  
  if(any(!metadata_fields %in% names(metadata)))
    stop("'metadata' in OmicSignature must have the following column names:", paste0(metadata_fields, collapse = ", "))
  
  # Check required signature fields
  signature_fields <- c('id', 'symbol', 'score', 'direction')
  
  if(any(!signature_fields %in% colnames(signature)))
    stop("'signature' in OmicSignature must have the following column names:", paste0(signature_fields, collapse = ", "))
  
  # If difexp is provided, check required difexp fields ####
  difexp_fields <- c('id', 'symbol', 'score', 'p_value')
  
  if(!is.null(difexp) && any(!difexp_fields %in% colnames(difexp)))
    stop("'difexp' in OmicSignature must have the following column names:", paste0(difexp_fields, collapse = ", "))
  
  # Check signature name (required) ####
  if(metadata$signature_name[1] %in% c(NA, "", NULL)){
    stop("'signature_name' in OmicSignature's metadata cannot be empty.")
  }else{
    signature_name <- metadata$signature_name[1]
  }
  
  # Check direction_type (required) ####
  direction_type_options <- c("uni-directional", "bi-directional", "multiple")
  
  if(!metadata$direction_type[1] %in% direction_type_options){
    stop("'direction_type' in OmicSignature's metadata object must be: \n", paste0(direction_type_options, collapse = "/"))
  }else{
    direction_type <- metadata$direction_type[1]
  }
  
  # Check assay_type (required) ####
  assay_type_options <- c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "DNA_binding_sites", "others")
  
  if(!metadata$assay_type[1] %in% assay_type_options){
    stop("'assay_type' in OmicSignature's metadata object must be: ", paste0(assay_type_options, collapse = "/"))
  }else{
    assay_type <- metadata$assay_type[1]
  }
  
  # Check organism (required) #####
  if(metadata$organism[1] %in% c(NA, "", NULL)){
    stop("'organism' in OmicSignature's metadata cannot be empty.")
  }else{
    organism <- metadata$organism[1]
  }
  
  # Look up organism id ####
  lookup_organism <- organism
  
  # SQL statement to look up organism in database
  statment <- SigRepoR::lookup_table_sql(
    table = "organisms", 
    return_var = c("organism_id", "organism"), 
    filter_coln_var = "organism", 
    filter_coln_val = list("organism" = lookup_organism)
  ) 
  
  # Get query table
  organism_id_tbl <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(nrow(organism_id_tbl) == 0){
    stop(sprintf("Organism = `%s` is currently not existed in our database.\n", lookup_organism),
         "You can use 'getOrganisms()' function to see a list of available organisms in our database.\n",
         "To add an organism into our database, please contact our admin for more details.\n")
  }else{
    organism_id <- organism_id_tbl$organism_id[1]
  }
  
  # Get user_id (required) ####
  user_id <- conn_info$user  
  
  # Look up signature_id in database ####
  signature_id <- c(signature_name, organism_id, direction_type, assay_type, user_id) %>% rlang::hash()
  
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
  
  if(nrow(signature_id_tbl) > 0)
    stop(sprintf("signature_name = '%s' is already existed in the database. Please choose a different name.", signature_name))
  
  # Look up platform id ####
  if("platform" %in% names(metadata)){
    
    lookup_platform <- metadata$platform[1]
    
    if(lookup_platform %in% c("", NA, NULL)){
      
      platform_id <- NULL
      
    }else{
     
      # SQL statement to look up platform in database
      statement <- SigRepoR::lookup_table_sql(
        table = "platforms", 
        return_var = "platform_id", 
        filter_coln_var = "platform_id", 
        filter_coln_val = list("platform_id" = lookup_platform)
      ) 
      
      # Get query table
      platform_id_tbl <- tryCatch({
        DBI::dbGetQuery(conn = conn, statement = statement)
      }, error = function(e){
        stop(e, "\n")
      }, warning = function(w){
        message(w, "\n")
      })
      
      if(nrow(platform_id_tbl) == 0){
        stop(sprintf("platform = `%s` is currently not existed in our database.\n", lookup_platform),
             "You can use 'getPlatforms()' function to see a list of available platforms in our database.\n",
             "To add a platform into our database, please contact our admin for more details.\n")
      }else{
        platform_id <- platform_id_tbl$platform_id[1]
      }
      
    }
    
  }else{
    
    platform_id <- NULL
    
  }
  
  # Look up phenotype id #####
  if("phenotype" %in% names(metadata)){
    
    lookup_phenotype <- metadata$phenotype[1]
    
    if(lookup_phenotype %in% c("", NA, NULL)){
    
      phenotype_id <- NULL
      
    }else{
      
      # SQL statement to look up phenotype in database
      statement <- SigRepoR::lookup_table_sql(
        table = "phenotypes", 
        return_var = "phenotype_id", 
        filter_coln_var = "phenotype_id", 
        filter_coln_val = list("phenotype_id" = lookup_phenotype)
      ) 
      
      # Get query table
      phenotype_id_tbl <- tryCatch({
        DBI::dbGetQuery(conn = conn, statement = statement)
      }, error = function(e){
        stop(e, "\n")
      }, warning = function(w){
        message(w, "\n")
      })
      
      if(nrow(phenotype_id_tbl) == 0){
        stop(sprintf("phenotype = `%s` is currently not existed in our database.\n", lookup_phenotype),
             "You can use 'getPhenotypes()' function to see a list of available phenotypes in our database.\n",
             "To add a phenotype into our database, please contact our admin for more details.\n")
      }else{
        phenotype_id <- phenotype_id_tbl$phenotype_id[1]
      }
      
    }
    
  }else{
    
    phenotype_id <- NULL
    
  }
  
  # Look up sample_type id ####
  if("sample_type" %in% names(metadata)){
    
    lookup_sample_type <- metadata$sample_type[1]
    
    if(lookup_sample_type %in% c("", NA, NULL)){
      
      sample_type_id <- NULL
      
    }else{
      
      # SQL statement to look up sample_type in database
      statement <- SigRepoR::lookup_table_sql(
        table = "sample_types", 
        return_var = "sample_type_id", 
        filter_coln_var = "sample_type_id", 
        filter_coln_val = list("sample_type_id" = lookup_sample_type)
      ) 
      
      # Get query table
      sample_type_id_tbl <- tryCatch({
        DBI::dbGetQuery(conn = conn, statement = statement)
      }, error = function(e){
        stop(e, "\n")
      }, warning = function(w){
        message(w, "\n")
      })
      
      if(nrow(sample_type_id_tbl) == 0){
        stop(sprintf("sample_type = `%s` is currently not existed in our database.\n", lookup_sample_type),
             "You can use 'getsample_types()' function to see a list of available sample_types in our database.\n",
             "To add a sample_type into our database, please contact our admin for more details.\n")
      }else{
        sample_type_id <- sample_type_id_tbl$sample_type_id[1]
      }
      
    }
    
  }else{
    
    sample_type_id <- NULL
    
  }    

  # others ####
  if(is.null(metadata$others)){
    others <- NULL
  }else{
    others <- seq_along(metadata$others) %>% 
      purrr::map2_chr(
        function(l){
          #l=1;
          list_name <- names(metadata$others)[l]
          paste0(list_name, ": ", paste0("<", metadata$others[[l]], ">", collapse = ", "))
        }
      ) %>% paste0(., collapse = "; ")
  }  

  # keywords
  keywords <- ifelse(is.null(metadata$keywords), NULL, paste0(metadata$keywords[which(!metadata$keywords %in% c("", NA, NULL))], collapse = ", "))
  
  ## PMID
  PMID <- ifelse(is.null(metadata$PMID), NULL, paste0(metadata$PMID[which(!metadata$PMID %in% c("", NA, NULL))], collapse = ", "))
  
  # year
  year <- ifelse(is.null(metadata$year), NULL, paste0(metadata$year[which(!metadata$year %in% c("", NA, NULL))], collapse = ", "))
  
  # has_difexp ####
  has_difexp <- ifelse(!is.null(difexp), 1, 0)  
  
  # Create signature metadata table ####
  table <- data.frame(
    signature_id = signature_id,
    signature_name = signature_name,
    organism_id = organism_id,
    direction_type = direction_type,
    assay_type = assay_type ,
    phenotype_id = phenotype_id,
    platform_id = platform_id,
    sample_type_id = sample_type_id,
    covariates = metadata$covariates[1],
    score_cutoff = metadata$score_cutoff[1],
    logfc_cutoff = metadata$logfc_cutoff[1],
    p_value_cutoff = metadata$p_value_cutoff[1],
    adj_p_cutoff = metadata$adj_p_cutoff[1],
    description = metadata$description[1],
    table = keywords,
    PMID = PMID,
    year = year,
    others = others,
    has_difexp = has_difexp, 
    user_id = user_id
  )
  
  # Get SQL statement to insert table into database
  statement <- SigRepoR::insert_table_sql(conn = conn, db_table_name = "signatures", table = table)
  
  # INSERT TABLE INTO DATABASE ####
  tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  # UPDATE OTHER TABLES INTO DATABASE ####
  
  # If signature has difexp, create a hash key and save a copy ####
  if(has_difexp == TRUE){
    data_path <- system.file("data/difexp", package = "SigRepoR")
    saveRDS(difexp, file = file.path(data_path, paste0(signature_id, ".RDS")))
  }
  
  # Add user to signature access table #####
  SigRepoR::addUserToSignature(
    conn = conn,
    signature_id = signature_id,
    user_id = user_id,
    access_type = "owner"
  )
  
  ## Create signature feature set table #####
  sig_feature_set <- signature %>% 
    dplyr::transmute(
      feature_name = symbol,
      orig_feature_id = id,
      score = score,
      direction = direction
    )
  
  ## Add signature feature set to database #####
  SigRepoR::addSignatureFeatureSet(
    conn = conn,
    signature_id = signature_id,
    assay_type = metadata$assay_type[1],
    sig_feature_set = sig_feature_set
  )
  
  ## Save dif_exp if provided ###
  if(!is.null(difexp)){
    base::saveRDS(difexp, file = file.path('inst/data/difexp', paste0(signature_id, ".RDS")))
  }
  
  # Add keywords to database ####
  if(!is.null(keywords)){
    keyword_tbl <- data.frame(
      keyword = keywords,
      stringsAsFactors = FALSE
    )
    SigRepoR::addKeyword(conn = conn, keyword_tbl = keyword_tbl)
  }
  
  
}







