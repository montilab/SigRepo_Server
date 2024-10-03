#' @title addSignature
#' @description Add a signature to signature table of the database
#' @param conn An established connection to database using newConnhandler() 
#' @param omic_signature An R6 class object from OmicSignature package
#' @export
addSignature <- function(
    conn,
    omic_signature
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "user"
  )
  
  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Check if omic_signature is an OmicSignature class object ####
  if(!is(omic_signature, "OmicSignature"))
    stop("'omic_signature' must be an R6 class object from OmicSignature package.") 
  
  # Check metadata and signature
  if(!"metadata" %in% names(omic_signature))
    stop("'omic_signature' must contain a metadata object.\n")
  
  if(!"signature" %in% names(omic_signature))
    stop("'omic_signature' must contain a signature object.\n")
  
  # Extract metadata and signature table from omic_signature ####
  metadata <- omic_signature$metadata; signature <- omic_signature$signature; 
  
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
  assay_type_options <- c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites")
  
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
  
  organism_id_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "organisms", 
    return_var = c("organism_id", "organism"), 
    filter_coln_var = "organism", 
    filter_coln_val = list("organism" = lookup_organism),
    check_db_table = TRUE
  ) 
  
  if(nrow(organism_id_tbl) == 0){
    SigRepo::addOrganismErrorMessage(
      db_table_name = "organisms",
      unknown_values = lookup_organism
    )
  }else{
    organism_id <- organism_id_tbl$organism_id[1]
  }
  
  # Get user_id (required) ####
  user_id <- conn_info$user  
  
  # Create a hash key to look up signature in the database ####
  signature_hashkey <- digest::digest(paste0(signature_name, organism_id, direction_type, assay_type, user_id), algo = "md5", serialize = FALSE)
  
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "signature_hashkey",
    filter_coln_val = list("signature_hashkey" = signature_hashkey),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, return the signature table
  if(nrow(signature_tbl) > 0){
    
    return(signature_tbl)
    
  }else{
    
    # Look up platform id ####
    if("platform" %in% names(metadata)){
      
      lookup_platform <- metadata$platform[1]
      
      if(lookup_platform %in% c("", NA, NULL)){
        
        platform_id <- 'NULL'
        
      }else{
        
        # SQL statement to look up platform in database
        platform_id_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
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
        
      }
      
    }else{
      
      platform_id <- 'NULL'
      
    }
    
    # Look up phenotype id #####
    if("phenotype" %in% names(metadata)){
      
      lookup_phenotype <- metadata$phenotype[1]
      
      if(lookup_phenotype %in% c("", NA, NULL)){
        
        phenotype_id <- 'NULL'
        
      }else{
        
        phenotype_id_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "phenotypes", 
          return_var = "phenotype_id", 
          filter_coln_var = "phenotype", 
          filter_coln_val = list("phenotype" = lookup_phenotype),
          check_db_table = TRUE
        ) 
        
        if(nrow(phenotype_id_tbl) == 0){
          SigRepo::addPhenotypeErrorMessage(
            db_table_name = "phenotypes",
            unknown_values = lookup_phenotype
          )
        }else{
          phenotype_id <- phenotype_id_tbl$phenotype_id[1]
        }
        
      }
      
    }else{
      
      phenotype_id <- 'NULL'
      
    }
    
    # Look up sample_type id ####
    if("sample_type" %in% names(metadata)){
      
      lookup_sample_type <- metadata$sample_type[1]
      
      if(lookup_sample_type %in% c("", NA, NULL)){
        
        sample_type_id <- 'NULL'
        
      }else{
        
        sample_type_id_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "sample_types", 
          return_var = "sample_type_id", 
          filter_coln_var = "sample_type", 
          filter_coln_val = list("sample_type" = lookup_sample_type),
          check_db_table = TRUE
        ) 
        
        if(nrow(sample_type_id_tbl) == 0){
          SigRepo::addSampleTypeErrorMessage(
            db_table_name = "sample_types",
            unknown_values = lookup_sample_type
          )
        }else{
          sample_type_id <- sample_type_id_tbl$sample_type_id[1]
        }
        
      }
      
    }else{
      
      sample_type_id <- 'NULL'
      
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
    
    ## PMID
    if("PMID" %in% names(metadata)){
      if(length(metadata$PMID) == 0){
        PMID <- 'NULL'
      }else{
        PMID <- paste0(metadata$PMID, collapse = ", ")
      }
    }else{
      PMID <- 'NULL'
    }
    
    # year
    if("year" %in% names(metadata)){
      if(length(metadata$year) == 0){
        year <- 'NULL'
      }else{
        year <- paste0(metadata$year, collapse = ", ")
      }
    }else{
      year <- 'NULL'
    }
    
    # author
    if("author" %in% names(metadata)){
      if(length(metadata$author) == 0){
        author <- 'NULL'
      }else{
        author <- paste0(metadata$author, collapse = ", ")
      }
    }else{
      author <- 'NULL'
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
              paste0(list_name, ": ", paste0("<", metadata$others[[l]], ">", collapse = ", "))
            }
          ) %>% paste0(., collapse = "; ")
      }
    }else{
      others <- 'NULL'
    }  
    
    # keywords
    if("keywords" %in% names(metadata)){
      if(length(metadata$keywords) == 0){
        keywords <- 'NULL'
      }else{
        keywords <- paste0(metadata$keywords, collapse = ", ")
        # Add keywords to database ####
        message("Adding keywords to database...\n")
        keyword_tbl <- data.frame(
          keyword = metadata$keywords,
          stringsAsFactors = FALSE
        )
        SigRepo::addKeyword(conn = conn, keyword_tbl = keyword_tbl)
      }
    }else{
      keywords <- 'NULL'
    }
    
    # has_difexp ####
    has_difexp <- ifelse(!is.null(difexp), 1, 0)  
    
    # If signature has difexp, save a copy with its hash key ####
    # This action must be performed before a signature is imported into the database.
    # This helps to make sure data is stored properly before interruptions in-between.
    if(has_difexp == TRUE){
      data_path <- system.file("data/difexp", package = "SigRepoR")
      saveRDS(difexp, file = file.path(data_path, paste0(signature_hashkey, ".RDS")))
    }
    
    # Create signature metadata table ####
    table <- data.frame(
      signature_name = signature_name,
      organism_id = organism_id,
      direction_type = direction_type,
      assay_type = assay_type ,
      phenotype_id = phenotype_id,
      platform_id = platform_id,
      sample_type_id = sample_type_id,
      covariates = covariates,
      score_cutoff = score_cutoff,
      logfc_cutoff = logfc_cutoff,
      p_value_cutoff = p_value_cutoff,
      adj_p_cutoff = adj_p_cutoff,
      description = description,
      keywords = keywords,
      PMID = PMID,
      year = year,
      author = author,
      others = others,
      has_difexp = has_difexp, 
      user_id = user_id,
      signature_hashkey = signature_hashkey
    )
    
    # Check table against database table ####
    table <- SigRepo::checkTableInput(
      conn = conn, 
      db_table_name = db_table_name,
      table = table, 
      exclude_coln_names = c("signature_id", "date_created"),
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      table = table,
      check_db_table = FALSE
    ) 
    
    # Get signature id from the imported table
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      return_var = "*", 
      filter_coln_var = "signature_hashkey",
      filter_coln_val = list("signature_hashkey" = signature_hashkey),
      check_db_table = FALSE
    ) 
    
    return(signature_tbl)
    
  }
  
}  
  
  
  
  
  

