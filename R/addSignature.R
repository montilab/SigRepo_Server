
addSignature <- function(
    conn,
    omic_signature,
    user_id,
    user_role
){
  
  # Name of table in database
  table <- "signatures"
  
  # Check if table exists in database
  all_tables <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = "show tables;")
  }, error = function(e){
    stop(e)
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(!table %in% all_tables[,1])
    stop(sprintf("There is no '%s' table in the database.", table))
  
  # Check if omic_signature is an OmicSignature class object
  if(!is(omic_signature, "OmicSignature"))
    stop("'omic_signature' must be an R6 class object from OmicSignature package.")  
  
  # Check metadata and signature
  if(!"metadata" %in% names(omic_signature))
    stop("'omic_signature' must contain a metadata object.\n")
  
  if(!"signature" %in% names(omic_signature))
    stop("'omic_signature' must contain a signature object.\n")
  
  metadata <- omic_signature$metadata # required
  signature <- omic_signature$signature # required
  
  if(!is(metadata, "list"))
    stop("'metadata' in OmicSignature must be a list.")
  
  if(!is(signature, "data.frame"))
    stop("'signature' in OmicSignature must be a data frame.")
  
  # Check difexp
  if("difexp" %in% names(omic_signature)){
    difexp <- omic_signature$difexp
    if(is.null(difexp)){
      difexp <- NULL
    }else{
      if(!is(difexp, "data.frame"))
        stop("'difexp' in OmicSignature must be a data frame.")
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
  
  # Check required difexp fields
  difexp_fields <- c('id', 'symbol', 'score', 'p_value')
  
  if(!is.null(difexp) && any(!difexp_fields %in% colnames(difexp)))
    stop("'difexp' in OmicSignature must have the following column names:", paste0(difexp_fields, collapse = ", "))
  
  # Check check signature name
  if(metadata$signature_name[1] %in% c(NA, "", NULL))
    stop("'signature_name' in OmicSignature's metadata cannot be empty.")

  # Check check signature name
  if(metadata$organism[1] %in% c(NA, "", NULL))
    stop("'organism' in OmicSignature's metadata cannot be empty.")
  
  # Check direction_type
  direction_type_options <- c("uni-directional", "bi-directional", "multiple")
  
  if(!metadata$direction_type[1] %in% direction_type_options)
    stop("'direction_type' in OmicSignature's metadata object must be: \n", paste0(direction_type_options, collapse = "/"))
  
  # Check assay_type
  assay_type_options <- c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "DNA_binding_sites", "others")
  
  if(!metadata$assay_type[1] %in% assay_type_options)
    stop("'assay_type' in OmicSignature's metadata object must be: ", paste0(assay_type_options, collapse = "/"))
  
  # Lookup IDs
  lookup_organism <- metadata$organism[1] %>% trimws() %>% gsub("'", "", .)
  
  organism_id_tbl <- lookup_id_sql(
    conn = conn, 
    table = "organisms",
    id_var = "organism_id",
    coln_var = "organism",
    coln_val = lookup_organism
  )
  
  if(nrow(organism_id_tbl) == 0){
    
    addOrganism(conn = conn, organism_tbl = as.data.frame(organism = lookup_organism))
    
    organism_id <- lookup_id_sql(
      conn = conn, 
      table = "organisms",
      id_var = "organism_id",
      coln_var = "organism",
      coln_val = lookup_organism
    ) %>% unlist()
    
  }else{
    
    organism_id <- organism_id_tbl$organism_id
    
  }

  if("platforms" %in% names(metadata)){
    
    lookup_platform_id <- ifelse(metadata$platform[1] %in% c(NA, "", NULL), "GPLXXXXX", metadata$platform[1])
    
    platform_id_tbl <- lookup_id_sql(
      conn = conn, 
      table = "platforms",
      id_var = "platform_id",
      coln_var = c("platform_id", "organism_id"),
      coln_val = c(lookup_platform_id, organism_id)
    )
    
    if(nrow(platform_id_tbl) == 0){
      
      platform_tbl <- data.frame(
        platform_id = lookup_platform_id,
        platform = "unknown",
        seq_technology = "unknown",
        organism_id = organism_id
      )
      
      addPlatform(conn = conn, platform_tbl = platform_tbl)
      
      platform_id <- lookup_id_sql(
        conn = conn, 
        table = "platforms",
        id_var = "platform_id",
        coln_var = c("platform_id", "organism_id"),
        coln_val = c(lookup_platform_id, organism_id)
      ) %>% unlist()
      
    }else{
      
      platform_id <- platform_id_tbl$platform_id
      
    }
    
  }else{
    
    platform_id <- "GPLXXXXX"
    
  }
  
  if("phenotype" %in% names(metadata)){
    
    lookup_phenotype <- ifelse(metadata$phenotype[1] %in% c(NA, "", NULL), "unknown", metadata$phenotype[1])
    
    phenotype_id_tbl <- lookup_id_sql(
      conn = conn, 
      table = "phenotypes",
      id_var = "phenotype_id",
      coln_var = "phenotype",
      coln_val = lookup_phenotype
    )  
    
    if(nrow(phenotype_id_tbl) == 0){
      
      addPhenotype(conn = conn, phenotype_tbl = as.data.frame(phenotype = lookup_phenotype))
      
      phenotype_id <- lookup_id_sql(
        conn = conn, 
        table = "phenotypes",
        id_var = "phenotype_id",
        coln_var = "phenotype",
        coln_val = lookup_phenotype,
      ) %>% unlist()
      
    }else{
      
      phenotype_id <- phenotype_id_tbl$phenotype_id
      
    }
    
  }else{
    
    phenotype_id <- NULL
    
  }
  
  if("sample_type" %in% names(metadata)){
    
    lookup_sample_type <- ifelse(metadata$sample_type[1] %in% c(NA, "", NULL), "unknown", metadata$sample_type[1])
    
    sample_type_id_tbl <- lookup_id_sql(
      conn = conn,
      table = "sample_types",
      id_var = "sample_type_id",
      coln_var = "sample_type",
      coln_val = lookup_sample_type
    )
    
    if(nrow(sample_type_id_tbl) == 0){
      
      sample_type_tbl <- data.frame(
        sample_type = lookup_sample_type,
        brenda_accession = NULL,
        stringsAsFactors = FALSE
      )
      
      addSampleType(conn = conn, sample_type_tbl = sample_type_tbl)
      
      sample_type_id <- lookup_id_sql(
        conn = conn,
        table = "sample_types",
        id_var = "sample_type_id",
        coln_var = "sample_type",
        coln_val = lookup_sample_type
      ) %>% unlist()
      
    }else{
      
      sample_type_id <- phenotype_id_tbl$phenotype_id
      
    }
    
  }else{
    
    sample_type_id <- NULL
    
  }    
    
  signature_name <- metadata$phenotype[1] %>% trimws() %>% gsub("'", "", .)
  
  signature_id_lookup <- lookup_id_sql(
    conn = conn,
    table = "signatures",
    id_var = "signature_id",
    coln_var = c('signature_name', 'organism_id', 'direction_type', 'assay_type', "user_id"),
    coln_val = c(metadata$signature_name[1], organism_id, metadata$direction_type[1], metadata$assay_type[1], user_id)
  )
  
  if(nrow(signature_id_lookup) > 0)
    stop("'signature_name' is already existed in the database. Please choose a different name.")
  
  # Clean variables
  signature_name <- paste0("'", metadata$signature_name, "'") 
  direction_type <- paste0("'", metadata$direction_type, "'") 
  assay_type <- paste0("'", metadata$assay_type, "'") 
  logfc_cutoff <- ifelse(is.null(metadata$logfc_cutoff), "'NULL'", paste0("'", metadata$logfc_cutoff, "'"))
  p_value_cutoff <- ifelse(is.null(metadata$p_value_cutoff), "'NULL'", paste0("'", metadata$p_value_cutoff, "'"))
  adj_p_cutoff <- ifelse(is.null(metadata$adj_p_cutoff), "'NULL'", paste0("'", metadata$adj_p_cutoff, "'"))
  score_cutoff <- ifelse(is.null(metadata$score_cutoff), "'NULL'", paste0("'", metadata$score_cutoff, "'"))
  keywords <- ifelse(is.null(metadata$keywords), "'NULL'", paste0("'", paste0("<", metadata$keywords, ">", collapse = "; "), "'"))
  cutoff_description <- ifelse(is.null(metadata$cutoff_description), "'NULL'", paste0("'", metadata$cutoff_description, "'"))
  PMID <- ifelse(is.null(metadata$PMID), "'NULL'", paste0("'", metadata$PMID, "'"))
  others <- ifelse(is.null(metadata$others), "'NULL'", paste0("'", paste0("<", metadata$keywords, ">", collapse = "; "), "'"))
  has_difexp <- ifelse(!is.null(difexp), 1, 0)
  uploaded_date <- paste0("'", as.Date(Sys.Date(), format="yyy-mm-dd"), "'")
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  col_names <- colnames(query_tbl)[which(!colnames(query_tbl) %in% "signature_id")]
  
  # Combine all values for each field
  val_names <- c(
    'signature_name', 'organism_id', 'direction_type', 'assay_type', 
    'phenotype_id', 'platform_id', 'sample_type_id', 'covariates', 
    'score_cutoff', 'logfc_cutoff', 'p_value_cutoff', 
    'adj_p_cutoff', 'cutoff_description', 'keywords', 'others', 
    'PMID', 'year', "has_difexp", "user_id", "uploaded_date"
  )
  
  values <- c(
    signature_name,
    organism_id,
    direction_type,
    assay_type,
    phenotype_id,
    platform_id,
    sample_type_id,
    covariates,
    score_cutoff,
    logfc_cutoff,
    p_value_cutoff,
    adj_p_cutoff,
    cutoff_description,
    keywords,
    others,
    PMID,
    year,
    has_difexp,
    user_id,
    uploaded_date
  )
  
  # Join column variables
  coln_var <- paste0("(", paste0(col_names, collapse = ", "), ")")
  
  # Join column values
  coln_val <- paste0("(", paste0(values, collapse = ", ") %>% gsub("'NULL'", "NULL", .), ");")
  
  # Insert values into table
  insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
  
  # Add keywords to database
  if(length(metadata$keywords) > 0){
    keyword_tbl <- data.frame(
      keyword = metadata$keywords,
      stringsAsFactors = FALSE
    )
    addKeyword(conn = conn, keyword_tbl = keyword_tbl)
  }

  # Add access signature to database
  access_tbl <- data.frame(
    user_id = user_id,
    signature_id = signature_id,
    access_type = ifelse(user_role == "admin", "admin", "owner"),
    stringsAsFactors = FALSE
  )
  
  addSignatureAccess(conn = conn, keyword_tbl = keyword_tbl)
  
}







