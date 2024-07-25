
addTranscriptomicsFeatureSet <- function(
    conn,
    feature_set
){
  
  # Name of table in database
  table <- "transcriptomics_proteomics_features" 
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  query_col_names <- colnames(query_tbl)[which(!colnames(query_tbl) %in% c("feature_id", "organism_id"))]
  tbl_col_names <- c(query_col_names, "organism")
  
  if(any(!tbl_col_names %in% colnames(feature_set)))
    stop("'feature_set' must have the following column names: ", paste0(tbl_col_names, collapse = ", "))
  
  ## Clean up the table 
  feature_set <- feature_set %>% 
    dplyr::select(all_of(tbl_col_names)) %>% 
    dplyr::mutate(
      feature_name = feature_name %>% trimws() %>% gsub("'", "", .)
    ) %>% 
    dplyr::distinct(feature_name, .keep_all = TRUE) %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'")
  
  # Getting the unique organisms
  unique_organism <- unique(feature_set$organism)
  
  # Read in the organism table
  organism_id_tbl <- lookup_values_sql(conn = conn, table="organisms", id_var="organism_id", coln_var="organism", coln_val=unique_organism) %>% 
    dplyr::distinct(organism, organism_id, .keep_all = TRUE)
  
  ## Retrieve the organism ids
  if(nrow(organism_id_tbl) > 0){
    
    ## Add organism ids to table
    feature_set <- feature_set %>% 
      dplyr::left_join(organism_id_tbl, by="organism") %>% 
      base::replace(is.na(.), "'NULL'") %>% 
      base::replace(. == "", "'NULL'")
    
    ## If table has values, only import non-existing ones
    if(nrow(query_tbl) > 0){
      
      feature_set <- feature_set %>% 
        dplyr::anti_join(
          query_tbl,
          by = "feature_name"
        )
      
      if(nrow(feature_set) == 0) return(NULL)
      
    }
    
    ## Create final column names 
    col_names <- c("organism_id", query_col_names)
    
    # Join column variables
    coln_var <- paste0("(", paste0(col_names, collapse = ", "), ")")
    
    # Get values
    if(nrow(organism_tbl) == 1){
      
      values <- paste0(
        "(", 
        paste0("'", feature_set$organism_id[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$feature_name[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$description[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$synonyms[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$n_synonyms[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$ensemble_ids[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$n_ensembl_ids[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$transcript_biotypes[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$chromosome_name[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$start_position[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$end_position[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"),
        ");\n"
      )
      
      # Join column values
      coln_val <- paste0(values, collapse = "") %>% gsub("'NULL'", "NULL", .)    
      
    }else{
      
      first_values <- paste0(
        "(", 
        paste0("'", feature_set$organism_id %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$feature_name %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$description %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$synonyms %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$n_synonyms %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$ensemble_ids %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$n_ensembl_ids %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$transcript_biotypes %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$chromosome_name %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$start_position %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$end_position %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"),
        "),\n"
      )
      
      last_values <- paste0(
        "(", 
        paste0("'", feature_set$organism_id[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$feature_name[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$description[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$synonyms[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$n_synonyms[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$ensemble_ids[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$n_ensembl_ids[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$transcript_biotypes[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$chromosome_name[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$start_position[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", feature_set$end_position[nrow(feature_set)] %>% trimws() %>% gsub("'", "", .), "'"),
        ");\n"
      )
      
      # Join column values
      coln_val <- paste0(c(first_values, last_values), collapse = "") %>% gsub("'NULL'", "NULL", .)
      
    }
    
    # Insert values into table
    insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
    
  }
}


