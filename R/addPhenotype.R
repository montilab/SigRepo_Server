
addPhenotype <- function(
    conn,
    phenotype_tbl
){
  
  # Name of table in database
  table <- "phenotypes" 
  
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
  
  # Check if phenotype_tbl is a data frame object
  if(!is(phenotype_tbl, "data.frame"))
    stop("'phenotype_tbl' must be a data frame object")
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  query_col_names <- colnames(query_tbl)[which(!colnames(query_tbl) %in% "phenotype_id")]
  
  if(any(!query_col_names %in% colnames(phenotype_tbl)))
    stop("'phenotype_tbl' must have the following column names:", paste0(query_col_names, collapse = ", "))
  
  ## Clean up the table 
  phenotype_tbl <- phenotype_tbl %>% 
    dplyr::select(query_col_names) %>% 
    dplyr::mutate(phenotype = phenotype %>% trimws() %>% gsub("'", "", .)) %>% 
    dplyr::distinct(phenotype, .keep_all = TRUE) %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'")
  
  ## If table has values, only import non-existing ones
  if(nrow(table_query) > 0){
    phenotype_tbl <- phenotype_tbl %>% 
      dplyr::anti_join(
        query_tbl,
        by = "phenotype"
      )
  }
  
  if(nrow(phenotype_tbl) == 0) return(NULL)
  
  # Join column variables
  coln_var <- paste0("(", paste0(query_col_names, collapse = ", "), ")")    
  
  # Get values
  if(nrow(phenotype_tbl) == 1){
    
    values <- paste0(
      "(", 
      paste0("'", phenotype_tbl$phenotype[nrow(phenotype_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      ");\n"
    )
    
    # Join column values
    coln_val <- paste0(values, collapse = "") %>% gsub("'NULL'", "NULL", .)
    
  }else{
    
    first_values <- paste0(
      "(", 
      paste0("'", phenotype_tbl$phenotype %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"),
      "),\n"
    )
    
    last_values <- paste0(
      "(", 
      paste0("'", phenotype_tbl$phenotype[nrow(phenotype_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      ");\n"
    )
    
    # Join column values
    coln_val <- paste0(c(first_values, last_values), collapse = "") %>% gsub("'NULL'", "NULL", .)
    
  }
  
  # Insert values into table
  insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
  
}



