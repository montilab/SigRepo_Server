
addKeyword <- function(
  conn,
  keyword_tbl
){
  
  # Name of table in database
  table <- "keywords" 
  
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
  
  # Check if keyword_tbl is a data frame object
  if(!is(keyword_tbl, "data.frame"))
    stop("'keyword_tbl' must be a data frame object")
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  query_col_names <- colnames(query_tbl)[which(!colnames(query_tbl) %in% "keyword_id")]
  
  if(any(!query_col_names %in% colnames(keyword_tbl)))
    stop("'keyword_tbl' must have the following column names:", paste0(query_col_names, collapse = ", "))
  
  ## Clean up the table 
  keyword_tbl <- keyword_tbl %>% 
    dplyr::select(query_col_names) %>% 
    dplyr::mutate(keyword = keyword %>% trimws() %>% gsub("'", "", .)) %>% 
    dplyr::distinct(keyword, .keep_all = TRUE) %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'")
  
  ## If table has values, only import non-existing ones
  if(nrow(table_query) > 0){
    keyword_tbl <- keyword_tbl %>% 
      dplyr::anti_join(
        query_tbl,
        by = "keyword"
      )
  }
  
  if(nrow(keyword_tbl) == 0) return(NULL)
  
  # Join column variables
  coln_var <- paste0("(", paste0(query_col_names, collapse = ", "), ")")    
  
  # Get values
  if(nrow(keyword_tbl) == 1){
    
    values <- paste0(
      "(", 
      paste0("'", keyword_tbl$keyword[nrow(keyword_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      ");\n"
    )
    
    # Join column values
    coln_val <- paste0(values, collapse = "") %>% gsub("'NULL'", "NULL", .)
    
  }else{
    
    first_values <- paste0(
      "(", 
      paste0("'", keyword_tbl$keyword %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"),
      "),\n"
    )
    
    last_values <- paste0(
      "(", 
      paste0("'", keyword_tbl$keyword[nrow(keyword_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      ");\n"
    )
    
    # Join column values
    coln_val <- paste0(c(first_values, last_values), collapse = "") %>% gsub("'NULL'", "NULL", .)
    
  }
  
  # Insert values into table
  insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
  
}



