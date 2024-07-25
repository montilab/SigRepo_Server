
addPlatform <- function(
  conn,
  platform_tbl
){
  
  # Name of table in database
  table <- "platforms" 
  
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
  
  # Check if platform_tbl is a data frame object
  if(!is(platform_tbl, "data.frame"))
    stop("'platform_tbl' must be a data frame object")
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  query_col_names <- colnames(query_tbl)[which(!colnames(query_tbl) %in% "organism_id")]
  tbl_col_names <- c(query_col_names, "organism")
  
  if(any(!tbl_col_names %in% colnames(platform_tbl)))
    stop("'platform_tbl' must have the following column names: ", paste0(tbl_col_names, collapse = ", "))
  
  ## Clean up the table 
  platform_tbl <- platform_tbl %>% 
    dplyr::select(all_of(tbl_col_names)) %>% 
    dplyr::mutate(
      platform = platform %>% trimws() %>% gsub("'", "", .),
      organism = organism %>% trimws() %>% gsub("'", "", .)
    ) %>% 
    dplyr::distinct(platform, .keep_all = TRUE) %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'")
  
  # Getting the unique organisms
  unique_organism <- unique(platform_tbl$organism)
  
  # Read in the organism table
  organism_id_tbl <- lookup_values_sql(conn = conn, table="organisms", id_var="organism_id", coln_var="organism", coln_val=unique_organism) %>% 
    dplyr::distinct(organism, organism_id, .keep_all = TRUE)
  
  ## Retrieve the organism ids
  if(nrow(organism_id_tbl) > 0){
    
    ## Add organism ids to table
    platform_tbl <- platform_tbl %>% 
      dplyr::left_join(organism_id_tbl, relationship = "many-to-many", by="organism") %>% 
      base::replace(is.na(.), "'NULL'") %>% 
      base::replace(. == "", "'NULL'")
    
    ## If table has values, only import non-existing ones
    if(nrow(query_tbl) > 0){
      
      platform_tbl <- platform_tbl %>% 
        dplyr::anti_join(
          query_tbl,
          by = "platform"
        )    
      
      if(nrow(platform_tbl) == 0) return(NULL)

    }
    
    ## Create final column names 
    col_names <- c(query_col_names, "organism_id")
    
    # Join column variables
    coln_var <- paste0("(", paste0(col_names, collapse = ", "), ")")
    
    # Get values
    if(nrow(platform_tbl) == 1){
      
      values <- paste0(
        "(", 
        paste0("'", platform_tbl$platform_id[nrow(platform_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", platform_tbl$platform[nrow(platform_tbl)] %>% trimws()  %>% gsub("'", "", .), "'"), ",",
        paste0("'", platform_tbl$seq_technology[nrow(platform_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", platform_tbl$organism_id[nrow(platform_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
        ");\n"
      )
      
      # Join column values
      coln_val <- paste0(last_values, collapse = "") %>% gsub("'NULL'", "NULL", .)
      
    }else{
      
      first_values <- paste0(
        "(", 
        paste0("'", platform_tbl$platform_id %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", platform_tbl$platform %>% utils::head(n = -1) %>% trimws()  %>% gsub("'", "", .), "'"), ",",
        paste0("'", platform_tbl$seq_technology %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", platform_tbl$organism_id %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), 
        "),\n"
      )
      
      last_values <- paste0(
        "(", 
        paste0("'", platform_tbl$platform_id[nrow(platform_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", platform_tbl$platform[nrow(platform_tbl)] %>% trimws()  %>% gsub("'", "", .), "'"), ",",
        paste0("'", platform_tbl$seq_technology[nrow(platform_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",
        paste0("'", platform_tbl$organism_id[nrow(platform_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
        ");\n"
      )
      
      # Join column values
      coln_val <- paste0(c(first_values, last_values), collapse = "") %>% gsub("'NULL'", "NULL", .)
      
    }
    
    # Insert values into table
    insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
    
  }
}



