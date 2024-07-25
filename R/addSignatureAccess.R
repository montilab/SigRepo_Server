
addSignatureAccess <- function(
    conn,
    signature_id,
    user_id,
    access_type = c("admin", "owner", "viewer")
){
  
  # Table name in database
  table <- "access"
  
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

  # Check access type
  access_type <- match.arg(access_type)
  
  ## create access table
  access_tbl <- data.frame(
    signature_id = signature_id,
    user_id = user_id,
    access_type = access_type
  )
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  query_col_names <- colnames(query_tbl)
  
  ## If table has values, only import non-existing ones
  if(nrow(table_query) > 0){
    access_tbl <- access_tbl %>% 
      dplyr::anti_join(
        query_tbl,
        by = c("signature_id", "user_id", "access_type")
      )
  }

  if(nrow(access_tbl) > 0){
    
    ## Create final column names 
    col_names <- c("access_id", "access_password_hashkey", query_col_names)
    
    # Join column variables
    coln_var <- paste0("(", paste0(col_names, collapse = ", "), ")")
    
    # Get values
    if(nrow(access_tbl) == 1){
      
      values <- paste0(
        "(", 
        paste0("'", access_tbl$access_id[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", access_tbl$access_password_hashkey[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", access_tbl$access_email[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_first[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_last[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_affiliation[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
        ");\n"
      )
      
      # Join column values
      coln_val <- paste0(values, collapse = "") %>% gsub("'NULL'", "NULL", .)   
      
    }else{
      
      first_values <- paste0(
        "(", 
        paste0("'", access_tbl$access_id %>% utils::head(n = -1)  %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", access_tbl$access_password_hashkey %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", access_tbl$access_email %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_first %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_last %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_affiliation %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"),
        "),\n"
      )
      
      last_values <- paste0(
        "(", 
        paste0("'", access_tbl$access_id[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", access_tbl$access_password_hashkey[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
        paste0("'", access_tbl$access_email[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_first[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_last[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
        paste0("'", access_tbl$access_affiliation[nrow(access_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
        ");\n"
      )
      
      # Join column values
      coln_val <- paste0(c(first_values, last_values), collapse = "") %>% gsub("'NULL'", "NULL", .)   
      
    }
    
    # Insert values into table
    insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
    
  }
}



