
addUser <- function(
  conn,
  user_tbl
){
  
  # Table name in database
  table <- "users"
  
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
  
  # Check if user_tbl is a data frame  object
  if(!is(user_tbl, "data.frame"))
    stop("'user_tbl' must be a data frame object")
  
  # Get column fields
  table_query <- sprintf("SELECT * FROM %s LIMIT 1", table)
  query_tbl <- DBI::dbGetQuery(conn = conn, statement = table_query)
  query_col_names <- colnames(query_tbl)[which(!colnames(query_tbl) %in% c("user_id", "user_password_hashkey"))]
  tbl_col_names <- c("user_name", "user_password", query_col_names)
  
  if(any(!tbl_col_names %in% colnames(user_tbl)))
    stop("'user_tbl' must have the following column names: ", paste0(tbl_col_names, collapse = ", "))
  
  ## Clean up the table 
  user_tbl <- user_tbl %>% 
    dplyr::select(all_of(tbl_col_names)) %>% 
    dplyr::mutate(
      user_id = user_name %>% trimws() %>% gsub("'", "", .),
      user_password_hashkey = sodium::password_store(as.character(user_password)),
    ) %>% 
    dplyr::distinct(user_id, user_email, user_role, .keep_all = TRUE) %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'")
  
  ## If table has values, only import non-existing ones
  if(nrow(query_tbl) > 0){
    
    user_tbl <- user_tbl %>% 
      dplyr::anti_join(
        query_tbl,
        by = c("user_id", "user_email", "user_role")
      )  
    
    if(nrow(user_tbl) == 0) return(NULL)

  }
  
  ## Create final column names 
  col_names <- c("user_id", "user_password_hashkey", query_col_names)
  
  # Join column variables
  coln_var <- paste0("(", paste0(col_names, collapse = ", "), ")")
  
  # Get values
  if(nrow(user_tbl) == 1){
    
    values <- paste0(
      "(", 
      paste0("'", user_tbl$user_id[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_password_hashkey[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_email[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_first[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_last[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_affiliation[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      paste0("'", user_tbl$user_role[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      ");\n"
    )
    
    # Join column values
    coln_val <- paste0(values, collapse = "") %>% gsub("'NULL'", "NULL", .)   
    
  }else{
  
    first_values <- paste0(
      "(", 
      paste0("'", user_tbl$user_id %>% utils::head(n = -1)  %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_password_hashkey %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_email %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_first %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_last %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_affiliation %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_role %>% utils::head(n = -1) %>% trimws() %>% gsub("'", "", .), "'"),
      "),\n"
    )
    
    last_values <- paste0(
      "(", 
      paste0("'", user_tbl$user_id[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_password_hashkey[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_email[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_first[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_last[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",",  
      paste0("'", user_tbl$user_affiliation[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"), ",", 
      paste0("'", user_tbl$user_role[nrow(user_tbl)] %>% trimws() %>% gsub("'", "", .), "'"),
      ");\n"
    )
    
    # Join column values
    coln_val <- paste0(c(first_values, last_values), collapse = "") %>% gsub("'NULL'", "NULL", .)   
    
  }
  
  # Insert values into table
  insert_table_sql(conn = conn, table = table, coln_var = coln_var, coln_val = coln_val)
  
}



