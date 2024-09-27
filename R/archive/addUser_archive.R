#' @title addUser
#' @description Add user information to database
#' @param conn An established database connection using newConnhandler() 
#' @param access_tbl A data frame containing appropriate column names
#' @export
addUser <- function(
    conn,
    user_tbl
){
  
  # Table name in database
  table <- "users"
  
  # Check if conn is a MySQLConnection class object
  if(!is(conn, "MySQLConnection"))
    stop("'conn' must be a MySQLConnection object obtained from newConnHandler()") 
  
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
  
  # Get number of observations
  statement <- sprintf("SELECT COUNT(*) AS count FROM %s", table)
  n_obs <- DBI::dbGetQuery(conn = conn, statement = statement)
  
  # Get column fields
  tbl_col_names <- c('user_name', 'user_password', 'user_email', 'user_first', 'user_last', 'user_affiliation', 'user_role')
  
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
  if(n_obs$count > 0){
    existing_tbl <- lookup_values_sql(conn=conn, table=table, coln_var="user_id", coln_val=user_tbl$user_id)
    if(nrow(existing_tbl) > 0){
      user_tbl <- user_tbl %>% 
        dplyr::mutate(unique_id = tolower(user_id)) %>% 
        dplyr::anti_join(
          existing_tbl %>% dplyr::mutate(unique_id = tolower(user_id)),
          by = "unique_id"
        )    
      if(nrow(user_tbl) == 0) return(NULL)
    }    
  }
  
  ## Create final column names 
  col_names <- c("user_id", "user_password_hashkey", 'user_email', 'user_first', 'user_last', 'user_affiliation', 'user_role')
  
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



