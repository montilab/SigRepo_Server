
#' @title insert_table_sql
#' @description Insert a table into the database
#' @param conn An established database connection using newConnhandler() 
#' @param table A table in the database
#' @export
insert_table_sql <- function(conn, db_table_name, table){
  
  # Get table column names
  tbl_col_names <- colnames(table)
  
  # Join column variables
  coln_var <- paste0("(", paste0(tbl_col_names, collapse = ", "), ")")    
  
  # Get values of each row
  coln_val <- seq_len(nrow(table)) %>% 
    purrr::map_chr(
      function(r){
        #r=1;
        values <- paste0("'", table[r, tbl_col_names], "'", collapse = ", ")
        if(r < nrow(table)){
          values <- paste0("(", values, "),\n")
        }else{
          values <- paste0("(", values, ");\n")
        }
      }
    ) %>% paste0(., collapse = "") %>% gsub("'NULL'", "NULL", .)
  
  # Create a SQL query to insert table into database
  query <- sprintf(
    "
    INSERT INTO %s %s
    VALUES %s
    ", db_table_name, coln_var, coln_val
  )
  
  return(query)
  
}

#' @title update_table_sql
#' @description update a table into the database
#' @param conn An established database connection using newConnhandler() 
#' @param table A table in the database
#' @param update_coln_var A list of column variables in the given table
#' @param update_coln_val A list of column values associated with the column variables
#' @param filter_coln_var A list of column values associated with the column variables
#' @param filter_coln_val A list of column values associated with the column variables
#' @export
update_table_sql <- function(conn, table, update_coln_var, update_coln_val, filter_coln_var, filter_coln_val){
  
  update_var_list <- paste0(update_coln_var, " = '", update_col_val, "'", collapse = ", ")
  filter_var_list <- paste0(filter_coln_var, " = '", filter_coln_val, "'", collapse = " AND ")
  
  query <- sprintf(
    "
    UPDATE %s \n
    SET %s \n
    WHERE %s;
    ", table, update_var_list, filter_var_list
  )
  
  return(query)
  
}

#' @title lookup_table_sql
#' @description Look up a list of variables based on a particular variable 
#' and its associated values in the database
#' @param table A table in the database
#' @param return_var a list of column variables to be returned from the given table. 
#' Default '*' (means everything).
#' @param filter_coln_var a list of column variables in the given table. Default NULL.
#' @param filter_coln_val a list of values associated with 'filter_coln_var' variables. 
#' Most importantly, 'filter_coln_val' must have names or labels that matched the values of 'filter_coln_var'.
#' Default NULL.
#' @param filter_var_by if length(filter_coln_var) > 1, then 'filter_var_by' must be
#' provided as a vector of logical operators (e.g., OR/AND) with n = length(filter_coln_var) - 1. 
#' Default NULL.
#' @export
lookup_table_sql <- function(table_name, return_var="*", filter_coln_var=NULL, filter_coln_val=NULL, filter_var_by=NULL){
  
  # Check table_name
  stopifnot("'table_name' cannot be empty." = 
              (length(table_name) > 0 && !table_name %in% c(NA, "")))
  
  # Check return_var
  if(length(return_var) == 0 || any(return_var %in% c(NA, "", NULL)))
    stop("The 'return_var' must contains a vector of variables of ", 
         "a given table to be returned from the database.")
  
  # Check filter_coln_var and filter_coln_val
  if(length(filter_coln_var) == 0 && length(filter_coln_val) == 0){
    
    where_clause <- ""
    
  }else{  
    
    if(length(filter_coln_var) != length(filter_coln_val) || !all(names(filter_coln_val) %in% filter_coln_var))
      stop("The length of 'filter_coln_var' must equal to the length of 'filter_coln_val'. ",
           "Furthermore, 'filter_coln_val' must have names or labels that matched the values of 'filter_coln_var'.")
    
    if(length(filter_coln_var) > 1 && length(filter_var_by) != (length(filter_coln_var)-1) && !all(toupper(filter_var_by) %in% c("OR", "AND")))
      stop("'filter_var_by' must contain a vector of logical operators (e.g, AND/OR) with n = length(filter_coln_var) - 1")
    
    where_clause <- seq_along(filter_coln_var) %>% 
      purrr::map_chr(
        function(s){
          #s=1;
          clause <- sprintf("trim(lower(%s)) IN (%s)", filter_coln_var[s], paste0(paste0("'", trimws(tolower(filter_coln_val[filter_coln_var[s]])), "'"), collapse = ", "))
          if(s < length(filter_coln_var)){
            clause <- paste0(clause, " ", filter_var_by[s], " ")
          }
          return(clause)
        }
      ) %>% paste0(., collapse="") %>% paste0("WHERE ", .)
    
  }
  
  # Create a list of return variables
  return_var_list <- paste0(return_var, collapse = ", ")
  
  # Create SQL statement to return table
  query <- sprintf(
    "
    SELECT %s
    FROM %s %s
    ", return_var_list, table_name, where_clause
  )
  
  return(query)
  
}

