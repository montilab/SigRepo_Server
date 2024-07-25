
# Insert table into database
insert_table_sql <- function(conn, table, coln_var, coln_val){
  
  query <- sprintf(
    "
    INSERT INTO %s %s
    VALUES 
    %s
    ", table, coln_var, coln_val
  )
  
  DBI::dbGetQuery(conn = conn, statement = query)
  
}


## Look up a variable id from a table
lookup_id_sql <- function(conn, table, id_var, coln_var, coln_val){
  
  where_clause <- paste0("trim(lower(", coln_var, ")) = trim(lower('", coln_val, "'))", collapse = " AND ")
  
  query <- sprintf(
    "
    SELECT %s 
    FROM %s 
    WHERE %s
    ", id_var, table, where_clause
  )
  
  DBI::dbGetQuery(conn = conn, statement = query)
  
}

## Look up a list of values
lookup_values_sql <- function(conn, table, id_var, coln_var, coln_val){
  
  where_clause <- sprintf("WHERE %s in (%s)", coln_var, paste0(paste0("'", coln_val, "'"), collapse = ", "))
  
  query <- sprintf(
    "
    SELECT %s, %s 
    FROM %s 
    %s
    ", coln_var, id_var, table, where_clause
  )
  
  DBI::dbGetQuery(conn = conn, statement = query)
  
}


