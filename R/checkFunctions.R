#' @title checkTableVar
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
checkTableVar <- function(
    conn,
    table,
    return_var,
    filter_coln_var,
    filter_coln_val
){
  
  # Look up variable in database
  statement <- SigRepoR::lookup_table_sql(
    conn = conn,
    table = table,
    return_var = return_var,
    filter_coln_var = coln_var,
    filter_coln_val = coln_val,
    filter_var_by = NULL
  )
  
  tbl <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(nrow(result) == 0)
    stop(sprintf("Cannot find %s = '%s' in the database.", coln_var, coln_val))

}


#' @title checkTableInput
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' 
#' @noRd
#' 
#' @export
checkConnection <- function(
    conn
){
  
  # Check if conn is a MySQLConnection class object
  if(!is(conn, "MySQLConnection"))
    stop("'conn' must be a MySQLConnection object obtained from newConnHandler()") 
  
  # Check user connection info
  conn_info <- DBI::dbGetInfo(conn)
  
  # Check if user has a root access to add data into database 
  if(!conn_info$user == "root")
    stop(sprintf("User = '%s' does not have permission to add data into the database", conn_info$user)) 
  
  return(conn_info)
  
}


#' @title checkTableInput
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
checkTableInput <- function(
  conn,
  database,
  db_table_name,
  table,
  require_tbl_colnames = NULL,
  include_tbl_colnames = NULL,
  exclude_db_colnames = NULL
){
  
  # Check if table exists in database
  all_tables <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = "show tables;")
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(!db_table_name %in% all_tables[,1])
    stop(sprintf("There is no '%s' table in the database.", db_table_name))
  
  # Check if table is a data frame object
  if(!is(table, "data.frame") || length(table) == 0)
    stop(sprintf("'%s' must be a data frame object and cannot be empty.", db_table_name))
  
  if(!is.null(exclude_db_colnames)){
    exclude_db_colnames_clause <- paste0("AND ", paste0("`COLUMN_NAME` NOT IN ('", exclude_db_colnames, "'", collapse=" AND ", ")"))
  }else{
    exclude_db_colnames_clause <- ""
  }
  
  # Get column fields
  col_names <- tryCatch({
    DBI::dbGetQuery(
      conn = conn, 
      statement = sprintf(
      "
      SELECT `COLUMN_NAME`
      FROM `INFORMATION_SCHEMA`.`COLUMNS`
      WHERE `TABLE_SCHEMA`='%s'
      AND `TABLE_NAME`='%s' %s
      ", database, db_table_name, exclude_db_colnames_clause
      )
    ) %>% 
      dplyr::select(`COLUMN_NAME`) %>% 
      purrr::flatten_chr()
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(!is.null(include_tbl_colnames))
    col_names <- c(col_names, include_tbl_colnames)

  # Check column fields
  if(any(!col_names %in% colnames(table)))
    stop(sprintf("'%s' table must have the following column names: %s", db_table_name, paste0(col_names, collapse = ", ")))
  
  # Check required fields
  purrr::walk(
    seq_along(require_tbl_colnames),
    function(r){
      #r=1;
      if(any(table[, require_tbl_colnames[r]] %in% c(NA, "")))
        stop(sprintf("'%s' cannot have empty values.", require_tbl_colnames[r]))
      
    }
  )

  ## Clean up the table 
  table <- table %>% 
    dplyr::select(all_of(col_names)) %>% 
    dplyr::mutate_if(is.character, ~trimws(gsub("'", "", ., perl = TRUE))) %>% 
    base::replace(is.null(.), "'NULL'") %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'") %>% 
    dplyr::distinct_all()
  
  # Get number of observations 
  statement <- sprintf("SELECT COUNT(*) AS count FROM %s", db_table_name)
  n_obs <- DBI::dbGetQuery(conn = conn, statement = statement)
  
  # Get table primary key
  primary_key_var <- suppressWarnings(
    DBI::dbGetQuery(
      conn = conn, 
      statement = sprintf(
        "
        SHOW INDEX 
        FROM %s.%s
        WHERE Key_name = 'PRIMARY' %s
        ", database, db_table_name, exclude_db_colnames_clause
      )
    )
  ) %>% dplyr::select("Column_name") %>% purrr::flatten_chr()
  
  ## Check if table has values, if yes, only import the non-existing ones
  if(n_obs$count > 0){
    
    existing_tbl <- seq_len(nrow(table)) %>% 
      purrr::map_dfr(
        function(k){
          #k=1
          primary_key_val <- table %>% 
            dplyr::select(all_of(primary_key_var)) %>% 
            slice(k) %>% 
            as.list()
          
          if(length(primary_key_var) > 1){
            filter_var_by <- rep("AND", length(primary_key_var)-1)
          }else{
            filter_var_by <- NULL
          }
          
          statement <- SigRepoR::lookup_table_sql(
            table_name = db_table_name, 
            return_var = colnames(table), 
            filter_coln_var = primary_key_var, 
            filter_coln_val = primary_key_val,
            filter_var_by = filter_var_by
          )
          
          tbl <- tryCatch({
            DBI::dbGetQuery(conn = conn, statement = statement)
          }, error = function(e){
            stop(e, "\n")
          }, warning = function(w){
            message(w, "\n")
          })
        }
      )
    
    if(nrow(existing_tbl) > 0){
      
      table <- table %>% dplyr::anti_join(existing_tbl)
      
      if(nrow(table) == 0)
        stop(sprintf("The given dataset already existed in the '%s' table of the database.", db_table_name))
    }
    
  }
  
  return(table)
  
}


