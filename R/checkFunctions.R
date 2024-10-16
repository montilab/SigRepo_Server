
#' @title checkPermissions
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' 
#' @noRd
#' 
#' @export
checkPermissions <- function(
    conn,
    action_type = c("SELECT", "INSERT", "UPDATE", "DELETE", "CREATE USER"),
    required_role = c("admin", "user", "guest")
){
  
  # Check action_type
  action_type <- match.arg(action_type)
  
  # Check required_role
  required_role <- match.arg(required_role)
  
  # Check if conn is a MySQLConnection class object
  if(!is(conn, "MySQLConnection"))
    stop("'conn' must be a MySQLConnection object obtained from newConnHandler().\n") 
  
  # Get user connection info
  conn_info <- DBI::dbGetInfo(conn)
  
  # Get a list of actions that user can perform in the database
  user_privileges <- suppressWarnings(
    DBI::dbGetQuery(conn = conn, statement = sprintf("SHOW GRANTS FOR '%s'@'%%';", conn_info$user))
  ) %>% 
    dplyr::slice(1) %>% 
    purrr::flatten_chr() %>% 
    gsub("GRANT(.*)ON(.*)TO(.*)", "\\1", ., perl = TRUE) %>% 
    stringr::str_split(., ",") %>% 
    purrr::flatten_chr() %>% 
    trimws()
  
  # Check if user has the permission to perform the selected action in the database
  if(!action_type %in% user_privileges)
    stop(sprintf("User = '%s' does not have permission to perform this action in the database.\n", conn_info$user)) 
  
  # Look up user in the database
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_id", "user_role"), 
    filter_coln_var = "user_id", 
    filter_coln_val = list("user_id" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # If user does not have a root access and not existed in the database
  if(nrow(user_tbl) == 0)
    stop(sprintf("User = '%s' has not been added to 'users' table in the database.", conn_info$user),
         "\tJust re-run newConnHandler() to automatically add your account to the user group.\n") 
  
  # Get the user roles
  if(user_tbl$user_role == "admin"){
    all_roles <- c("viewer", "editor", "admin")
  }else if(user_tbl$user_role == "editor"){
    all_roles <- c("viewer", "editor")
  }else{
    all_roles <- user_tbl$user_role
  }
  
  # Check if user has the permission to perform the selected action in the database
  if(!required_role %in% all_roles)
    stop(sprintf("User = '%s' does not have permission to perform this action in the database.", conn_info$user)) 
  
  return(conn_info)
  
}

#' @title checkDBTable
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
checkDBTable <- function(
    conn,
    db_table_name,
    check = TRUE
){

  # If check is false, exit
  if(check == FALSE) return(NULL)
  
  # Check if conn is a MySQLConnection class object
  if(!is(conn, "MySQLConnection"))
    stop("'conn' must be a MySQLConnection object obtained from newConnHandler().\n") 
  
  # Check if table exists in database
  all_tables <- tryCatch({
    suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  # Check if table exists in the database
  if(!db_table_name %in% all_tables[,1])
    stop(sprintf("There is no '%s' table in the database.", db_table_name))
  
}

#' @title getDBColNames
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
getDBColNames <- function(
    conn,
    db_table_name,
    check_db_table = TRUE
){
  
  # Whether to check database table
  SigRepo::checkDBTable(
    conn = conn,
    db_table_name = db_table_name,
    check = check_db_table
  )
  
  # Get database column names
  statement <- sprintf("SELECT * FROM %s LIMIT 1", db_table_name)
  
  db_table <- tryCatch({
    suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  db_col_names <- colnames(db_table)
  
  return(db_col_names)
  
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
    db_table_name,
    table,
    exclude_coln_names = NULL,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0)
    stop(sprintf("'%s' table must be a data frame object and cannot be empty.\n", db_table_name))
  
  # Get table column names
  tbl_col_names <- colnames(table)
  
  # Whether to exclude selected column names from the checklist
  if(length(exclude_coln_names) > 0){
    db_col_names <- db_col_names[which(!db_col_names %in% exclude_coln_names)]
  }
  
  # Check column fields
  if(any(!db_col_names %in% tbl_col_names))
    stop(sprintf("'%s' table is missing the following column names: %s.\n", db_table_name, paste0(db_col_names[which(!db_col_names %in% tbl_col_names)], collapse = ", ")))
  
  # Clean up the table by converting all empty values as NULL 
  table <- table %>% 
    dplyr::mutate_if(is.character, ~trimws(gsub("'", "", ., perl = TRUE))) %>% 
    base::replace(is.null(.), "'NULL'") %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'") %>% 
    dplyr::distinct_all()
  
  # Return a unique and cleanup table
  return(table)
  
}


#' @title createHashKey
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
#' @import digest
createHashKey <- function(
    table,
    hash_var = NULL,
    hash_columns = NULL,
    hash_method = c("md5", "sodium")
){
  
  # check hash_method
  hash_method <- match.arg(hash_method)
  
  # Check hash_var
  stopifnot("'hash_var' cannot be empty." = 
              (length(hash_var) == 1 && !hash_var %in% c(NA, "")))
  
  # Check hash_columns
  stopifnot("'hash_columns' cannot be empty." = 
              (length(hash_columns) > 0 && all(!hash_columns %in% c(NA, ""))))   
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0)
    stop(sprintf("'%s' table must be a data frame object and cannot be empty.\n", db_table_name))

  ## Check if the hash variable already existed in the table
  if(hash_var %in% colnames(table)) return("")
  
  # Make sure hash columns exist in table and are not empty
  purrr::walk(
    seq_along(hash_columns),
    function(r){
      #r=1;
      ## Check if the hash column exists in the table
      if(!hash_columns[r] %in% colnames(table))
        stop(sprintf("'%s' is required and does not existed in the table.\n", hash_columns[r]))
      
      ## Make sure hash column is not empty
      if(any(table[, hash_columns[r]] %in% c(NA, "", NULL)))
        stop(sprintf("'%s' is required and cannot have any empty values.\n", hash_columns[r]))
      
    }
  )
  
  # Create the hash variable
  table <- table %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(
      hash_key = ifelse(
        hash_method == "md5", 
        paste0(!!!syms(hash_columns)) %>% tolower(.) %>% digest::digest(., algo = "md5", serialize = FALSE), 
        paste0(!!!syms(hash_columns)) %>% sodium::password_store(.)
      )
    ) %>% 
    dplyr::rename(
      !! paste0(hash_var) := hash_key
    ) %>% 
    dplyr::ungroup()
  
  return(table)

}

#' @title removeDuplicates
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
#' @import digest
removeDuplicates <- function( 
    conn,
    db_table_name,
    table,
    coln_var,
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  stopifnot("'coln_var' cannot be empty." = 
              (length(coln_var) == 1 && !coln_var %in% c(NA, "")))
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0)
    stop(sprintf("'%s' table must be a data frame object and cannot be empty.", db_table_name))
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table))
    stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names) 
    stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  
  # Get number of observations 
  statement <- sprintf("SELECT COUNT(*) AS count FROM %s", db_table_name)
  n_obs <- suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  
  ## Check if table has values. If yes, only return non-overlapping samples
  if(n_obs$count > 0){
    
    return_var <- coln_var
    filter_coln_var <- coln_var
    filter_coln_val <- table %>% dplyr::distinct(!!!syms(coln_var)) %>% as.list()
    
    existing_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      return_var = return_var, 
      filter_coln_var = filter_coln_var, 
      filter_coln_val = filter_coln_val,
      check_db_table = check_db_table
    )
    
    if(nrow(existing_tbl) > 0){
      table <- table %>% dplyr::mutate(id = trimws(tolower(!!!syms(coln_var)))) %>% 
        dplyr::anti_join(
          existing_tbl %>% dplyr::mutate(id = trimws(tolower(!!!syms(coln_var)))) %>% dplyr::select(-all_of(coln_var)), 
          by = "id"
        )
      if(nrow(table) == 0)
        message(sprintf("All records of this dataset already existed in the '%s' table of the database.\n", db_table_name))
    }
    
  }
  
  return(table)
  
}
  
#' @title getVariableID
#' @description Get a variable ID by looking up its primary key or hash key
#' @param conn An established connection to database using newConnhandler() 
#' @param db_table_name An api key uses to access the database
#' @param table An api key uses to access the database
#' @param coln_var An api key uses to access the database
#' @param coln_var_id An api key uses to access the database
#' @param check_db_table An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
#' @import digest
getVariableID <- function(
    conn,
    db_table_name,
    table,
    coln_var,
    coln_var_id,
    check_db_table = TRUE
){
  
  # Get able column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  stopifnot("'coln_var' cannot be empty." = 
              (length(coln_var) == 1 && !coln_var %in% c(NA, "")))  
  
  # Check coln_var_id
  stopifnot("'coln_var_id' cannot be empty." = 
              (length(coln_var_id) == 1 && !coln_var_id %in% c(NA, "")))
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0)
    stop(sprintf("'%s' table must be a data frame object and cannot be empty.", db_table_name))

  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table))
    stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  
  # Make sure column variable is not empty
  if(any(table[, coln_var] %in% c(NA, "", NULL)))
    stop(sprintf("'%s' is required and cannot have any empty values.", coln_var))
  
  # Check if columns existed in the database
  check_db_var <- c(coln_var, coln_var_id)
  
  ## Check if column variable existed in the database
  if(any(!check_db_var %in% db_col_names))
    stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", check_db_var[which(!check_db_var %in% db_col_names)], "'", collapse = ", "), db_table_name))
  
  # Get number of observations 
  statement <- sprintf("SELECT COUNT(*) AS count FROM %s", db_table_name)
  n_obs <-  suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  
  # Look up ID in database
  if(n_obs$count > 0){
    
    return_var <- check_db_var
    filter_coln_var <- coln_var
    filter_coln_val <- table %>% dplyr::distinct(!!!syms(coln_var)) %>% as.list()
    
    tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name, 
      return_var = return_var, 
      filter_coln_var = filter_coln_var, 
      filter_coln_val = filter_coln_val,
      check_db_table = check_db_table
    )
    
    return(tbl)
    
  }else{
    
    return(data.frame(NULL))
    
  }
  
}

#' @title addOrganismErrorMessage
#' @description Error message for trying to add unknown organisms into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
addOrganismErrorMessage <- function(
  db_table_name,
  unknown_values
){
  
  stop(sprintf("The following organisms: %s are not existed in the '%s' table of our database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name),
       "\tYou can use 'getOrganisms()' to see a list of available organisms in our database.\n",
       "\tTo add an organism into our database, please contact our admin for more details.\n")
  
}

#' @title addTranscriptomicsFeatureErrorMessage
#' @description Error message for trying to add unknown features into database
#' @param db_table_name The table name in database
#' @param unknown_features The unknown values
#' 
#' @noRd
#' 
#' @export 
addTranscriptomicsFeatureErrorMessage <- function(
  db_table_name,
  organism_id,
  unknown_features
){
  
  # Look up table
  organism <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "organisms", 
    return_var = "organism", 
    filter_coln_var = "organism_id", 
    filter_coln_val = list("organism_id" = 2),
    check_db_table = TRUE
  )$organism 
  
  stop(sprintf("The following features: %s do not existed in '%s' species as referenced to the '%s' table of our database.\n", paste0("'", unknown_features, "'", collapse = ", "), organism, db_table_name), 
       "\tYou can use 'getFeatures()' to see a list of available features by organisms and assay types.\n",
       "\tTo add a feature name into our database, please contact our admin for more details.\n")
  
}

#' @title addPlatformErrorMessage
#' @description Error message for trying to add unknown platforms into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
addPlatformErrorMessage <- function(
  db_table_name,
  unknown_values
){
  
  stop(sprintf("The following platforms: %s are not existed in the '%s' table of our database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name), 
       "\tYou can use 'getPlatforms()' to see a list of available features in our database.\n",
       "\tTo add a platform into our database, please contact our admin for more details.\n")
  
}

#' @title addSampleTypeErrorMessage
#' @description Error message for trying to add unknown sample types into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
addSampleTypeErrorMessage <- function(
  db_table_name,
  unknown_values
){
  
  stop(sprintf("The following sample types: %s are not existed in the '%s' table of our database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name),
       "\tYou can use 'getSampleTypes()' to see a list of available sample types in our database.\n",
       "\tTo add a sample type into our database, please contact our admin for more details.\n")
  
}

#' @title addPhenotypeErrorMessage
#' @description Error message for trying to add unknown phenotypes into database
#' @param db_table_name The table name in database
#' @param unknown_values The unknown values
#' 
#' @noRd
#' 
#' @export
addPhenotypeErrorMessage <- function(
  db_table_name,
  unknown_values
){
  
  stop(sprintf("The following phenotypes: %s are not existed in the '%s' table of our database.\n", paste0("'", unknown_values, "'", collapse = ", "), db_table_name),
       "\tYou can use 'getPhenotypes()' to see a list of available phenotypes in our database.\n",
       "\tTo add a phenotype into our database, please contact our admin for more details.\n")
  
}



  