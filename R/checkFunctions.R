
#' @title checkPermissions
#' @description Check api key whether it is valid to access the database
#' @param conn_handler An established connection to database using SigRepo::newConnhandler() 
#' 
#' @noRd
#' 
#' @export
checkPermissions <- function(
    conn_handler,
    action_type = c("SELECT", "INSERT", "UPDATE", "DELETE", "CREATE USER"),
    required_role = c("admin", "editor", "viewer")
){
  
  # Initiate db connection ####
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check action_type
  action_type <- base::match.arg(action_type)
  
  # Check required_role
  required_role <- base::match.arg(required_role)
  
  # Get user connection info
  conn_info <- DBI::dbGetInfo(conn)
  
  # Look up user in the database
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_name", "user_role"), 
    filter_coln_var = "user_name", 
    filter_coln_val = list("user_name" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # Check if user is root
  if(conn_info$user == "root"){
    
    # If user has a root access but does not exist in users table of the database
    # Then add root to users table
    if(nrow(user_tbl) == 0)
      base::stop(sprintf("User = '%s' has not been added to 'users' table in the database.", conn_info$user),
                 "\tJust re-run newConnHandler() to add 'root' to the table.\n") 
    
  }else{
    
    # Check if user existed in the users table in the database
    if(nrow(user_tbl) == 0)
      base::stop(sprintf("User = '%s' does not exist in the database. Please contact admin to add user to the database.\n", conn_info$user)) 
    
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
      base::stop(sprintf("User = '%s' does not have permission to perform this action in the database.\n", conn_info$user)) 
    
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
      base::stop(sprintf("User = '%s' does not have permission to perform this action in the database.", conn_info$user)) 
    
  }
  
  # Return user connection and user role
  return(
    c(conn = conn, conn_handler = list(conn_handler), conn_info, user_role = user_tbl$user_role)
  )
  
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
    base::stop("'conn' must be a MySQLConnection object obtained from newConnHandler().\n") 
  
  # Check if table exists in database
  all_tables <- tryCatch({
    suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
  }, error = function(e){
    base::stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  # Check if table exists in the database
  if(!db_table_name %in% all_tables[,1])
    base::stop(sprintf("There is no '%s' table in the database.", db_table_name))
  
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
    base::stop(sprintf("The table must be a data frame object and cannot be empty.\n"))
  
  # Get table column names
  tbl_col_names <- colnames(table)
  
  # Whether to exclude selected column names from the checklist
  if(length(exclude_coln_names) > 0){
    db_col_names <- db_col_names[which(!db_col_names %in% exclude_coln_names)]
  }
  
  # Check column fields
  if(any(!db_col_names %in% tbl_col_names))
    base::stop(sprintf("the table is missing the following column names: %s.\n", paste0(db_col_names[which(!db_col_names %in% tbl_col_names)], collapse = ", ")))
  
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

#' @title checkDuplicatedEmail
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param api_key An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
#' @import digest
checkDuplicatedEmail <- function( 
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
  stopifnot("'coln_var' cannot be empty." = (length(coln_var) == 1 && !coln_var %in% c(NA, "")))
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0)
    base::stop(sprintf("'%s' table must be a data frame object and cannot be empty.", db_table_name))
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table))
    base::stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names) 
    base::stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  
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
      base::stop(sprintf("A user will the following email address: %s already existed in the '%s' table of the database.\n", paste0(unique(existing_tbl[,return_var]), collapse = ","), db_table_name))
    }
    
  }
  
  return(table)
  
}

#' @title checkOmicSignature
#' @description Check omic_signature is a valid R6 object
#' @param omic_signature An R6 class object from OmicSignature package
#' 
#' @noRd
#' 
#' @export
checkOmicSignature <- function(
    omic_signature
){
  
  # Check if omic_signature is an OmicSignature class object ####
  if(!is(omic_signature, "OmicSignature"))
    base::stop("'omic_signature' must be an R6 class object from OmicSignature package.") 
  
  # Check metadata and signature
  if(!"metadata" %in% names(omic_signature))
    base::stop("'omic_signature' must contain a metadata object.\n")
  
  if(!"signature" %in% names(omic_signature))
    base::stop("'omic_signature' must contain a signature object.\n")
  
  # Extract metadata from omic_signature ####
  metadata <- omic_signature$metadata
  
  # Check if metadata is a list ####
  if(!is(metadata, "list"))
    base::stop("'metadata' in OmicSignature must be a list.")
  
  # Check required metadata fields ####
  metadata_fields <- c('signature_name', 'organism', 'direction_type', 'assay_type', 'phenotype')
  
  if(any(!metadata_fields %in% names(metadata)))
    base::stop("'metadata' in OmicSignature must have the following column names:", paste0(metadata_fields, collapse = ", "))
  
  # Extract signature table from omic_signature ####
  signature <- omic_signature$signature  
  
  # Check if signature is a data frame ####
  if(!is(signature, "data.frame"))
    base::stop("'signature' in OmicSignature must be a data frame.")
  
  # Check required signature fields ####
  signature_fields <- c('feature_name', 'probe_id', 'score', 'direction')
  
  if(any(!signature_fields %in% colnames(signature)))
    base::stop("'signature' in OmicSignature must have the following column names:", paste0(signature_fields, collapse = ", "))
  
  # Make sure required column fields do not have any empty values ####
  if(any(is.na(signature[,signature_fields]) == TRUE))
    base::stop(sprintf("All required column names in 'signature' of OmicSignature: %s cannot contain any empty values.\n", paste0(signature_fields, collapse = ", ")))
  
  # Check difexp is provided ####
  if("difexp" %in% names(omic_signature)){
    difexp <- omic_signature$difexp
    if(is.null(difexp)){
      difexp <- NULL
    }else{
      # Check if difexp is a data frame ####
      if(!is(difexp, "data.frame")) 
        base::stop("'difexp' in OmicSignature must be a data frame object.")
    }
  }else{
    difexp <- NULL
  }
  
  # If difexp is provided, check required difexp fields ####
  difexp_req_fields <- c('feature_name', 'probe_id', 'score'); difexp_opt_fields <- c('p_value', 'q_value','adj_p')
  
  if(!is.null(difexp) && any(!difexp_req_fields %in% colnames(difexp)) && all(!difexp_opt_fields %in% colnames(difexp)))
    base::stop("'difexp' in OmicSignature must have the following column names: ", paste0("'", difexp_req_fields, "'", collapse = ", "), " and one of the following fields: ", paste0("'", difexp_opt_fields, "'", collapse = " or "))
  
  # Make sure required column fields do not have any empty values ####
  if(!is.null(difexp) && any(is.na(difexp[,difexp_req_fields]) == TRUE))
    base::stop(sprintf("All required column names in 'difexp' of OmicSignature: %s cannot contain any empty values.\n", paste0(difexp_req_fields, collapse = ", ")))
  
  # Check signature name (required) ####
  if(metadata$signature_name[1] %in% c(NA, "", NULL))
    base::stop("'signature_name' in OmicSignature's metadata cannot be empty.")
  
  # Check direction_type (required) ####
  direction_type_options <- c("uni-directional", "bi-directional", "categorical")
  
  if(!metadata$direction_type[1] %in% direction_type_options)
    base::stop("'direction_type' in OmicSignature's metadata object must be: \n", paste0(direction_type_options, collapse = "/"))
  
  # Check assay_type (required) ####
  assay_type_options <- c("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites")
  
  if(!metadata$assay_type[1] %in% assay_type_options)
    base::stop("'assay_type' in OmicSignature's metadata object must be: ", paste0(assay_type_options, collapse = "/"))
  
  # Check organism (required) #####
  if(metadata$organism[1] %in% c(NA, "", NULL))
    base::stop("'organism' in OmicSignature's metadata is required and cannot be empty.")
  
  # Check phenotype (required) #####
  if(metadata$phenotype[1] %in% c(NA, "", NULL))
    base::stop("'phenotype' in OmicSignature's metadata is required and cannot be empty.")
  
  # Create has_difexp variable to store whether omic_signature has difexp included ####
  has_difexp <- ifelse(!is.null(difexp), 1, 0) 
  
  # Return difexp status
  return(has_difexp)
  
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
    base::stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
  db_col_names <- colnames(db_table)
  
  return(db_col_names)
  
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
    base::stop(sprintf("'%s' table must be a data frame object and cannot be empty.", db_table_name))
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table))
    base::stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  
  # Make sure column variable is not empty
  if(any(table[, coln_var] %in% c(NA, "", NULL)))
    base::stop(sprintf("'%s' is required and cannot have any empty values.", coln_var))
  
  # Check if columns existed in the database
  check_db_var <- c(coln_var, coln_var_id)
  
  ## Check if column variable existed in the database
  if(any(!check_db_var %in% db_col_names))
    base::stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", check_db_var[which(!check_db_var %in% db_col_names)], "'", collapse = ", "), db_table_name))
  
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
  base::stopifnot("'hash_var' cannot be empty." = 
                    (length(hash_var) == 1 && !hash_var %in% c(NA, "")))
  
  # Check hash_columns
  base::stopifnot("'hash_columns' cannot be empty." = 
                    (length(hash_columns) > 0 && all(!hash_columns %in% c(NA, ""))))   
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0)
    base::stop(sprintf("'%s' table must be a data frame object and cannot be empty.\n", db_table_name))
  
  ## Check if the hash variable already existed in the table
  if(hash_var %in% colnames(table)) return("")
  
  # Make sure hash columns exist in table and are not empty
  purrr::walk(
    seq_along(hash_columns),
    function(r){
      #r=1;
      ## Check if the hash column exists in the table
      if(!hash_columns[r] %in% colnames(table))
        base::stop(sprintf("'%s' is required and does not existed in the table.\n", hash_columns[r]))
      
      ## Make sure hash column is not empty
      if(any(table[, hash_columns[r]] %in% c(NA, "", NULL)))
        base::stop(sprintf("'%s' is required and cannot have any empty values.\n", hash_columns[r]))
      
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
    base::stop(sprintf("'%s' table must be a data frame object and cannot be empty.", db_table_name))
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table))
    base::stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names) 
    base::stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  
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





