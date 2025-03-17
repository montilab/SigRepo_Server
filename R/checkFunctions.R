
#' @title print_messages
#' @description Function to whether print diagnostic messages or not
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @noRd
#' 
#' @export
print_messages <- function(verbose){
  
  if(verbose == TRUE){
    base::options(warning.length = 2000L, show.error.messages	= TRUE, verbose = verbose)
  }else if(verbose == FALSE){
    base::options(warning.length = 2000L, show.error.messages	= TRUE, verbose = verbose)
  }
  
}

#' @title verbose
#' @description Function to whether print diagnostic messages or not
#' 
#' @noRd
#' 
#' @export
verbose <- function(...){
  
  # Fetch verbose option
  opt <- base::getOption("verbose", FALSE)

  # If opt is FALSE
  if(!opt) return(base::invisible(NULL))
  
  # Return messages
  msgs <- base::list(...)
  base::message(msgs, "\n")

}

#' @title checkPermissions
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param action_type An established connection to database using SigRepo::newConnhandler() 
#' @param required_role An established connection to database using SigRepo::newConnhandler() 
#' 
#' @noRd
#' 
#' @export
checkPermissions <- function(
    conn,
    action_type = c("SELECT", "INSERT", "UPDATE", "DELETE", "CREATE USER"),
    required_role = c("admin", "editor", "viewer")
){
  
  # Check action_type
  action_type <- base::match.arg(action_type, several.ok = TRUE)
  
  # Check required_role
  required_role <- base::match.arg(required_role)
  
  # Get user connection info
  conn_info <- DBI::dbGetInfo(conn)
  
  # Look up user in the database
  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "users", 
    return_var = c("user_name", "user_role", "api_key"), 
    filter_coln_var = "user_name", 
    filter_coln_val = list("user_name" = conn_info$user), 
    check_db_table = TRUE
  )
  
  # Check if user existed in the users table in the database
  if(nrow(user_tbl) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(base::sprintf("User = '%s' does not exist in the database. Please contact admin to add user to the database.\n", conn_info$user)) 
  }
  
  # Get the grant tables
  grant_tbl <- base::tryCatch({
    base::suppressWarnings(
      DBI::dbGetQuery(conn = conn, statement = base::sprintf("SHOW GRANTS FOR '%s'@'%%';", conn_info$user))
    ) 
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  })    
  
  # Create a placeholder to store all privileges
  user_privileges <- NULL
  
  # Loop through each grant and extract list of actions that user can perform in the database
  for(s in 1:nrow(grant_tbl)){ 
    #s=1;
    privs <- grant_tbl %>% 
      dplyr::slice(s) %>% 
      purrr::flatten_chr() %>% 
      base::gsub("GRANT(.*)ON(.*)TO(.*)", "\\1", ., perl = TRUE) %>% 
      stringr::str_split(., ",") %>% 
      purrr::flatten_chr() %>% 
      base::trimws()
    user_privileges <- c(user_privileges, privs)
  }
  
  # Check if user has the permission to perform the selected actions in the database
  if(any(!action_type %in% user_privileges)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("User = '%s' does not have permission to perform this action in the database.\n", conn_info$user)) 
  }
  
  # Get the user roles
  if(user_tbl$user_role == "admin"){
    all_roles <- c("viewer", "editor", "admin")
  }else if(user_tbl$user_role == "editor"){
    all_roles <- c("viewer", "editor")
  }else if(user_tbl$user_role == "viewer"){
    all_roles <- c("viewer")
  }
  
  # Check if user has the specific role to perform the selected action in the database
  if(!required_role %in% all_roles){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("User = '%s' does not have permission to perform this action in the database.", conn_info$user)) 
  }
  
  # Return user connection and user role
  return(
    c(conn_info, user_role = user_tbl$user_role, api_key = user_tbl$api_key)
  )
  
}

#' @title checkDBTable
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name Name of table in the database
#' @param check Check whether table exists in the database. Default = TRUE.
#' 
#' @noRd
#' 
#' @export
checkDBTable <- function(
    conn,
    db_table_name,
    check = TRUE
){
  
  # If check is false, exit function
  if(check == FALSE) return(NULL)
  
  # Check if table exists in database
  all_tables <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "show tables;"))
  
  # Check if table exists in the database
  if(!db_table_name %in% all_tables$Tables_in_sigrepo){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("There is no '%s' table in SigRepo database.", db_table_name))
  }
  
}

#' @title checkTableInput
#' @description Check table against a particular table in the database.
#' @param conn An established connection to database using newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param table A data frame object
#' @param exclude_coln_names A list of column names to be excluded from the check.
#' @param check_db_table Check whether table exists in the database. Default = TRUE.
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
  if(!is(table, "data.frame") || length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("'table' must be a data frame object and cannot be empty.\n"))
  }
  
  # Whether to exclude selected column names from the checklist
  if(length(exclude_coln_names) > 0){
    db_col_names <- db_col_names[which(!db_col_names %in% exclude_coln_names)]
  }
  
  # Check column fields
  if(any(!db_col_names %in% colnames(table))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("the table is missing the following column names: %s.\n", paste0(db_col_names[which(!db_col_names %in% colnames(table))], collapse = ", ")))
  }
  
  # Clean up the table by converting all empty values as NULL 
  table <- table %>% 
    dplyr::mutate_if(is.character, ~base::trimws(base::gsub("'", "", ., perl = TRUE))) %>% 
    base::replace(is.null(.), "'NULL'") %>% 
    base::replace(is.na(.), "'NULL'") %>% 
    base::replace(. == "", "'NULL'") %>% 
    dplyr::distinct_all()
  
  # Return a unique and cleanup table
  return(table)
  
}

#' @title checkDuplicatedEmails
#' @description Check if provided user table has duplicated emails.
#' @param conn An established connection to database using newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param table A data frame object
#' @param coln_var A list of column names to be excluded from the check.
#' @param check_db_table Check whether table exists in the database. Default = TRUE.
#' 
#' @noRd
#' 
#' @export
checkDuplicatedEmails <- function( 
    conn,
    db_table_name = "users",
    table,
    coln_var = "user_email",
    check_db_table = TRUE
){
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  if(!length(coln_var) == 1 || any(coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  }
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  }
  
  # Get number of observations 
  n_obs <- SigRepo:::getNumOfObs(conn = conn, db_table_name = db_table_name)
  
  ## Check if table has values, only return non-overlapping samples
  if(n_obs$count > 0){
    
    return_var <- "*"
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
      purrr::walk(
        1:nrow(table),
        function(s){
          #s=1;
          check_email <- existing_tbl %>% dplyr::filter(user_email %in% table$user_email[s])
          if(nrow(check_email) > 0 && !base::trimws(base::tolower(table$user_name[s])) %in% base::trimws(base::tolower(check_email$user_name))){
            # Disconnect from database ####
            base::suppressWarnings(DBI::dbDisconnect(conn))  
            # Return error message
            base::stop(
              base::sprintf("\tThe following email address:\n"),
              base::sprintf("\t%s\n", base::paste0(unique(existing_tbl[,return_var]), collapse = ",\n")),
              base::sprintf("\talready existed in the '%s' table of the database.\n", db_table_name),
              base::sprintf("\tEmail address must be unique for each user. Please provide a different email for user = '%s'.\n", table$user_name[s])
            )
          }
        }
      )
    }
    
  }
}

#' @title checkOmicSignature
#' @description Check omic_signature is a valid R6 object
#' @param omic_signature An R6 class object from OmicSignature package
#' 
#' @noRd
#' 
#' @export
checkOmicSignature <- function(
    omic_signature,
    check = TRUE
){
  
  # Whether to validate the object
  if(check == FALSE){
    
    # Check difexp is provided ####
    if("difexp" %in% names(omic_signature)){
      difexp <- omic_signature$difexp
      if(is.null(difexp)){
        difexp <- NULL
      }else{
        # Check if difexp is a data frame 
        if(!is(difexp, "data.frame") || length(difexp) == 0) 
          base::stop("'difexp' in OmicSignature must be a data frame object and cannot be empty.")
      }
    }else{
      difexp <- NULL
    }
    
    # Create has_difexp variable to store whether omic_signature has difexp included ####
    has_difexp <- ifelse(!is.null(difexp), 1, 0) 
    
    # Return difexp status
    return(has_difexp)
    
  }
  
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
    base::stop("'metadata' in OmicSignature must have the following names:", paste0(metadata_fields, collapse = ", "))
  
  # Extract signature table from omic_signature ####
  signature <- omic_signature$signature  
  
  # Check if signature is a data frame ####
  if(!is(signature, "data.frame") || length(signature) == 0)
    base::stop("'signature' in OmicSignature must be a data frame and cannot be empty.")
  
  # Check required signature fields ####
  signature_fields <- c('feature_name', 'probe_id', 'score', 'direction')
  
  if(any(!signature_fields %in% colnames(signature)))
    base::stop("'signature' in OmicSignature must have the following column names:", paste0(signature_fields, collapse = ", "))
  
  # Make sure required column fields do not have any empty values ####
  if(any(is.na(signature[,signature_fields]) == TRUE))
    base::stop(sprintf("All required column names in 'signature' of OmicSignature: %s cannot contain any empty values.", paste0(signature_fields, collapse = ", ")))
  
  # Check the direction symbols in signature table
  if(any(!signature$direction %in% c("+", "-")))
    base::stop("The 'direction' variable in 'signature' table of OmicSignature must contain +/- symbols only.")
  
  # Check difexp is provided ####
  if("difexp" %in% names(omic_signature)){
    difexp <- omic_signature$difexp
    if(is.null(difexp)){
      difexp <- NULL
    }else{
      # Check if difexp is a data frame 
      if(!is(difexp, "data.frame") || nrow(difexp) == 0) 
        base::stop("'difexp' in OmicSignature must be a data frame object and cannot be empty.")
    }
  }else{
    difexp <- NULL
  }
  
  # If difexp is provided, check required difexp fields ####
  difexp_req_fields <- c('feature_name', 'probe_id', 'score'); difexp_opt_fields <- c('p_value', 'q_value','adj_p')
  
  if(!is.null(difexp) && any(!difexp_req_fields %in% colnames(difexp)) && all(!difexp_opt_fields %in% colnames(difexp)))
    base::stop("'difexp' in OmicSignature must have the following required column names: ", paste0("'", difexp_req_fields, "'", collapse = ", "), " and one of the following fields: ", paste0("'", difexp_opt_fields, "'", collapse = " or "))
  
  # Make sure required column fields do not have any empty values ####
  if(!is.null(difexp) && any(is.na(difexp[,difexp_req_fields]) == TRUE))
    base::stop(sprintf("% are required column names in 'difexp' of OmicSignature, and they cannot contain any empty values.\n", paste0(difexp_req_fields, collapse = ", ")))
  
  # Check signature name (required) ####
  if(length(metadata$signature_name[1]) == 0 || metadata$signature_name[1] %in% c(NA, ""))
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
  if(length(metadata$organism[1]) == 0 || metadata$organism[1] %in% c(NA, ""))
    base::stop("'organism' in OmicSignature's metadata is required and cannot be empty.")
  
  # Check phenotype (required) #####
  if(length(metadata$phenotype[1]) == 0 || metadata$phenotype[1] %in% c(NA, ""))
    base::stop("'phenotype' in OmicSignature's metadata is required and cannot be empty.")
  
  # Create has_difexp variable to store whether omic_signature has difexp included ####
  has_difexp <- ifelse(!is.null(difexp), 1, 0) 
  
  # Check probe_id in signature are in difexp table if difexp is provided
  if(has_difexp == 1 && any(!signature$probe_id %in% difexp$probe_id)){
    base::stop("Some probe_id in the signature are not included in the probe_id in the difexp.")
  }
  
  # Return difexp status
  return(has_difexp)
  
}


#' @title checkOmicCollection
#' @description Check omic_signature is a valid R6 object
#' @param omic_collection An OmicSignatureCollection object from OmicSignature package
#' 
#' @noRd
#' 
#' @export
checkOmicCollection <- function(
    omic_collection
){
  
  # Check if metadata exists in the collection
  if(!"metadata" %in% names(omic_collection))
    base::stop("'OmicSignatureCollection' must contain a metadata object.\n")
  
  # Check if OmicSigList exists in the collection
  if(!"OmicSigList" %in% names(omic_collection))
    base::stop("'OmicSignatureCollection' must contain a OmicSigList object.\n")
  
  # Extract metadata from omic_collection ####
  metadata <- omic_collection$metadata
  
  # Check if metadata is a list ####
  if(!is(metadata, "list"))
    base::stop("'metadata' in OmicSignatureCollection must be a list.")
  
  # Check required metadata fields ####
  metadata_fields <- c('collection_name', 'description')
  
  if(any(!metadata_fields %in% names(metadata)))
    base::stop("'metadata' in OmicSignatureCollection must have the following names:", paste0(metadata_fields, collapse = ", "))
  
  # Extract OmicSigList from omic_collection ####
  omic_sig_list <- omic_collection$OmicSigList  
  
  # Check if OmicSigList is a list ####
  if(!is(omic_sig_list, "list"))
    base::stop("'OmicSigList' in OmicSignatureCollection must be a list containning a list of signature objects.")
  
  # Check required signature fields ####
  purrr::walk(
    base::seq_along(omic_sig_list),
    function(c){
      #c=1;
      SigRepo::checkOmicSignature(
        omic_signature = omic_sig_list[[c]],
        check = TRUE
      )
    }
  )

}

#' @title getNumOfObs
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name A table in the database
#' 
#' @noRd
#' 
#' @export
getNumOfObs <- function(
    conn,
    db_table_name
){
  
  # Create sql statement
  statement <- base::sprintf("SELECT COUNT(*) AS count FROM %s", db_table_name)
  
  # Get number of observations 
  n_obs <- base::tryCatch({
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  }, warning = function(w){
    base::message(w, "\n")
  })
  
  # Return number of observations 
  return(n_obs)
  
}

#' @title getDBColNames
#' @description Get column names of a particular table in the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param check_db_table Check whether table exists in the database. Default = TRUE
#' 
#' @noRd
#' 
#' @export
getDBColNames <- function(
    conn,
    db_table_name,
    check_db_table = TRUE
){
  
  # Check whether table exists in the database
  SigRepo::checkDBTable(
    conn = conn,
    db_table_name = db_table_name,
    check = check_db_table
  )
  
  # Get sql statement
  statement <- base::sprintf("SELECT * FROM %s LIMIT 1", db_table_name)
  
  # Run sql statement
  db_table <- base::tryCatch({
    base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = statement))
  }, error = function(e){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))    
    # Return error message
    base::stop(e, "\n")
  }, warning = function(w){
    base::message(w, "\n")
  })
  
  # Return column names ####
  return(colnames(db_table))
  
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
  
  # Get table column names
  db_col_names <- SigRepo::getDBColNames(
    conn = conn,
    db_table_name = db_table_name,
    check_db_table = check_db_table
  )
  
  # Check coln_var
  if(!length(coln_var) == 1 || any(coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check coln_var_id
  if(!length(coln_var_id) == 1 || any(coln_var_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var_id' must have a length of 1 and cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  }
  
  # Make sure column variable is not empty
  if(any(table[, coln_var] %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("'%s' is a required field and cannot contain any empty values.", coln_var))
  }
  
  # Check if columns existed in the database
  check_db_var <- c(coln_var, coln_var_id)
  
  ## Check if column variable existed in the database
  if(any(!check_db_var %in% db_col_names)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", check_db_var[which(!check_db_var %in% db_col_names)], "'", collapse = ", "), db_table_name))
  }
  
  # Look up ID in database
  return_var <- check_db_var
  filter_coln_var <- coln_var
  filter_coln_val <- table %>% dplyr::distinct(!!!syms(coln_var)) %>% as.list()
  
  tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = db_table_name, 
    return_var = return_var, 
    filter_coln_var = filter_coln_var, 
    filter_coln_val = filter_coln_val,
    check_db_table = FALSE
  )
  
  # Return table
  return(tbl)
  
}



#' @title createHashKey
#' @description Check api key whether it is valid to access the database
#' @param conn An established connection to database using newConnhandler() 
#' @param hash_var An api key uses to access the database
#' @param hash_columns An api key uses to access the database
#' @param hash_method An api key uses to access the database
#' 
#' @noRd
#' 
#' @export
#' @import digest
createHashKey <- function(
    table,
    hash_var,
    hash_columns,
    hash_method = c("md5", "sodium")
){
  
  # check hash_method
  hash_method <- base::match.arg(hash_method)
  
  # Check hash_var
  if(!length(hash_var) == 1 || any(hash_var %in% c(NA, ""))){
    base::stop("'hash_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check hash_columns
  if(length(hash_columns) == 0 || any(hash_columns %in% c(NA, ""))){  
    base::stop("'hash_columns' cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0){
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  # Make sure hash columns exist in table and are not empty
  purrr::walk(
    seq_along(hash_columns),
    function(r){
      #r=1;
      ## Check if the hash column exists in the table
      if(!hash_columns[r] %in% colnames(table)){
        base::stop(sprintf("'%s' is required and does not existed in the table.\n", hash_columns[r]))
      }      
      ## Make sure hash column is not empty
      if(any(table[, hash_columns[r]] %in% c(NA, ""))){
        base::stop(sprintf("'%s' is required and cannot contain any empty values.\n", hash_columns[r]))
      }      
    }
  )
  
  # Get column names without the creating harh_var
  tbl_colnames <- colnames(table)[which(!colnames(table) %in% hash_var)]
  
  # Create the hash variable
  table <- table %>% 
    dplyr::select(all_of(tbl_colnames)) %>% 
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
  
  # Return table
  return(table)
  
}

#' @title removeDuplicates
#' @description Remove duplicates from table in the database
#' @param conn An established connection to database using SigRepo::newConnhandler() 
#' @param db_table_name Name of a table in the database
#' @param table A data frame object
#' @param coln_var A column variable in the data table
#' @param check_db_table whether to check database table. Default = TRUE
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
  if(!length(coln_var) == 1 || any(coln_var %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'coln_var' must have a length of 1 and cannot be empty.")
  }
  
  # Check if table is a data frame object and not empty
  if(!is(table, "data.frame") || length(table) == 0){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop("'table' must be a data frame object and cannot be empty.")
  }
  
  ## Check if column variable existed in the table
  if(!coln_var %in% colnames(table)){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("Column '%s' does not existed in the table.", coln_var))
  }
  
  ## Check if column variable existed in the database
  if(!coln_var %in% db_col_names){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(sprintf("Columns: %s does not exist in the '%s' table of the database.", paste0("'", coln_var, "'", collapse = ", "), db_table_name))
  }
  
  # Get number of observations
  n_obs <- SigRepo::getNumOfObs(conn = conn, db_table_name = db_table_name)
  
  ## Check if table has values, only return non-overlapping samples
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
      if(nrow(table) == 0){
        base::warnings(base::sprintf("All records of this dataset already existed in the '%s' table of the database.\n", db_table_name))
      }
    }
    
  }
  
  # Return table
  return(table)
  
}





