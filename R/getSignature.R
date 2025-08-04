#' @title getSignature
#' @description Get a list of signatures uploaded by a specified user in the database.
#' @param conn_handler A handler used to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param signature_name Name of signatures to be returned
#' @param signature_id ID of signatures to be returned
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
getSignature <- function(
    conn_handler,
    signature_name = NULL,
    signature_id = NULL,
    verbose = TRUE
){
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user permissions
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT", 
    required_role = "viewer"
  )
  
  user_role <- conn_info$user_role[1]
  user_name <- conn_info$user[1]
  
  # Lookup signatures
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signatures", 
    return_var = "*", 
    check_db_table = TRUE
  )
  
  # Filter by provided parameters
  filter_var_list <- list(
    "signature_id" = base::unique(signature_id),
    "signature_name" = base::unique(signature_name)
  )
  
  for (r in base::seq_along(filter_var_list)) {
    filter_status <- ifelse(length(filter_var_list[[r]]) == 0 || all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if (filter_status) {
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][which(!filter_var_list[[r]] %in% c(NA, ""))]
      signature_tbl <- signature_tbl %>% 
        dplyr::filter(base::trimws(base::tolower(!!rlang::sym(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }
  
  # Permission filtering for non-admins
  if (user_role != "admin") {
    signature_visibility <- signature_tbl %>% dplyr::filter(visibility == FALSE) %>% dplyr::distinct(signature_id, visibility)
    
    for (w in 1:nrow(signature_visibility)) {
      signature_access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access", 
        return_var = "*", 
        filter_coln_var = c("signature_id", "user_name", "access_type"),
        filter_coln_val = list(
          "signature_id" = signature_visibility$signature_id[w], 
          "user_name" = user_name, 
          "access_type" = c("owner", "editor", "viewer")
        ),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      )
      if (nrow(signature_access_tbl) == 0) {
        signature_tbl <- signature_tbl %>% dplyr::filter(!signature_id %in% signature_visibility$signature_id[w])
      }
    }
  }
  
  # Return early if no signatures
  if (nrow(signature_tbl) == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    SigRepo::verbose("There are no signatures returned from the search parameters.\n")
    return(NULL)
  }
  
  # Select relevant columns (optional)
  coln_names <- colnames(signature_tbl)
  signature_tbl <- signature_tbl %>% dplyr::select(all_of(coln_names))
  
  # Retrieve OmicSignature objects via API
  omic_signature_list <- base::list()
  
  for (r in 1:nrow(signature_tbl)) {
    db_signature_tbl <- signature_tbl %>% dplyr::slice(r)
    
    # API call setup
    signature_hashkey <- db_signature_tbl$signature_hashkey[1]
    api_key <- conn_info$api_key[1]
    
    if (is.null(signature_hashkey) || signature_hashkey == "") {
      warning(sprintf("Missing hashkey for signature ID %s", db_signature_tbl$signature_id[1]))
      next
    }
    
    api_url <- base::sprintf(
      "http://%s:%s/get_signature?api_key=%s&signature_hashkey=%s",
      conn_handler$host[1],
      conn_handler$api_port[1],
      api_key,
      signature_hashkey
    )
    
    res <- httr::GET(api_url)
    
    if(res$status_code != 200){
      stop("API request failed")
    }
    
    raw_data <- httr::content(res, as = "raw")
    # save to a temp file
    
    tmp_file <- tempfile(fileext = ".RDS")
    writeBin(raw_data, tmp_file)
   

    
    omic_signature <- base::readRDS(tmp_file)
    
    
    # Add to list
    omic_signature_list[[db_signature_tbl$signature_name[1]]] <- omic_signature
  }
  
  # Clean up DB connection
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  return(omic_signature_list)
}
