#' @title getCollection
#' @description Get a list of collection uploaded by a specified user in the database.
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param collection_name Name of collection to be returned
#' @param signature_id ID of collection to be returned
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
getCollection <- function(
    conn_handler,
    collection_name = NULL,
    collection_id = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT", 
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # If user_role is not admin, check user access to the signature ####
  if(user_role != "admin"){
    
    # Check user access ####
    collection_access_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "collection_access", 
      return_var = "*", 
      filter_coln_var = c("user_name", "access_type"),
      filter_coln_val = list("user_name" = user_name, "access_type" = c("owner", "editor", "viewer")),
      filter_var_by = "AND",
      check_db_table = TRUE
    ) 
    
    # If user does not have owner or editor permission, throw an error message
    if(nrow(collection_access_tbl) == 0){
      
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn)) 
      
      # Show message
      base::stop(base::sprintf("\nThere are no collection that belong to user_name = '%s' in the SigRepo database.\n", user_name))
      
    }
    
    # Look up collection
    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "collection", 
      return_var = "*", 
      filter_coln_var = "collection_id", 
      filter_coln_val = list("collection_id" = unique(collection_access_tbl$collection_id)),
      check_db_table = TRUE
    ) 
    
  }else{
    
    # Look up collection
    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "collection", 
      return_var = "*", 
      check_db_table = TRUE
    ) 
    
  }

  # Get a list of filtered variables
  filter_var_list <- list(
    "collection_id" = base::unique(collection_id),
    "collection_name" = base::unique(collection_name)
  )
  
  # Filter table with given search variables
  for(r in base::seq_along(filter_var_list)){
    #r=1;
    filter_status <- ifelse(length(filter_var_list[[r]]) == 0 || all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
    if(filter_status == TRUE){
      filter_var <- base::names(filter_var_list)[r]
      filter_val <- filter_var_list[[r]][which(!filter_var_list[[r]] %in% c(NA, ""))]
      collection_tbl <- collection_tbl %>% dplyr::filter(base::trimws(base::tolower(!!!syms(filter_var))) %in% base::trimws(base::tolower(filter_val)))
    }
  }
  
  # Check if signature exists
  if(nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("\nThere are no collection returned from the search parameters.\n"))
    
  }else{
    
    # Create a place holder to store collection
    omic_collection_list <- base::list()
    
    # Create an omic signature object for each signature id ####
    for(r in 1:nrow(collection_tbl)){
      #r=1;
      db_collection_tbl <- collection_tbl %>% dplyr::slice(r)
      
      # Create an OmicSignature object
      omic_signature_collection <- SigRepo::createOmicCollection(
        conn_handler = conn_handler,
        db_collection_tbl = db_collection_tbl
      )
      
      # Append OmicSignature object to overall list
      omic_collection_list <- c(
        omic_collection_list, 
        omic_signature_collection
      )
    }
    
    # Add names to signatures
    base::names(omic_collection_list) <- collection_tbl$collection_name

    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn))
    
    # Return table
    return(omic_collection_list)
    
  }  
}







