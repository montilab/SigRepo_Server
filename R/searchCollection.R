#' @title searchCollection
#' @description Get a list of collection available in the database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param collection_name Name of collection to be looked up by.
#' @param user_name Name of users that the collection belongs to.
#' 
#' @examples 
#' 
#' # Create a db connection
#' conn_handler <- SigRepo::newConnHandler(
#'  dbname = "sigrepo", 
#'  host = "montilab.bu.edu", 
#'  port = 3306, 
#'  user = "guest", 
#'  password = "guest"
#' )
#' 
#' # Get a list of collection available in the database
#' collection_tbl <- sigRepo::searchCollection(
#'   conn_handler = conn_handler
#' )
#' 
#' @export
searchCollection <- function(
    conn_handler,
    collection_name = NULL,
    signature_name = NULL,
    user_name = NULL
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permissions ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = "viewer"
  )
  
  # If user_role is not admin, check user access to the signature ####
  if(length(username) > 0 && all(!user_name %in% c("", NA))){
    
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
      base::suppressMessages(DBI::dbDisconnect(conn))     
      
      # Show message
      base::stop(sprintf("There are no collection returned from the search parameters.\n"))
      
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
    
    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "collection", 
      return_var = "*", 
      check_db_table = TRUE
    ) 
    
  }
  
  # Check if signature exists
  if(nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))     
    
    # Show message
    base::stop(sprintf("There are no collection returned from the search parameters.\n"))
    
  }else{
    
    # Look up signatures in the signature collection table
    signature_collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signature_collection_access", 
      return_var = "*", 
      filter_coln_var = "collection_id", 
      filter_coln_val = list("collection_id" = unique(collection_tbl$collection_id)),
      check_db_table = TRUE
    ) 
    
    # Add signatures to collection table
    collection_tbl <- collection_tbl %>% 
      dplyr::left_join(signature_collection_tbl, by = "collection_id")  
    
    # Look up name of the signatures in the signature table
    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = c("signature_id", "signature_name"),
      filter_coln_var = "signature_id", 
      filter_coln_val = list("signature_id" = unique(collection_tbl$signature_id)),
      check_db_table = TRUE
    ) 
    
    # Add name of the signatures to collection table
    collection_tbl <- collection_tbl %>% 
      dplyr::left_join(signature_tbl, by = "signature_id")
    
    # Get a list of filtered variables
    filter_var_list <- list(
      "collection_name" = collection_name,
      "signature_name" = signature_name
    )
    
    # Filter table with given search variables
    for(r in base::seq_along(filter_var_list)){
      #r=1;
      filter_status <- ifelse(length(filter_var_list[[r]]) == 0 || all(filter_var_list[[r]] %in% c("", NA)), FALSE, TRUE)
      if(filter_status == TRUE){
        filter_var <- names(filter_var_list)[r]
        filter_val <- filter_var_list[[r]][which(!filter_var_list[[r]] %in% c(NA, ""))]
        collection_tbl <- collection_tbl %>% dplyr::filter(trimws(tolower(!!!syms(filter_var))) %in% trimws(tolower(filter_val)))
      }
    }
    
    # Check if collection is empty, throw an error message
    if(nrow(collection_tbl) == 0){
      
      # Disconnect from database ####
      base::suppressMessages(DBI::dbDisconnect(conn))     
      
      # Show message
      base::stop(sprintf("There are no collection returned from the search parameters.\n"))
      
    }
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))
    
    # Return table
    return(collection_tbl)
    
  }
}







