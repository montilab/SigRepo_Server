#' @title updateCollectionMetadata
#' @description Update metadata of a collection in the database
#' @param conn_handler A handler uses to establish connection to  
#' a remote database obtained from SigRepo::newConnhandler()
#' @param collection_id ID of collection in the database to be updated.
#' @param collection_name Name of the collection to be changed.
#' @param description Description of the collection to be changed.
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
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
#' @export
updateCollectionMetadata <- function(
    conn_handler,
    collection_id,
    collection_name = NULL,
    description = NULL,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = c("SELECT", "INSERT", "DELETE"),
    required_role = "editor"
  )

  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Check collection_id
  if(!length(collection_id) == 1 || all(collection_id %in% c(NA, ""))){
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    # Show message
    base::stop("'collection_id' must have a length of 1 and cannot be empty.")
  }
  
  # Check if collection exists ####
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "collection",
    return_var = "*",
    filter_coln_var = "collection_id",
    filter_coln_val = list("collection_id" = collection_id),
    check_db_table = TRUE
  )
  
  # If collection exists, return the collection table else throw an error message
  if(nrow(collection_tbl) == 0){
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Show message
    base::stop(base::sprintf("There is no collection_id = '%s' in the 'collection' table of the SigRepo Database.", collection_id))
    
  }else{
    
    # If user is not admin, check if it has access to collection
    if(user_role != "admin"){
      
      # Check if user is the one who uploaded the collection
      collection_user_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "collection",
        return_var = "*",
        filter_coln_var = c("collection_id", "user_name"), 
        filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name),
        filter_var_by = "AND",
        check_db_table = FALSE
      )
      
      # If not, check if user was added as an owner or editor
      if(nrow(collection_user_tbl) == 0){
        
        # Get access collection table
        collection_access_tbl <- SigRepo::lookup_table_sql(
          conn = conn,
          db_table_name = "collection_access",
          return_var = "*",
          filter_coln_var = c("collection_id", "user_name", "access_type"),
          filter_coln_val = list("collection_id" = collection_id, "user_name" = user_name, "access_type" = c("owner", "editor")),
          filter_var_by = c("AND", "AND"),
          check_db_table = TRUE
        )
        
        # If user does not have permission, throw an error message
        if(nrow(collection_access_tbl) == 0){
          
          # Disconnect from database ####
          base::suppressWarnings(DBI::dbDisconnect(conn)) 
          
          # Show message
          base::stop(base::sprintf("User = '%s' does not have permission to update collection_id = '%s' in the database.", user_name, collection_id))
          
        }
      }
    }
    
    # Check if both collection_name and description are empty ####
    if((length(collection_name[1]) == 0 || all(collection_name[1] %in% c("", NA))) && (length(description[1]) == 0 || all(description[1] %in% c("", NA)))){
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn)) 
      # Show message
      base::stop("'collection_name' and 'description' cannot both be empty. Please provide a value to either variable.")
    }

    # 1. Delete collection id from collection metadata table ####
    SigRepo::delete_table_sql(
      conn = conn,
      db_table_name = "collection",
      delete_coln_var = "collection_id",
      delete_coln_val = collection_tbl$collection_id[1],
      check_db_table = FALSE
    )
    
    # Add updated variables
    metadata_tbl <- collection_tbl %>% 
      dplyr::mutate(
        collection_id = collection_tbl$collection_id,
        user_name = collection_tbl$user_name,
        collection_name = ifelse(length(!!collection_name[1]) == 0 || all(!!collection_name[1] %in% c("", NA)), collection_name, !!collection_name[1]),
        description = ifelse(length(!!description[1]) == 0 || all(!!description[1] %in% c("", NA)), description, !!description[1])
      )
    
    # Create a new hash key for the collection ####
    metadata_tbl <- SigRepo::createHashKey(
      table = metadata_tbl,
      hash_var = "collection_hashkey",
      hash_columns = c("collection_name", "user_name"),
      hash_method = "md5"
    )
    
    # Check table against database table ####
    metadata_tbl <- SigRepo::checkTableInput(
      conn = conn,
      db_table_name = "collection",
      table = metadata_tbl, 
      exclude_coln_names = "date_created",
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn,
      db_table_name = "collection", 
      table = metadata_tbl,
      check_db_table = FALSE
    ) 
    
    # Disconnect from database ####
    base::suppressWarnings(DBI::dbDisconnect(conn)) 
    
    # Return message
    SigRepo::verbose(base::sprintf("collection_id = '%s' has been updated.", metadata_tbl$collection_id[1]))

  }
}




