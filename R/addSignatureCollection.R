#' @title addSignatureCollection
#' @description Add signature collection to database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param omic_collection A collection of OmicSignature objects from OmicSignature package
#' @export
addSignatureCollection <- function(
    conn_handler,
    omic_collection
){
  
  # Allow messages to be printed up to 2000 length long
  base::options(warning.length = 2000L)
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]    
  
  # Get table name in database ####
  db_table_name <- "collection"
  
  # Create collection metadata table ####
  metadata_tbl <- SigRepo::createCollectionMetadata(
    conn_handler = conn_handler, 
    omic_collection = omic_collection
  ) 
  
  # Add additional variables in collection metadata table ####
  metadata_tbl <- metadata_tbl %>% dplyr::mutate(user_name = user_name)
  
  # Create a hash key to look up whether collection is already existed in the database ####
  metadata_tbl <- SigRepo::createHashKey(
    table = metadata_tbl,
    hash_var = "collection_hashkey",
    hash_columns = c("collection_name", "user_name"),
    hash_method = "md5"
  )
  
  # Check if collection exists using its hash key ####
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    return_var = "*", 
    filter_coln_var = "collection_hashkey",
    filter_coln_val = list("collection_hashkey" = metadata_tbl$collection_hashkey),
    check_db_table = TRUE
  ) 
  
  # If the signature exists, throw an error message ####
  if(nrow(collection_tbl) > 0){
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))
    
    # Show message
    base::message(sprintf("\tYou already uploaded a collection with collection_name = '%s' into the SigRepo Database.\n", metadata_tbl$collection_name),
                  sprintf("\tUse searchCollection() to see more details about the collection.\n"),
                  sprintf("\tTo re-upload, try to use a different name.\n"),
                  sprintf("\tID of the uploaded collection: %s\n", collection_tbl$collection_id))
    
    # Return collection id
    return(collection_tbl$collection_id)
    
  }else{
    
    # 1. Uploading each signature in the collection into the database
    base::message("Uploading each signature in the collection into the database...\n")
    
    # Extract omic_sig_list from omic_collection ####
    omic_sig_list <- omic_collection$OmicSigList
    
    # Add signature into the database ####
    signature_id_list <- c()
    
    for(c in base::seq_along(omic_sig_list)){
      #c=1;
      signature_id <- base::tryCatch({
        SigRepo::addSignature(
          omic_signature = omic_sig_list[[c]],
          conn_handler = conn_handler
        )
      }, error = function(e){
        # Disconnect from database ####
        base::suppressWarnings(DBI::dbDisconnect(conn))  
        # Return error message
        base::stop(e, "\n")
      }, warning = function(w){
        base::message(w, "\n")
      }) 
      
      signature_id_list <- c(signature_id_list, signature_id)
      
    }
    
    # 2. Uploading collection metadata into database
    base::message("Uploading collection metadata into the database...\n")
    
    # Check table against database table ####
    metadata_tbl <- SigRepo::checkTableInput(
      conn = conn, 
      db_table_name = db_table_name,
      table = metadata_tbl, 
      exclude_coln_names = c("collection_id", "date_created"),
      check_db_table = FALSE
    )
    
    # Insert table into database ####
    SigRepo::insert_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      table = metadata_tbl,
      check_db_table = FALSE
    ) 
    
    # Look up collection id for the next step ####
    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = db_table_name, 
      return_var = "*", 
      filter_coln_var = "collection_hashkey",
      filter_coln_val = list("collection_hashkey" = metadata_tbl$collection_hashkey),
      check_db_table = FALSE
    ) 
    
    # 3. Adding user to collection access table after collection
    # was imported successfully in step (1)
    base::message("Adding user to collection access table in the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addUserToCollection(
        conn_handler = conn_handler,
        collection_id = collection_tbl$collection_id,
        user_name = user_name,
        access_type = "owner"
      )
    }, error = function(e){
      # Delete signature
      base::suppressMessages(SigRepo::deleteCollection(conn_handler = conn_handler, collection_id = collection_tbl$collection_id))
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }, warning = function(w){
      base::message(w, "\n")
    }) 
    
    # 4. Adding signature to collection access table after collection
    # was imported successfully in step (1)
    base::message("Adding signature to collection access table in the database...\n")
    
    # If there is a error during the process, remove the signature and output the message
    base::tryCatch({
      SigRepo::addSignatureToCollection(
        conn_handler = conn_handler,
        collection_id = collection_tbl$collection_id,
        signature_id = signature_id_list
      )
    }, error = function(e){
      # Delete signature
      base::suppressMessages(SigRepo::deleteCollection(conn_handler = conn_handler, collection_id = collection_tbl$collection_id))
      # Disconnect from database ####
      base::suppressWarnings(DBI::dbDisconnect(conn))  
      # Return error message
      base::stop(e, "\n")
    }, warning = function(w){
      base::message(w, "\n")
    }) 
    
    # Return message
    base::message("Finished uploading.\n")
    
    # Disconnect from database ####
    base::suppressMessages(DBI::dbDisconnect(conn))   
    
  }
}



