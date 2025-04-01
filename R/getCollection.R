#' @title getCollection
<<<<<<< HEAD
#' @description Retrieve a Signature Collection from the database
#' @param conn An established connection to database using newConnhandler() 
#' @param collection_id The name of the collection to retrieve
#' @export

getCollection <- function(conn, collection_id) {

# ROUGH DESIGN OF FUNCTIN NOT READY 

# check if user is allowed to access the database
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = 'admin'
  )


  # if the user is admin, allo access
  if(conn_info$user_role == 'admin'){
    message('User is an admin of the collection, Proceeding with Colection retrieval')
    return(TRUE)
  }

  # if user is not admin then go to signature access table and check if user can view the collection

  db_table_name_1 <- 'collection_access'\

  query <- sprintf(
    "SELECT collection_name, access_role 
     FROM %s 
     WHERE user_id = '%s'",
    db_table_name, conn_info$user_id
  )

collection_access <- DBI::dbGetQuery(conn =conn, statement = query)

 # check if the user is allowed to access the specified collection

 if(nrow(collection_access) == 0 || !collection_id %in% collection_access$collection_name))+
    stop('User does not have access to the specified collection') 
}


  # Database name for collection table
  db_table_name_2- "collection_relationships"
  
  # grabbing collections in database
  db_collections <- DBI::dbGetQuery(conn = conn, 
                                     statement = sprintf("SELECT collection_name FROM %s", 
                                                         db_table_name))
  
 

 # close connection 

 DBI::dbDisconnect(conn_info$conn)
=======
#' @description Get a list of collection uploaded by a specified user in the database.
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required)
#' @param collection_name Name of collection to be returned
#' @param collection_id ID of collection to be returned
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
    required_role = "viewer"
  )
  
  # Get user_role ####
  user_role <- conn_info$user_role[1] 
  
  # Get user_name ####
  user_name <- conn_info$user[1]
  
  # Look up collection
  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "collection", 
    return_var = "*", 
    check_db_table = TRUE
  ) 
  
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
  
  # If user_role is not admin, check if user has the permission to access the collection ####
  if(user_role != "admin"){
    
    # Get a list of collection with visibility = FALSE
    collection_visibility <- collection_tbl %>% dplyr::filter(visibility == FALSE) %>% dplyr::distinct(collection_id, visibility) 
    
    # Check if user has the permission to view the signatures ####
    for(w in 1:nrow(collection_visibility)){
      #w=1;
      # Check user access ####
      collection_access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "collection_access", 
        return_var = "*", 
        filter_coln_var = c("collection_id", "user_name", "access_type"),
        filter_coln_val = list("collection_id" = collection_visibility$collection_id[w], "user_name" = user_name, "access_type" = c("owner", "editor", "viewer")),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      ) 
      
      # If user does not have owner or editor permission, throw an error message
      if(nrow(collection_access_tbl) == 0){
        collection_tbl <- collection_tbl %>% dplyr::filter(!collection_id %in% collection_visibility$collection_id[w])
      }
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







>>>>>>> reina_dev
