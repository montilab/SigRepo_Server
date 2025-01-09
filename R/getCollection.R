#' @title getCollection
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