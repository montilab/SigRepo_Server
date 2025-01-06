#' @title getCollection
#' @description Retrieve a Signature Collection from the database
#' @param conn An established connection to database using newConnhandler() 
#' @param collection_id The name of the collection to retrieve
#' @export

getCollection <- function(conn, omic_signature) { 
  
  #check user connection and permission

  conn_info <- SigRepoR::checkPermissions(
    conn = conn,
    action_type = 'SELECT',
    required_role = c('admin','editor')
  )

# table name for collection
db_table_name <- 'collection_relationships' 

# if the user is not admin or editor of that signature, then go to collection access table to see if that user has viewing access



}