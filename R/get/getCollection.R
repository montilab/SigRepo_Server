#' @title getCollection
#' @description Retrieve a Signature Collection from the database
#' @param conn An established connection to database using newConnhandler() 
#' @param collection_id The name of the collection to retrieve
#' @export

getCollection <- function(conn, collection_id) {


# check if user is allowed to access the database
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "SELECT",
    required_role = 'admin'
  )

  # if user is not admin then go to signature access table and check if user can view the collection

  db_table_name_1 <- 'collection_access'

  collection_access <- DBI::dbGetQuery(conn =conn, 
                                        statement= sprintf("SELECT collection_name FROM %s WHERE usuer_id = '%s'", db_table_name_1, conn_info$user_id) # need to ask reina how to get user name
  
  # Database name for collection table
  db_table_name_2- "collection_relationships"
  
  # grabbing collections in database
  db_collections <- DBI::dbGetQuery(conn = conn, 
                                     statement = sprintf("SELECT collection_name FROM %s", 
                                                         db_table_name))
  
 