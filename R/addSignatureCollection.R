#' @title addSignatureCollection
#' @description Add signature collection to database
#' @param conn An established connection to database using newConnhandler() 
#' @param omic_signature_collection A collection of OmicSignature objects from OmicSignature package
#' @export
addSignatureCollection <- function(
    conn,
    omic_signature_collection
){
#   
# 
#   
#   
#   # checking user permission ####
#   # guest for right now for testing purposes.
#   
#   conn_info <- SigRep::checkPermissions(
#     conn_handler = conn_handler,
#     action_type = 'INSERT',
#     required_role = 'guest'
#   )
#   
#   # getting user role
#   
#   user_role <- conn_info$user_role[1]
#   
#   user_name <- conn_info$user[1]
#   
#   db_table_name <- 'collection'
#   
#   # grabbing the omic signature object name here 
#   
#   collection_name <- deparse(substitute(omic_signature_collection))
#   
#   
#   # define table name
#   collection_table <- 'collection'
#   # check if siganture collection is already in the database
#   
#   collection_tbl <- SigRepo::lookup_table_sql(
#     conn = conn_info$conn,
#     db_table_name = collection_table,
#     return_var = "*",
#     filter_coln_var = 'collection_name',
#     filter_coln_val = list("collection_name" = collection_name),
#     check_db_table = TRUE
#   )
#   
#   # if signature collection already exists, then throw error
#   
#   
#  if(nrow(collection_tbl) > 0) {
#    base::stop(sprintf(" The signature collection already exists in the database, use getSignatureCollection to retrieve the collection instead."))
#    
#    # disconnect from the database
#    base::suppressMessages(DBI::dbDisconnect(conn_info$conn))
#  }else{
#    
#   # parse the omic signature objects into separate signature objects
#    
#    # we need to create this f
#  }
# 
#    
# # check user role 
# 
#   if(!user_role %in% "guest"){
#     base::stop(sprintf("User '%s' does not have the correct acces to add a SignatureCollection into the database."))
#     
#     # disconnect from database
#     base::shppressMessages(DBI::dbDisconnect(conn_info$conn))
#   }
#     
#   
# # if the user has the required permissions and the signature collection is not already in the database, then the user will be able to upload the signatute collection.
#   
# # 
}



