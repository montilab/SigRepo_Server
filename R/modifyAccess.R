#' @title modifyAccess
#' @description Modify user access to either a signature or collection
#' @param conn An established connection to database using newConnhandler() 
#' @param objectType The type of object being accessed (signature or collection)
#' @param objectName The name of the object being accessed
#' @param user_id The user_id of the user being granted access, or a list of user_ids
#' @param access_role The role of the user being granted access
#' @export


modifyAccess <- function(conn, objectType, objectName, user_id, objectName, user_id, access_role){
  
 # checking the type of the object-  signature or collection

# if signature, then query signature table to see if signature exists

# if it exists then, query the users who already have access to the signature and their roles

 # if users are already the access_role, then dont change, if other users are not the access_role, then change their access_role to the new access_role

# if the user is not in the list of users who have access to the signature, then add the user to the list of users who have access to the signature

# if collection, then query collection table to see if collection exists

# if it exists then, query the users who already have access to the collection and their roles

 # if users are already the access_role, then dont change, if other users are not the access_role, then change their access_role to the new access_role

# if the user is not in the list of users who have access to the collection, then add the user to the list of users who have access to the collection



# close connection 

DBI::dbDisconnect(conn_info$conn)