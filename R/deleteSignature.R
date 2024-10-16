#' @title deleteSignature
#' @description Add a signature to signature table of the database
#' @param conn An established connection to database using newConnhandler() 
#' @param signature_name name of your signature that you previously uploaded
#' @export
deleteSignature <- function(
    conn,
    omic_signature
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "UPDATE",
    required_role = "editor"
  )
  
  # Get table name in database ####
  db_table_name <- "signatures"

  #1. check if signature-name exists in the database that belongs to the user.
  
  #2. if yes, remove signature_name from the "signatures" table
  
  #3. if yes, remove signature feature set that belongs to the user's uploaded signature name
  
  #4. if yes, and difexp is provided, remove the files from the system storage
  
  #5. if yes, update the access signature table.
  
}  
  
  
  
  
  

