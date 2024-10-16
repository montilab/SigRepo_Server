#' @title updateSignature
#' @description Add a signature to signature table of the database
#' @param conn An established connection to database using newConnhandler() 
#' @param omic_signature An R6 class object from OmicSignature package
#' @param signature_name name of your signature that you previously uploaded
#' @export
updateSignature <- function(
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

  #1. delete signature using deleteSignature(conn=conn, signature_name=signature_name)
  
  #2. after all removed, re-upload the provided omic signature using addSignature(conn=conn, omic_signature=omic_signature)
  
}  
  
  
  
  
  

