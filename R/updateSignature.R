#' @title updateSignature
#' @description Add a signature to the signature table of the database
#' @param conn An established connection to the database using newConnhandler() 
#' @param omic_signature An R6 class object from the OmicSignature package
#' @param signature_name The name of the signature to update
#' @export
updateSignature <- function(
    conn,
    omic_signature,
    signature_name
){
  
  # Check user connection and permission ####
  conn_info <- SigRepoR::checkPermissions(
    conn = conn, 
    action_type = "UPDATE",
    required_role = "editor"
  )
  
  if (!conn_info ) {
    stop("You do not have sufficient permissions to update signatures.")
  }
  
  # Get table name in database ####
  db_table_name <- "signatures"
  
  # Step 1: Delete the existing signature
  delete_result <- tryCatch({
    SigRepoR::deleteSignature(conn = conn, signature_name = signature_name)
  }, error = function(e) {
    stop(paste("Failed to delete signature:", e$message))
  })
  
  if (!delete_result) {
    stop("Error: Unable to delete the existing signature.")
  }
  
  # Step 2: Add the new omic signature
  add_result <- tryCatch({
    SigRepoR::addSignature(conn = conn, omic_signature = omic_signature)
  }, error = function(e) {
    stop(paste("Failed to add the new signature:", e$message))
  })
  
  if (!add_result) {
    stop("Error: Unable to add the new omic signature.")
  }
  
  message("Signature successfully updated.")\

  # close connection

  DBI::dbDisconnect(conn_info$conn)
}
