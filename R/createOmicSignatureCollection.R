#' @title createOmicSignatureCollection
#' @description Get the collection set uploaded by a specific user in the database.
#' @param conn_handler A handler uses to establish connection to the database
#' obtained from SigRepo::newConnhandler() (required)
#' @param db_collection_tbl A collection table in the database with associated 
#' a list of signature ids to be made into an OmicSignatureCollection object (required)
#' 
#' @noRd
#' 
#' @export
#' 
#' @import OmicSignature
createOmicSignatureCollection <- function(
    conn_handler,
    db_collection_tbl
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "editor"
  )
  
  # Check if table is a data frame object and not empty
  if(!is(db_collection_tbl, "data.frame") || length(db_collection_tbl) == 0){
    # Disconnect from database ####
    DBI::dbDisconnect(conn)    
    # Show message
    base::stop(sprintf("'db_collection_tbl' must be a data frame and cannot be empty.\n"))
  }
  
  # Create metadata
  metadata <- db_collection_tbl %>% base::as.list()
  
  # Look up signatures in the signature collection table
  signature_collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signature_collection_access", 
    return_var = "*", 
    filter_coln_var = "collection_id", 
    filter_coln_val = list("collection_id" = unique(db_collection_tbl$collection_id)),
    check_db_table = TRUE
  ) 
  
  # Add signatures to collection table
  signature_collection_tbl <- db_collection_tbl %>% 
    dplyr::left_join(signature_collection_tbl, by = "collection_id")  
  
  # Create a place holder to store signatures
  omic_signature_list <- list()
  
  # Create an omic signature object for each signature id ####
  for(r in 1:nrow(signature_collection_tbl)){
    #r=1;
    db_signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "signatures", 
      return_var = "*", 
      filter_coln_var = "signature_id", 
      filter_coln_val = list("signature_id" = signature_collection_tbl$signature_id[r]),
      check_db_table = TRUE
    ) 
    
    # Create an OmicSignature object
    omic_signature <- SigRepo::createOmicSignature(
      conn_handler = conn_handler,
      db_signature_tbl = db_signature_tbl
    )
    
    # Append OmicSignature object to overall list
    omic_signature_list <- c(
      omic_signature_list, 
      omic_signature
    )
  }
  
  # Create an OmicSignatureCollection object
  OmicCol <- OmicSignature::OmicSignatureCollection$new(
    metadata = metadata,
    OmicSigList = omic_signature_list
  )
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn))
  
  # Return collection
  return(OmicCol)
  
}







