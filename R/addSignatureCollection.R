#' @title addSignatureCollection
#' @description Add signature collection to database
#' @param conn An established connection to database using newConnhandler() 
#' @param omic_signature_collection A collection of OmicSignature objects from OmicSignature package
#' @param user_name An user id
#' @export
addSignatureCollection <- function(
    conn,
    omic_signature_collection,
    user_name
){
  
  # Table name in database
  table <- "signature_collection"
  
  # Check if table exists in database
  all_tables <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = "show tables;")
  }, error = function(e){
    stop(e)
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(!table %in% all_tables[,1])
    stop(sprintf("There is no '%s' table in the database.", table))
  
  # Check if omic_signature_collection is an OmicSignature class object
  if(!is(omic_signature_collection, "R6"))
    stop("'omic_signature_collection' must be an R6 class object from OmicSignature package.")  
  
  # Check metadata and signature
  if(!"metadata" %in% names(omic_signature_collection))
    stop("'omic_signature_collection' must contain a metadata object.\n")
  
  if(!"OmicSigList" %in% names(omic_signature_collection))
    stop("'omic_signature_collection' must contain an OmicSigList object.\n")
  
  metadata <- omic_signature_collection$metadata # required
  OmicSigList <- omic_signature_collection$OmicSigList # required
  
  if(is.null(metadata) || nrow(metadata) == 0)
    stop("'metadata' in OmicSignatureCollection cannot be empty.")
  
  if(is.null(OmicSigList))
    stop("'OmicSigList' in OmicSignatureCollection cannot be empty.")
  
  # Check required metadata fields
  metadata_fields <- c('collection_name', 'description')
  
  if(any(!metadata_fields %in% names(metadata)))
    stop("'metadata' in OmicSignatureCollection must have the following column names:", paste0(metadata_fields, collapse = ", "))
  

  
  
}

# close connection

DBI::dbDisconnect(conn_info$conn)

