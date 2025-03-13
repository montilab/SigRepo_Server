#' @title createCollectionMetadata
#' @description Create a metadata object for a signature
#' @param conn_handler An established connection to database using SigRepo::conn_init() 
#' @param omic_collection An R6 class object from OmicSignature package
#' 
#' @noRd
#' 
#' @export
createCollectionMetadata <- function(
    conn_handler,
    omic_collection
){
  
  # Establish user connection ###
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Check if omic_signature is a valid R6 object ####
  # If yes, return whether it has difexp included ####
  base::tryCatch({
    SigRepo::checkOmicCollection(
      omic_collection = omic_collection
    )
  }, error = function(e){
    # Disconnect from database
    base::suppressWarnings(DBI::dbDisconnect(conn))  
    # Return error message
    base::stop(e, "\n")
  }, warning = function(w){
    base::message(w, "\n")
  })  
  
  # Extract metadata from omic_collection ####
  metadata <- omic_collection$metadata
  
  # Create signature metadata table ####
  metadata_tbl <- base::data.frame(
    collection_name = metadata$collection_name,
    description = metadata$description
  )
  
  # Extract omic_sig_list from omic_collection ####
  omic_sig_list <- omic_collection$OmicSigList
  
  # Check required signature fields ####
  purrr::walk(
    base::seq_along(omic_sig_list),
    function(c){
      #c=1;
      SigRepo::checkOmicSignature(
        omic_signature = omic_sig_list[[c]],
        check = TRUE
      )
    }
  )
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn)) 
  
  # Return the metadata tbl ####
  return(metadata_tbl)
  
}






