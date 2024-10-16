#' @title addSignatureHandler
#' @description Add signature to database
#' @param conn An established connection to database using newConnhandler() 
#' @param omic_signature An R6 class object from OmicSignature package
#' @export
addSignatureHandler <- function(
    conn,
    omic_signature
){
  
  # 1. Uploading signature into database
  message("\nUploading signature to database...\n")
  
  signature_tbl <- SigRepoR::addSignature(
    conn = conn, 
    omic_signature = omic_signature
  )
  
  # 2. Importing signature feature set into database after signature
  # was imported successfully in step (1)
  message("Adding signature feature set to database...\n")
  
  feature_set <- omic_signature$signature %>% 
    dplyr::transmute(
      feature_name = symbol,
      orig_feature_id = id,
      score = score,
      direction = direction
    )
  
  # Add signature feature set to database #####
  SigRepoR::addSignatureFeatureSet(
    conn = conn,
    signature_id = signature_tbl$signature_id,
    organism_id = signature_tbl$organism_id,
    assay_type = signature_tbl$assay_type,
    feature_set = feature_set
  )
  
  # 3. Adding user to signature access table after signature
  # was imported successfully in step (1)
  message("Adding user to signature access in database...\n")
  
  SigRepoR::addUserToSignature(
    conn = conn,
    signature_id = signature_tbl$signature_id,
    user_id = signature_tbl$user_id,
    access_type = "owner"
  )
  
  message("Finished uploading...\n")
  
}  
  
  
  
  
  

