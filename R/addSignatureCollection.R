#' @title addSignatureCollection
#' @description adding an OmicSignatureCollection objecct to your file system
#' and databasee
#' @importFrom rlist list.append
#' @param OmicSignatureCollectionObj the OmicSignatureCollection Object to upload
#' @param connHandle The database connection handle
#' @param uploadPath where to upload the file
#' @param thisUser username of the submitter, for confirmation
#' @export
addSignatureCollection <-
  function(OmicSignatureCollectionObj,
           connHandle,
           uploadPath,
           thisUser) {
    OmicSigList <- OmicSignatureCollectionObj$OmicSigList
    lapply(OmicSigList,
           addSignature,
           thisHandle = connHandle,
           uploadPath,
           thisUser)
    signatureNames <- list()
    for (x in OmicSigList) {
      list.append(signatureNames, x$metadata$signature_name)
    }
    addCollectionSignatures(
      connHandle,
      OmicSignatureCollectionObj$metadata$collection_name,
      c(signatureNames)
    )
  }
