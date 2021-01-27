#' @title retrieve OmicSig obj from VM file system
#' @param signatureName name of the signature
#' @param signatureDir directory which the signature file is in
#' @importFrom OmicSignature readJson 
#' @return OmicSignature object
#' @export
retrieveOmicSigObj <-
  function(signatureName, signatureDir = SigRepoSignatureDir) {
    return(readJson(
      paste0(
        Sys.getenv(signatureDirectory),
        signatureName,
        "_obj.json"
      )
    ))
  }
