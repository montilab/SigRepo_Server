#' @title addSignatureToQueue
#' @importFrom OmicSignature writeJson
#' @param signatureName name of the signature
#' @param signatureObject an OmicSignature Object.
#' @param verdict whether to add a signature to your system, or to remove it
#' from your queue
#' @param verbose verbosity of the command
#' @export
addSignatureToQueue <- function(signatureName,
                                signatureObject,
                                verdict = NULL,
                                verbose = F) {
  query = paste0(
    "INSERT INTO sigrepo.signature_queue(signature_name) VALUES(",
    singleQuote(signatureName),
    ");"
  )
  if (!is.null(verdict)) {
    query = paste0(
      "INSERT INTO sigrepo.signature_queue(signature_name, verdict) VALUES(",
      singleQuote(signatureName),
      ",",
      verdict,
      ");"
    )
  }
  if (verbose) {
    print(query)
  }
  sqlGeneric(query)
  thisPath = "sigrepo.bu.edu:3838/signatures/queue/"
  writeJson(
    signatureObject,
    paste0(
      "/data_files/signatures/queue/",
      signatureName,
      "_obj.json"
    )
  )
}
