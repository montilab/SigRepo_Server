#' @title Create a link to a signature file based on the signature name
#' @param signatureName the name of the signature to link to
#' @param signatureDir link to the SigRepo server
#' @export
createLink <-
  function(signatureName, signatureDir = Sys.getenv("signatureDirectory")) {
    paste0(
      '<a href="',
      signatureDir,
      signatureName,
      "_obj.json",
      '"',
      'target="_blank">',
      signatureName,
      "</a>"
    )
  }
