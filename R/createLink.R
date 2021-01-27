#' @title Create a link to a signature file based on the signature name
#' @param signatureName the name of the signature to link to
#' @param repoLink link to the SigRepo server
#' @export
createLink <-
  function(signatureName, repoLink = SigRepoLink) {
    paste0(
      '<a href="',
      Sys.getenv("signatureDirectory"),
      signatureName,
      "_obj.json",
      '"',
      'target="_blank">',
      signatureName,
      "</a>"
    )
  }
