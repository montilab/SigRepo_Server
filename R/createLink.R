#' @title createLink
#' @param signatureName the name of the signature to link to
#' @param signatureDir directory to the signature files; defaults to settings
#' configured when running SigRepoR::configureSigRepo
#' @param signatureServer host of the signature files; defaults to settings
#' configured when running SigRepoR::configureSigRepo
#' @param applicationPort port of the SigRepo Application; defaults to settings
#' configured when running SigRepoR::configureSigRepo
#' @param signatureFileExtension string denoting the file extension each
#' html anchor should contain. defaults to empty string
#' @export
createLink <-
  function(signatureName,
           signatureDir=Sys.getenv("signatureDirectory"),
           signatureServer=Sys.getenv("signatureServer"),
           applicationPort=Sys.getenv("applicationPort"),
           signatureFileExtension="") {
    portURLExtension = ""
    if(is.null(applicationPort) && applicationPort != ""){
      portURLExtension = paste0(":", applicationPort,"/")
    }
    paste0(
      '<a href="',
      signatureServer,
      applicationPort,
      signatureDir,
      signatureName,
      signatureFileExtension,
      '"',
      'target="_blank">',
      signatureName,
      "</a>"
    )
  }
