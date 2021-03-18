#' @title writeSignatureFile
#' @param fileObject signature object
#' @param userName name of the user uploading
#' @export
writeSignatureFile <- function(fileObject, userName) {
  copyFile(fileObject, userName)
}
