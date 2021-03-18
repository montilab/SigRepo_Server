#' @title writeSignatureFile
#' @param fileObject signature object
#' @param fileName name of the file
#' @export
writeSignatureFile <- function(fileObject, userName) {
  copyFile(fileObject, userName)
}
