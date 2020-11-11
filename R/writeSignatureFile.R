#' @title writeSignatureFile
#' @param uploadPath path to upload / write the signature file
#' @param fileObject signature object
#' @param fileName name of the file
#' @export
writeSignatureFile <- function(uploadPath, fileObject, fileName) {
  sigDir <- paste(uploadPath, fileName, sep = "")
  copyFile(fileObject, sigDir)
}
