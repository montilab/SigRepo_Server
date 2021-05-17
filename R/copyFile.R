#' @title copyFile
#' @param objectFile input file object
#' @param signatureServer server hosting the signature files. Defaults to
#' configuration settings
#' @param uploadPath the directory to which the signature files get written.
#' Defaults to configuration settings.
#' @param thisUser the username that's uploading the file. Note that this user
#' needs to be able to write to your target directory.
#' @export
copyFile <- function(objectFile,
                     signatureServer = Sys.getenv("signatureServer"),
                     uploadPath = Sys.getenv("signatureDirectory"),
                     thisUser) {
  # check if input is empty or not. if empty, do nothing.
  if (objectFile != "") {
    system(paste(
      "scp",
      objectFile,
      paste0(thisUser, "@", signatureServer, ":", uploadPath, objectFile)
    ))
  }
}
