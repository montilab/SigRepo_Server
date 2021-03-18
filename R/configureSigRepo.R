#' @title configureSigRepo
#' @description configures your instance of Sigrepo to have all its
#' functions point to the server of your choice by setting System level
#' variable assignments
#' @param signatureDirectory where your signature files will be on the signature server's file system
#' @param databaseServer where your database is
#' @param databasePort which port to use to connect to your database
#' @param applicationPort optional; string denoting the port that your application
#' is being hosted on; defaults to NULL
#' @param signatureServer optional; string denoting the server that is hosting
#' the signature files; defaults to the databaseServer url
#' @export
configureSigRepo <- function(signatureDirectory,
                             databaseServer,
                             databasePort,
                             applicationPort=NULL,
                             signatureServer=NULL) {
  # TODO use pingr package to first check if connections
  # to the new settings can be established in the first place
  # otherwise don't change settings
  Sys.setenv(signatureDirectory=signatureDirectory)
  Sys.setenv(databaseServer=databaseServer)
  Sys.setenv(databasePort=databasePort)
  Sys.setenv(signatureServer=signatureServer)
  Sys.getenv(applicationPort=applicationPort)
  if (!is.null(signatureHost) && !signatureHost == "") {
    Sys.getenv(signatureServer=databaseServer)
  }
  paste(
    "SigRepo has been Reconfigured to the following",
    "",
    paste0("Application Port: ", applicationPort),
    paste0("Database Server: ", databaseServer),
    paste0("Database Port: ", databasePort),
    paste0("Database Port: ", databasePort),
    paste0("Signature Server: ", signatureServer),
    paste0("Signature Directory: ", signatureDirectory),
    sep = "\n"
  )
}
