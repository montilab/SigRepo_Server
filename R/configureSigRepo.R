#' @title configureSigRepo
#' @description configures your instance of Sigrepo to have all its
#' functions point to the server of your choice by setting System level
#' variable assignments
#' @param signatureDirectory where your signature files will be
#' @param databaseServer where your database is
#' @param databasePort which port to use to connect to your database
#' @export
configureSigRepo <- function(signatureDirectory,
                             databaseServer,
                             databasePort) {
  # use pingr package to first check if connections
  # to the new settings can be established in the first place
  # otherwise don't change settings
  Sys.setenv(signatureDirectory = signatureDirectory)
  Sys.setenv(databaseServer = databaseServer)
  Sys.setenv(databasePort = databasePort)
  paste(
    "SigRepo has been Reconfigured to the following", "",
    paste0("Database Server: ", databaseServer),
    paste0("Database Port: ", databasePort),
    paste0("Signature Directory: ", signatureDirectory),
    sep = "\n"
  )
}
