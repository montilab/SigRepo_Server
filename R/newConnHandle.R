#' @title newConnHandle
#' @description establishes a new connection handle to the host
#' updated 2020/08/12
#' If you want to use the guest account, has very restricted access, select only.
#' newConnHandle("You", thisPassword = NULL)
#' will prompt you to enter password for user "You"
#' @importFrom DBI dbConnect
#' @importFrom getPass getPass
#' @param db character: the schema to which you want the handle to point.
#' @param databaseHost character: the server you to which you want to connect
#' @param databasePort integer: the Port you wish to use. Our server currently uses
#' port 4253
#' @param thisUser character: the user establishing the handle
#' @param thisPassword character: password associated with user. optional and
#' can be set to null, but
#' would need to input the password separately through the getPass method defined
#' in the function.
#' @param usePassword character: whether to use the password specified or ask
#' the user to input the password through getPass.
#' @return a MySQL connection Handle.
#' @export
newConnHandle <- function(db = "sigrepo",
                          driver = RMySQL::MySQL(),
                          databaseHost = Sys.getenv("databaseServer"),
                          databasePort = as.integer(Sys.getenv("databasePort")),
                          thisUser = "guest",
                          thisPassword = "guest",
                          usePassword = "YES") {
  if (is.null(databaseHost) || is.null(databasePort)) {
    stop(paste("Cannot establish a connection, configuration settings are not set",
      "Please first use",
      "`configureSigrepo(signatureDirectory, databaseServer, databasePort)`",
      "as a one-time step to point to the required servers.",
      sep = "\n"
    ))
  }
  if (is.null(thisPassword) || is.null(usePassword) || tolower(usePassword) == "no") {
    thisPassword <- getPass::getPass()
  }
  dbConnect(
    drv = driver,
    dbname = db,
    host = databaseHost,
    port = databasePort,
    username = thisUser,
    password = thisPassword
  )
}
