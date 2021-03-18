#' @title sqlGeneric
#'
#' @description Submit a query to the database
#'
#' @param query the query to submit. String
#' @param conn connection Handle. uses default newConnHandle call if none
#' defined.
#' @param disconnectAfter Boolean: whether to disconnect the handle after
#' executing the query
#' @export
sqlGeneric <- function(query, conn = newConnHandle(), disconnectAfter = T) {
  ## Connect to the database
  conn <- conn
  ## Disconnect from database when exiting sqlGeneric()
  if (disconnectAfter) {
    on.exit(dbDisconnect(conn), add = TRUE)
  }
  ## Query database and return results
  queryResult <- dbGetQuery(conn, statement=query)
  return(queryResult)
}
