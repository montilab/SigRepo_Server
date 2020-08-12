#' Submit a query to the database
#'
#' @param query the query to submit
sqlGeneric <- function(query, conn=newConnHandle(), disconnectAfter=T) {
	## Connect to the database
	conn <- conn
	## Disconnect from database when exiting sqlGeneric()
	if(disconnectAfter){
		on.exit(dbDisconnect(conn), add=TRUE)
	}
	## Query database and return results
	queryResult <- dbGetQuery(conn, statement=query)
	return(queryResult)
}