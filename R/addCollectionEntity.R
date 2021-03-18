#' @title addCollectionEntity
#'
#' @description adds signature Collection entities to signature_collections 
#' table in the Database.
#' @importFrom DBI sqlInterpolate dbGetQuery
#' @param connHandle connection handle to use for query
#' @param collectionName name of the collection you're adding
#' @param collectionDescription description of the collection you're adding
#' @export
addCollectionEntity <- function(connHandle,
                                collectionName=NULL,
                                collectionDescription=NULL) {
  # query construction. Think of it like a string substitution,
	# but accounting for multiple entries.
  queryInsertString <- "INSERT 
	INTO 
	signature_collections(
		signature_collection_name, 
		signature_collection_description
	)
	VALUES
	(?collection_name, ?collection_description);"
  queryInsertCollection <- sqlInterpolate(
  	connHandle, 
  	queryInsertString,
    collection_name=collectionName,
    collection_description=collectionDescription
  )
  dbGetQuery(connHandle, queryInsertCollection)
  return(TRUE)
}
