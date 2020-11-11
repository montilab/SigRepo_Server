#' @title updateCollectionEntity
#' @description update a collection in the DB
#' @param connHandle DB connection
#' @param collectionName current collection name
#' @param newCollectionName new collection name
#' @param newCollectionDescription new collection description
#' @return Dataframe with the following fields:
#' > "attribute"-column containing each possible value of said attribute
#' > "count"-the number of signatures belonging to each attribute
#' @export
updateCollectionEntity <- function(connHandle, collectionName = NULL,
                                   newCollectionName = NULL,
                                   newCollectionDescription = NULL) {
  if (is.null(newCollectionName) || length(newCollectionName == 0)) {
    newCollectionName <- collectionName
  }

  collectionIdQuery <- "select signature_collection_id 
			from signature_collections where signature_collection_name = ?scn;"
  collectionId <- dbGetQuery(
    connHandle,
    sqlInterpolate(connHandle,
      collectionIdQuery,
      scn = collectionName
    )
  )$signature_collection_id
  queryUpdateString <- "UPDATE 
			signature_collections 
			SET 
				signature_collection_name=?new_scn, 
				signature_collection_description=?new_scd 
			WHERE 
				signature_collection_id=?scid;"
  queryUpdate <- sqlInterpolate(connHandle, queryUpdateString,
    new_scn = newCollectionName,
    new_scd = newCollectionDescription,
    scid = collectionId
  )
  dbGetQuery(connHandle, queryUpdate)
  print("Updated your collection entity successfully")
  return(TRUE)
}
