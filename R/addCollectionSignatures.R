#' @title addCollectionSignatures
#'
#' @description adds signature-collection pairs to signatures_to_collections
#' table in the database.
#' @importFrom DBI sqlInterpolate dbGetQuery dbWriteTable
#' @importFrom tibble data_frame
#' @param thisHandle the connection handle used for executing the insert/s
#' @param collectionName character vector denoting the collection
#' @param signatures character vector of signature names to associate with
#' collectionName parameter
#' @export
addCollectionSignatures <- function(thisHandle,
                                    collectionName = NULL,
                                    signatures = NULL) {
  collectionIdQuery <- "select
    signature_collection_id as collectionId
    from
      signature_collections
    where signature_collection_name = ?scn;"
  collectionId <- dbGetQuery(thisHandle,
                             sqlInterpolate(thisHandle, collectionIdQuery,
                                            scn = collectionName))$collectionId
  signatureIds <- sqlFindingQuery(
    "signatures",
    fields = c("signature_id"),
    ins = list("signature_name" = c(signatures))
  )$signature_id
  signatureCollectionDf <- data_frame(signatureIds = signatureIds,
                                      collectionId = rep(collectionId, length(signatureIds)))
  dbWriteTable(
    thisHandle,
    name = "temp_table",
    value = signatureCollectionDf,
    row.names = F,
    overwrite = T,
    append = F
  )
  dbGetQuery(
    thisHandle,
    "insert
       into
         signatures_to_collections(signature_id, signature_collection_id)
        select * from temp_table;"
  )
}
