#' @title addKeywordSignatures
#'
#' @description adds signature-keyword pairs to keyword_signature
#' table in the database. This table captures a many to many relationship
#' between the signatures table and the keywords table, and contains
#' signature_id and keyword_id as foreign keys. Hence, before uploading, this
#' function first fetches the numerical IDs associated with the signatures and
#' keywords.
#'
#' @importFrom DBI sqlInterpolate dbGetQuery dbWriteTable
#' @importFrom tibble data_frame
#' @param thisHandle the connection handle used for executing the insert/s
#' @param keywords character vector denoting the keywords with which to
#' associate a signature
#' @param signature character vector of signature name to associate with
#' keywords parameter
#' @export
addKeywordSignatures <- function(thisHandle,
                                 keywords = NULL,
                                 signature = NULL) {
  # Get the signature ID of the signature of interest
  signatureIdQuery <- "select
            signature_id as signatureId
            from
              signatures
            where signature_name = ?sn;"
  signatureId <- dbGetQuery(thisHandle,
                            sqlInterpolate(thisHandle, signatureIdQuery,
                                           sn = signature))$signatureId
  # get keyword IDs of interest
  keywordIds <- sqlFindingQuery("keywords",
                                fields = c("keyword_id"),
                                ins = list("keyword" = c(keywords)))$keyword_id
  # get rid of the pairs that already exist in the database
  # so you can insert only pairs that don't exist already in the table
  keywordIdsAlready <- sqlFindingQuery(
    "keyword_signature_view",
    fields = c("keyword_id"),
    ins = list("signature_name" = c(signature))
  )$keyword_id
  keywordIds <- setdiff(keywordIds, keywordIdsAlready)
  signatureKeywordsDf <- data_frame(signatureId = rep(signatureId, length(keywordIds)),
                                    collectionId = keywordIds)
  # make a temp table in the db and then swap it into the target table
  dbWriteTable(
    thisHandle,
    name = "temp_table",
    value = signatureKeywordsDf,
    row.names = F,
    overwrite = T,
    append = F
  )
  dbGetQuery(
    thisHandle,
    "insert
        into
        keyword_signature(signature_id, keyword_id)
         select * from temp_table;"
  )
}
