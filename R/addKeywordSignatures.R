#' @title addKeywordSignatures
#'
#' @description adds signature-keyword pairs to signatures_to_collections
#' table in the database.
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
  # Error Handling
  # should include creating new handle from user info if the handle provided
  # is either closed or corrupt
  # if(T){
  # 	print("add error handling code here")
  # }
  signatureIdQuery <- "select 
						  signature_id as signatureId
						from 
						  signatures 
						where signature_name = ?sn;"
  signatureId <- dbGetQuery(
    thisHandle,
    sqlInterpolate(thisHandle, signatureIdQuery,
      sn = signature
    )
  )$signatureId
  keywordIds <- sqlFindingQuery("keywords",
    fields = c("keyword_id"),
    ins = list("keyword" = c(keywords))
  )$keyword_id
  keywordIdsAlready <- sqlFindingQuery("keyword_signature_view",
    fields = c("keyword_id"),
    ins = list("signature_name" = c(signature))
  )$keyword_id
  keywordIds <- setdiff(keywordIds, keywordIdsAlready)
  signatureKeywordsDf <- data_frame(
    signatureId = rep(signatureId, length(keywordIds)),
    collectionId = keywordIds
  )
  dbWriteTable(thisHandle,
    name = "temp_table", value = signatureKeywordsDf,
    row.names = F, overwrite = T, append = F
  )
  dbGetQuery(thisHandle, "insert 
			   into 
			   keyword_signature(signature_id, keyword_id) 
			   select * from temp_table;")
}
