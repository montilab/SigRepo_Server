#' @title addSignatureKeywords
#' @description  function for adding keyword-signature pairs
#' @importFrom dplyr bind_rows
#' @param keyword_v character vector containing keywords
#' @param sid signature id associated with keywords
#' @param keywordHandle connection handle for keyword-signature insertion to Database
#' @export
addSignatureKeywords <- function(keyword_v, sid, keywordHandle) {
  keywords <- unique(as.character(keyword_v))
  # what keywords already are in the DB?
  # isolate new keywords and insert into core table
  # keywordHandle = newConnHandle(thisUser=user, thisPassword = pw)
  keyword_sql <- sqlFindingQuery("keywords",
    c("keyword_id", "keyword"),
    ins = list("keyword" = c(keywords)), verbose = T,
    handle = keywordHandle, disconnectAfter = F
  )
  # new keywords
  new_keywords <- setdiff(keywords, keyword_sql$keyword)
  new_keyword_ids <- NULL
  # if there are any, add them to db
  if (length(new_keywords) != 0) {
    insert_newkeywords_query <- paste("INSERT INTO keywords(keyword) VALUES ", paste("(", singleQuote(new_keywords), ")", sep = "", collapse = ","), ";")
    # keywordHandle = newConnHandle(thisUser=user, thisPassword = pw)
    insert_newkeywords_sql <- sqlGeneric(insert_newkeywords_query, keywordHandle)
    new_keyword_ids <-
      sqlFindingQuery(
        fields = c("keyword_id"),
        dbTable = "keywords",
        ins = list("keyword" = c(new_keywords))
      )$keyword_id
  }
  keyword_ids_final <- keyword_sql$keyword_id
  if (!is.null(new_keyword_ids)) {
    keyword_ids_final <- bind_rows(keyword_sql$keyword_id, new_keyword_ids)
  }
  # make dataframe of insert values
  # if there's a function that mass inserts a dataframe into the db
  # i'd like to do that instead of all these pastes
  keyword_signature.df <- data.frame(
    signature_id = rep(sid, length(keyword_v)),
    keyword_id = keyword_ids_final
  )
  keyword_signature_insert_query <- paste(
    "INSERT INTO keyword_signature(signature_id,keyword_id) VALUES ",
    paste("(",
      keyword_signature.df$signature_id, ",",
      keyword_signature.df$keyword_id,
      ")",
      sep = "", collapse = ","
    ),
    ";"
  )
  keyword_handle <- keywordHandle
  sqlGeneric(keyword_signature_insert_query, keyword_handle)
}
