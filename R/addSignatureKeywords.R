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
  keywordSql <- sqlFindingQuery("keywords",
    c("keyword_id", "keyword"),
    ins = list("keyword" = c(keywords)), verbose = T,
    handle = keywordHandle, disconnectAfter = F
  )
  # new keywords
  keywordSql
  newKeywords <- setdiff(keywords, keywordSql$keyword)
  newKeywordIds <- NULL
  # if there are any, add them to db
  if (length(newKeywords) != 0) {
    insertNewkeywordsQuery <- paste("INSERT INTO keywords(keyword) VALUES ", paste("(", singleQuote(newKeywords), ")", sep = "", collapse = ","), ";")
    # keywordHandle = newConnHandle(thisUser=user, thisPassword = pw)
    insertNewkeywordsSql <- sqlGeneric(insertNewkeywordsQuery, keywordHandle)
    newKeywordIds <-
      sqlFindingQuery(
        fields = c("keyword_id"),
        dbTable = "keywords",
        ins = list("keyword" = c(newKeywords))
      )$keyword_id
  }
  keywordIdsFinal <- keywordSql$keyword_id
  if (!is.null(newKeywordIds)) {
    keywordIdsFinal <- bind_rows(keywordSql$keyword_id, newKeywordIds)
  }
  # make dataframe of insert values
  # if there's a function that mass inserts a dataframe into the db
  # i'd like to do that instead of all these pastes
  keywordSignature.df <- data.frame(
    signature_id = rep(sid, length(keyword_v)),
    keyword_id = keywordIdsFinal
  )
  keywordSignatureInsertQuery <- paste(
    "INSERT INTO keyword_signature(signature_id,keyword_id) VALUES ",
    paste("(",
      keywordSignature.df$signature_id, ",",
      keywordSignature.df$keyword_id,
      ")",
      sep = "", collapse = ","
    ),
    ";"
  )
  keywordHandle2 <- keywordHandle
  sqlGeneric(keywordSignatureInsertQuery, keywordHandle2)
}
