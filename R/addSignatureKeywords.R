#' @title addSignatureKeywords
#' @description  function for adding keyword-signature pairs
#' @importFrom dplyr bind_rows
#' @param keyword_v character vector containing keywords
#' @param sid signature id associated with keywords
#' @param keywordHandle connection handle for keyword-signature insertion to Database
#' @param verbose verbosity. whether to output helpful print statements along the way
#' @export
addSignatureKeywords <- function(keyword_v, sid, keywordHandle, verbose=F) {
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
  if(verbose){
    keywordSql
  }
  newKeywords <- setdiff(keywords, keywordSql$keyword)
  if(length(newKeywords)>0 && verbose==T){
    print("Here are your new keywords to go into the DB")
    print(newKeywords)
  }
  else if(verbose==T){
    print("No new keywords to be added beforehand.")
  }
  newKeywordIds <- NULL
  # if there are any, add them to db
  if (length(newKeywords) != 0) {
    insertNewkeywordsQuery <- paste("INSERT INTO keywords(keyword) VALUES ", paste("(", singleQuote(newKeywords), ")", sep = "", collapse = ","), ";")
    if(verbose){
      print(insertNewkeywordsQuery)
    }
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
  if(verbose==T){
    print("inserting signature-keyword pairs with this query")
    print(keywordSignatureInsertQuery)
  }
  keywordHandle2 <- keywordHandle
  sqlGeneric(keywordSignatureInsertQuery, keywordHandle2)
  if(verbose){
    print("Finished adding new keywords(if any) and signature-keyword pairs")
  }
}
