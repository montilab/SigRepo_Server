#' @title singleQuote
#' @description Add single quotation marks around a string
#'
#' @param myString the string to add single quotation marks to
#' @return a string, flanked by single quotes
#' @examples 
#' singleQuote("String to put in single quotes")
#' @export
singleQuote <- function(myString) {
  return(paste0("'", myString, "'"))
}
