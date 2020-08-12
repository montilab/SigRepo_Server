#' Add single quotation marks around a string
#'
#' @param myString the string to add single quotation marks to
singleQuote <- function(myString) {
	return(paste0("'", myString, "'"))
}