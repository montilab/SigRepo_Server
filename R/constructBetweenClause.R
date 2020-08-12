
#' Assemble part of a where clause of the form
#'   "<field> BETWEEN <value1> AND <value2>"
#'
#' @param myList named list where the names are fields and the values are values
#'   associated with those fields
#' @param listKey the field name to access from myList
#'
#' Note that myList[[listKey]] must have exactly two values
constructBetweenClause <- function(myList, listKey) {
	## Make sure myList[[listKey]] has exactly two values
	if (length(myList[[listKey]]) != 2) {
		stop(paste0(
			"You tried to construct a between clause with ",
			length(myList[[listKey]]),
			" element(s)!"
		))
	}
	## Construct clause
	betweenClause <- paste0(listKey,
							" BETWEEN ",
							paste0(singleQuote(myList[[listKey]][1:2]), collapse=" AND "))
	return(betweenClause)
}
