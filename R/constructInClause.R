#' @title constructInClause
#'
#' @description Assemble part of a where clause of the form "<field> IN (<field_values>)"
#'
#' @param myList named list where the names are fields and the values are values
#'   associated with those fields
#' @param listKey the field name to access from myList
#' @export
constructInClause <- function(myList, listKey) {
  ## Construct clause
  inClause <- paste0(
    listKey,
    " IN (",
    paste(singleQuote(myList[[listKey]]), collapse = ","),
    ")"
  )

  ## If the values for this field include 'NA', then allow null values in query
  ##   "(<field> IN (<field_values>) OR <field> is null)"
  if ("NA" %in% myList[[listKey]]) {
    inClause <- paste0("(", inClause, " OR ", listKey, " is null)")
  }
  return(inClause)
}
