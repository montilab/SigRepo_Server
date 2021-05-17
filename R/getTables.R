#' @title getTables
#'
#' @description lists all the table names in your SigRepo instance
#' @return a dataframe listing all table names
#' @export
getTables <- function() {
  sqlGeneric("SHOW TABLES;")
}