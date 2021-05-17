#'@title getTableFields
#'@description obtains the fields of a table of interest within the database.
#'@param tableName character denoting the name of a table in the database.
#'@export
getTableFields <- function(tableName) {
  sqlGeneric(paste("Show fields in ", tableName, " ;"))
}