#'@title getAssayPlatforms
#'@description obtains the assay platforms within the database.
#'@export
getAssayPlatforms <- function(){
	sqlFindingQuery("assay_platforms")
}