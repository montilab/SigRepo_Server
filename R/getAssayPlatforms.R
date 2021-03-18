#'@title getAssayPlatforms
#'@description obtains the assay platforms within the database.
getAssayPlatforms <- function(){
	sqlFindingQuery("assay_platforms")
}