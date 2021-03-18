#'@title getFeatures
#'@description obtains the assay platforms within the database.
#'@param nicely boolean: whether to query a feature view that looks nicer.
getFeatures <- function(nicely=F){
	if(nicely){
		sqlFindingQuery("feature_view")
	}
	else{
		sqlFindingQuery("features")
	}
}