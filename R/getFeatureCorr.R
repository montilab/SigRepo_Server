getFeatureCorr <- function(featureNames) {
	assertthat::are_equal(length(featureNamess), 2)
	sqlGeneric(paste0(
		"CALL featurePaired(",
		singleQuote(featureNames[1]),
		singleQuote(featureNames[2]),
		");"
	))
}