#' @title addFeatures
#'
#' @description inserts features into a database. Ensure that the user doing so
#' has insert privilages.
#' @param featureName string: name of the feature
#' @param referenceGenome string: name of the reference genome
#' @param featureType string: name of the feature type e.g. "gene"
#' @param handle: your connection handle. ensure you have insert privilages on 
#' your system
#' @export
addFeatures <- function(featureName, referenceGenome, featureType, handle){
	referenceGenomeId = sqlFindingQuery(
		"reference_genome",
		c("reference_genome_id"),
		ins = list("reference_genome" = referenceGenome)
	)$reference_genome_id
	featureTypeId = sqlFindingQuery(
		"feature_types",
		c("feature_type_id"),
		ins = list("feature_type" = featureType)
	)$feature_type_id
	insertQuery = paste(
		"insert 
			into 
				features(
					feature_name,
					reference_genome_id,
					feature_type_id
				)
		values(",
		paste(featureName,
					referenceGenomeId,
					featureTypeId,
					sep = ","),
		");"
	)
	sqlGeneric(insertQuery, handle, disconnectAfter = F)
}
