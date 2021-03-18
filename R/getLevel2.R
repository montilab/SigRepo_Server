getLevel2 <- function(insList=NULL){
	sqlFindingQuery(
		"feature_signature_view",
		c(
			"signature_name",
			"source_type",
			"phenotype",
			"feature_name",
			"feature_type",
			"weight",
			"direction"
		),
		ins=insList
	)
}