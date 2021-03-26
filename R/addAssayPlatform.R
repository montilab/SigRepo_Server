#' @title addAssayPlatform
#'
#' @description inserts an assay platform into the assay_platforms table
#' in your SigRepo database.
#' @param accession string: accession ID of the platform
#' @param name string: name of the platform
#' @param species string: species that the platform is used on
#' @param handle connection handle object. ensure the user has insert
#' privilages. see ?newConnHandle for more information
#' @export
addAssayPlatform <- function(accession, name, species, handle) {
	speciesId = sqlFindingQuery("species", ins = list("species" = species))$species_id
	insertQuery = paste0(
		"INSERT INTO assay_platforms(geo_platform_accession, platform_name, species_id)
		values('",
		accession,
		"', '",
		name,
		"',",
		speciesId,
		");"
	)
	print(insertQuery)
	sqlGeneric(insertQuery, handle)
}