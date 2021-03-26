#' @title statsByAttribute
#' @description obtain counts of signatures of each possible attribute of
#' interest in the Database
#' updated 2020/08/12
#' @param attribute the field name of which you want to get signature counts.
#' Keep in mind the possible attributes depend on the fields declared in your
#' instance of SigRepoR, if you have one. Possible legal entries include
#' 'phenotype', 'organism', 'assay_platform'. uses platform_signature_view
#' as reference view.
#' @return Dataframe with the following fields:
#' > "attribute"-column containing each possible value of said attribute
#' > "count"-the number of signatures belonging to each attribute
#' @export
statsByAttribute <- function(attribute) {
  sql <- paste("SELECT", attribute, ",count(signature_name) as 'count'
FROM
	platform_signature_view psv  where source_type is not null
GROUP BY", attribute, ";")
  return(sqlGeneric(sql))
}
