#'@title getLevel2
#'@description obtains level2 representation of a signature
#'@param insList a named list of criteria you want to search by,
#'similar to sqlFindingQuery. defaults to getting every signature
#'@export
getLevel2 <- function(insList = NULL) {
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
    ins = insList
  )
}