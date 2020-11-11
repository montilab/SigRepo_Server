#' @title objectUploadQC
#' @description conducts Quality checking of the OmicSignature file to be uploaded
#' @param objectFile rds or json file representing your OmicSignature Object
#' @export
objectUploadQC <- function(objectFile) {
  this <- readJson(objectFile)
  thisMisfits <- oddOnesOut(this)
  platform_name <- this$metadata$platform
  validPlatform <- as.integer(sqlFindingQuery(
    "assay_platforms", c("platform_id"),
    ins = list(
      "platform_name" = platform_name,
      "geo_platform_accession" = platform_name
    ),
    collapse_by = " OR "
  )$platform_id[1])
  qualityChecking <- ""
  if (length(thisMisfits) > 0) {
    qualityChecking <- paste(
      qualityChecking, "These features don't exist in our DB:",
      paste(thisMisfits, collapse = ", "), "...\n"
    )
  }
  if (is.na(validPlatform)) {
    qualityChecking <- paste(
      qualityChecking, "The platform,",
      platform_name, ", is not a valid platform"
    )
  }
  return(qualityChecking)
}
