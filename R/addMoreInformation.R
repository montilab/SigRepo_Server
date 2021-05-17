#' @title addMoreInformation
#' @description adds a JSON typed value into the DB associated with
#' a signature of interest
#' @importFrom stringr str_replace_all
#' @importFrom OmicSignature readJson
#' @importFrom jsonlite toJSON
#' @importFrom dplyr %>%
#' @param signatureName name of the signature
#' @param connHandle connection to the DB
#' @param rootDir directory of the signatures
#' @param objectExtension extension of object file. Important when you're saving
#' multiple different files of different natures about the same signature.
#' @param verbose default to FALSE. use TRUE to print messages
#' @export
addMoreInformation <- function(signatureName,
                               connHandle = newConnHandle(),
                               rootDir = Sys.getenv("signatureDirectory"),
                               objectExtension = "_obj.json",
                               verbose = FALSE) {
  # check if signature exists in db
  signatureIdQuery <-
    sprintf(
      "select signature_id from signatures where signature_name=%s;",
      singleQuote(signatureName)
    )
  signatureId <- sqlGeneric(signatureIdQuery,
                            connHandle)$signature_id
  print(signatureName)
  if (length(signatureId) == 0) {
    print(sprintf("No signature by the name %s exists", signatureName))
    return(F)
  }
  signatureObjectFile <-
    paste0(rootDir, signatureName, objectExtension)
  # get column names of table of interest
  signatureColumnsQuery <- "SELECT
  COLUMN_NAME
  FROM
  INFORMATION_SCHEMA.COLUMNS
  WHERE TABLE_NAME='signatures';"
  signatureColumns <- sqlGeneric(signatureColumnsQuery, newConnHandle(), T)$COLUMN_NAME %>% str_replace_all("_id", "")
  tryCatch({
    thisObject <- readJson(signatureObjectFile)
    thisObjectMetadata <- thisObject$metadata
    thisObjectMetadataFields <- names(thisObjectMetadata)
    if (verbose == T) {
      print("signatures columns")
      print(signatureColumns)
      print("metadata fields")
      print(thisObjectMetadataFields)
      print(which(
        !thisObjectMetadataFields %in% c(signatureColumns, "keywords")
      ))
    }
    # which metadata fields aren't represented in the table of interest?
    metadataNotInFieldsJSON <- thisObjectMetadata[which(!thisObjectMetadataFields %in% signatureColumns)] %>% toJSON()
    print(metadataNotInFieldsJSON)
    # if there aren't extra metadata fields, bail
    if (metadataNotInFieldsJSON == "[]") {
      print("No extra information provided in metadata")
      return(F)
    }
    else {
      sqlGeneric(
        sprintf(
          "update signatures set more_information=%s where signature_id=%i",
          singleQuote(metadataNotInFieldsJSON),
          signatureId
        ),
        connHandle,
        disconnectAfter = F
      )
    }
  },
  error = function(e) {
    print(e)
    return(F)
  })
  return(T)
}
