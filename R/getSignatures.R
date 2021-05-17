#'@title getSignatures
#'@description obtains the metadata for a given list of signature Names
#'within the database. Note that signature_name is a unique identifier
#'in the database. If desired, you can obtain the level2 information for your signatures.
#'Level2 signature data is simply the list of significant features and their corresponding
#'weight and direction for a given signature.
#'@param signatureNames A character vector with the signature Names. By default,
#'setting this parameter to "ALL" gets every signature's metadata.
#'@param withLevel2 Boolean denoting whether you want to
#'@returns a named list of dataframes. "metadata" holds the signatures' metadata.
#'"level2" holds the level2 representation.
#'@export
getSignatures <- function(signatureNames = c("ALL"),
                          withLevel2 = F) {
  level2 = "Not requested."
  if (signatureNames != c("ALL")) {
    signatureMeta = sqlFindingQuery(
      "signature_view",
      ins = list("signature_name" = signatureNames))
  }
  else if (signatureNames == c("ALL")) {
    signatureMeta = sqlFindingQuery("signature_view")
  }
  if (withLevel2) {
    if (signatureNames != c("ALL")) {
      level2 = sqlFindingQuery("feature_signature_view",
                               ins = list("signature_name" = signatureNames))
    }
    else if (signatureNames == c("ALL")) {
      level2 = sqlFindingQuery("feature_signature_view",)
    }
  }
  return(list("metadata" = signatureMeta,
              "level2" = level2))
}
