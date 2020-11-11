#' @title addSignatureWrapper
#' @description Function that executes all required inserts and uploads of a signature
#' @importFrom xfun file_ext
#' @importFrom utils write.table
#' @param objectFile a json or rds file, formatted in the way of an
#' OmicSignature Object. It can also be a variable that is the object itself.
#' @param thisHandle Database connection handle
#' @param uploadPath where to upload the object file
#' @param thisUser the username of the submitter
#' @export
addSignatureWrapper <- function(objectFile, thisHandle, uploadPath, thisUser) {
  if (typeof(objectFile) == "character") {
    qceMessage <- objectUploadQC(objectFile)
    if (qceMessage != "") {
      stop(qceMessage)
    }
    extension <- tolower(file_ext(objectFile))
    if (extension == "json") {
      signature_object <- readJson(objectFile)
    }
    else if (extension == "rds") {
      signature_object <- readRDS(objectFile)
    }
    writeSignatureFile(uploadPath, signature_object, basename(objectFile))
  }
  else if (typeof(objectFile) == "environment") {
    # If you already have the object as a variable
    # in your environment(proper form would be an environment type)
    signature_object <- objectFile
    writeJson(objectFile, paste(uploadPath,
      paste0(signature_object$metadata$signature_name, ".json"),
      sep = "/"
    ))
  }
  else {
    stop("You need to upload with either an RDS file or a JSON file")
  }
  signature_name <- signature_object$metadata$signature_name
  if (signature_name == "" || is.null(signature_name)) {
    stop("STOP. YOU HAVE VIOLATED THE LAW. need to have a signature name in the metadata")
  }
  if (length(signature_object[["difexp"]]) > 0) {
    write.table(signature_object[["difexp"]], paste(uploadPath, paste0(signature_name, "_difexp.tsv"), sep = "/"))
  }
  fdr_cutoff <- 0.05
  logfc_cutoff <- 1
  these_signatures <- signature_object$signatures
  sig_meta <- signature_object$metadata
  print("adding signature information")
  # if signature meta already in database, don't try to
  # add it again and keep going with the other inserts
  last_sid <- as.integer(sqlFindingQuery("signatures", c("signature_id"),
    ins = list("signature_name" = sig_meta$signature_name)
  )$signature_id[1])
  if (is.na(last_sid)) {
    addSignature(sig_meta$signature_name,
      sig_meta$organism,
      sig_meta$platform,
      singleQuote(sig_meta$cell_lines),
      phenotype = sig_meta$phenotype,
      thisUser, uploadHandle = thisHandle,
      verbose = T, disconnectAfter = F
    )
  }
  else {
    print("signature meta info already added, moving on")
  }
  # since signature is inserted now, we can get its signature id
  # and feed it into the lvl2/3 upload function
  last_sid <- as.integer(sqlFindingQuery("signatures", c("signature_id"),
    ins = list("signature_name" = sig_meta$signature_name)
  )$signature_id[1])
  print("added signature info successfully. inserting level2")
  addLevel2(these_signatures,
    last_sid,
    sig_meta$signature_name,
    thisHandle,
    verbose = T
  )
  if (length(signature_object$metadata$keywords) != 0) {
    addSignatureKeywords(
      signature_object$metadata$keywords,
      last_sid, thisHandle
    )
  }
}
