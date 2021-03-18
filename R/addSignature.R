#' @title addSignature
#' @description Adds signature information to signatures table in the DB
#' @importFrom DBI dbSendQuery dbDisconnect
#' @param sig_name name of signature
#' @param species_name name of *species*
#' @param platform_name name of assay platform
#' @param cell_line name of cell line
#' @param phenotype character describing the phenotype of signature
#' @param uploader character: the user uploading the signature
#' @param uploadHandle connection handle: the connection handle to use
#' for uploading the signature
#' @param verbose default to FALSE. use TRUE to print messages
#' @param disconnectAfter Boolean: whether to disconnect the handle
#' when finished. 
#' all fields are usually obtained from input values from application
#' @export
addSignature <- function(sig_name, species_name, platform_name,
                         cell_line, phenotype=NULL, uploader, uploadHandle,
                         verbose=F, disconnectAfter=T) {
  phenotypeId <- phenotypeCheckOrAdd(phenotype, uploadHandle)
  thisSubmitterId <- sqlFindingQuery("submitters", c("submitter_id"),
    ins = list("submitter_name" = c(uploader))
  )$submitter_id
  now <- Sys.time()
  signature_name <- singleQuote(sig_name)
  my_species <- (species_name)
  species_id_insert <- sqlFindingQuery("species", c("species_id"),
    ins = list("species" = my_species)
  )$species_id[1]
  platform_id <- as.integer(sqlFindingQuery("assay_platforms",
    c("platform_id"),
    ins = list(
      "platform_name" = platform_name,
      "geo_platform_accession" = platform_name
    ),
    collapse_by = " OR "
  )$platform_id[1])
  insert_query <- paste("insert into signatures(
                         signature_name,
                         upload_date,
                         species_id,
                         cell_line,
                         platform_id,
                         phenotype_id,
                         submitter_id) values(", paste(signature_name, "now()",
    species_id_insert,
    cell_line, platform_id,
    phenotypeId, thisSubmitterId,
    sep = ","
  ), ");")
  if (verbose) {
    print(insert_query)
  }
  insert_signature_conn <- uploadHandle
  dbSendQuery(insert_signature_conn, insert_query)
  if (disconnectAfter) {
    dbDisconnect(insert_signature_conn)
  }
}
