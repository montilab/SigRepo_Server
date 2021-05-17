#' @title addLevel2 data
#' @description function will insert lvl2/3 data into DB
#' @importFrom utils read.table
#' @importFrom dplyr bind_rows
#' @param lv2_file file path to lvl2/3 data file,
#' @param sid signature id,
#' @param sig_name signature_name
#' @param thisHandle connection handle to use
#' @param verbose print out messages as the function executes. False by default
#' @export
addLevel2 <- function(lv2_file, sid, sig_name, thisHandle, verbose=F) {
  # reading file into a table
  # also accounts for uploading from an omicsig object
  # that object would contain a dataframe(list) representation
  # of lv2
  if (typeof(lv2_file) == "list") {
    lv2Table <- lv2_file
  }
  else {
    print(typeof(lv2_file))
    lv2Table <- read.table(lv2_file, header = T)
  }
  if (verbose == T) {
    print(lv2Table$signature_symbol)
  }
  # fetching feature ids corresponding to symbols
  lv2_feature_ids <- sqlFindingQuery(
    "features",
    c("feature_name", "feature_id"),
    ins = list("feature_name" = c(as.character(lv2Table$signature_symbol)))
  )
  lv2Table$signature_symbol <- toupper(lv2Table$signature_symbol)
  lv2_feature_ids$feature_name <- toupper(lv2_feature_ids$feature_name)
  fids <- bind_rows(lv2_feature_ids)$feature_id
  fnames <- bind_rows(lv2_feature_ids)$feature_name
  # all coming from same signature, hence 'rep'
  sidCol <- rep(sid, length(fids))

  # debugging block.
  if (verbose == T) {
    print(lv2_feature_ids)
    print(lv2Table$signature_score)
    print(lv2Table$signature_direction)
    print(length(fids))
    print(length(sidCol))
    print(length(lv2Table$signature_score))
    print(length(lv2Table$signature_direction))
  }

  # dataframe for inserting into db
  # need to integrate synonym checker function here instead of just
  # merging what works(temporary solution)
  lv2Insert.df <- merge(lv2Table, lv2_feature_ids,
    by.x="signature_symbol",
    by.y="feature_name"
  )
  print(nrow(lv2Insert.df))
  # making insert query for all records at once. if transaction fails,
  # sql will rollback, which is what we want.
  insertRecords <- paste(
    "(",
    sidCol, ",",
    lv2Insert.df$feature_id, ",",
    lv2Insert.df$signature_score, ",",
    singleQuote(lv2Insert.df$signature_direction), ")",
    sep="", collapse=","
  )
  insert_lv2_query <- paste(
    "INSERT INTO feature_signature(signature_id,feature_id,weight,direction) VALUES ",
    insertRecords,
    sep=""
  )
  if (verbose == T) {
    print(insert_lv2_query)
  }
  # executes insert
  # don't DC the handle used to insert so the user can keep working with
  # that handle
  sqlGeneric(insert_lv2_query, thisHandle, disconnectAfter=F)
}
