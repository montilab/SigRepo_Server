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
addLevel2 <- function(lv2_file, sid, sig_name, thisHandle, verbose = F) {
  # reading file into a table
  # also accounts for uploading from an omicsig object
  # that object would contain a dataframe(list) representation
  # of lv2
  if (typeof(lv2_file) == "list") {
    lv2_table <- lv2_file
  }
  else {
    lv2_table <- read.table(lv2_file, header = T)
  }
  if (verbose == T) {
    print(lv2_table$signature_symbol)
  }
  # fetching feature ids corresponding to symbols
  lv2_feature_ids <- sqlFindingQuery(
    "features",
    c("feature_name", "feature_id"),
    ins = list("feature_name" = c(as.character(lv2_table$signature_symbol)))
  )
  lv2_table$signature_symbol <- toupper(lv2_table$signature_symbol)
  lv2_feature_ids$feature_name <- toupper(lv2_feature_ids$feature_name)
  fids <- bind_rows(lv2_feature_ids)$feature_id
  fnames <- bind_rows(lv2_feature_ids)$feature_name
  # all coming from same signature, hence 'rep'
  sid_col <- rep(sid, length(fids))

  # debugging block. set to TRUE for printing variables to console
  if (verbose == T) {
    print(lv2_feature_ids)
    # print(fids)
    print(lv2_table$signature_score)
    print(lv2_table$signature_direction)
    print(length(fids))
    print(length(sid_col))
    print(length(lv2_table$signature_score))
    print(length(lv2_table$signature_direction))
  }


  # dataframe for inserting into db
  # need to integrate synonym checker function here instead of just
  # merging what works(temporary solution)
  lv2_insert.df <- merge(lv2_table, lv2_feature_ids,
    by.x = "signature_symbol",
    by.y = "feature_name"
  )
  print(nrow(lv2_insert.df))
  # making insert query for all records at once. if transaction fails,
  # sql will rollback, which is what we want.
  insert_records <- paste(
    "(",
    sid_col, ",",
    lv2_insert.df$feature_id, ",",
    lv2_insert.df$signature_score, ",",
    singleQuote(lv2_insert.df$signature_direction), ")",
    sep = "", collapse = ","
  )
  insert_lv2_query <- paste(
    "INSERT INTO feature_signature(signature_id,feature_id,weight,direction) VALUES ",
    insert_records,
    sep = ""
  )
  if (verbose == T) {
    print(insert_lv2_query)
  }
  # executes insert
  sqlGeneric(insert_lv2_query, thisHandle, disconnectAfter = F)
}
