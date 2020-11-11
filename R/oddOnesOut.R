#' @title oddOnesOut
#' @description finds features that are not in your database but are in your file
#' @param your_obj your OmicSignatureObject
#' @export
oddOnesOut <- function(your_obj) {
  # fetching feature ids corresponding to symbols
  lv2_feature_ids <- sqlFindingQuery(
    "features",
    c("feature_name", "feature_id"),
    ins = list("feature_name" = c(your_obj$signature$signature_symbol))
  )
  fids <- bind_rows(lv2_feature_ids)$feature_id
  fnames <- bind_rows(lv2_feature_ids)$feature_name
  return(
    setdiff(
      toupper(your_obj$signature$signature_symbol),
      toupper(fnames)
    )
  )
}
