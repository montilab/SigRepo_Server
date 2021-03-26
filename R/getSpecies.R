#' @title getSpecies
#'
#' @description gets species from the species table in a database
#' @param myIns optional named list with query parameters. see ?sqlFindingQuery
#' for more information.
#' @export
getSpecies <- function(myIns=NULL){
	sqlFindingQuery("species", ins=myIns)
}