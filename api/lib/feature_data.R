# Organism-filter parsing shared by /update_transcriptomics and /update_proteomics.

parse_organism_filter <- function(organism) {
  if (base::length(organism) > 0 && base::all(!organism %in% c("", NA))) {
    organism <- base::sapply(base::seq_along(organism), function(i) {
      base::strsplit(organism[i], ",", fixed = TRUE)[[1]] |> base::trimws() |> base::as.character()
    }) |> base::as.vector() |> base::sort() |> base::unique()
  }

  organism
}
