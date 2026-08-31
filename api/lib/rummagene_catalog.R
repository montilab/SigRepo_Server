# Rummagene catalog: build-time gate, storage, and query.
#
# The build job (api/lib/rummagene_catalog_build.R) streams Rummagene's
# latest.gmt, qualifies each set through api/lib/rummagene_ingest.R, and stores
# the survivors here. See specs/2026-08-31-rummagene-catalog-design.md.
#
# Depends on api/lib/rummagene_ingest.R and the `conn_handler` global in api.R.

if (!base::exists("%||%")) {
  `%||%` <- function(a, b) if (base::is.null(a)) b else a
}

# One GMT line -> list(term, description, genes, pmcid), or NULL when the line
# cannot yield a candidate.
#
# GMT is `term \t description \t gene \t gene \t ...`. Rummagene's terms start
# with the source article's PMC id, which is the only route to MeSH -- a term
# without one can never qualify, so it is dropped here rather than carried
# through the whole pipeline.
rummagene_parse_gmt_line <- function(line) {
  line <- base::as.character(line)[1]
  if (base::is.na(line) || !base::nzchar(base::trimws(line))) {
    return(NULL)
  }

  fields <- base::strsplit(line, "\t", fixed = TRUE)[[1]]
  if (base::length(fields) < 3) {
    return(NULL)
  }

  term <- base::trimws(fields[1])
  pmcid <- rummagene_pmcid_from_term(term)
  if (base::length(pmcid) == 0 || base::is.na(pmcid)) {
    return(NULL)
  }

  genes <- base::trimws(fields[-c(1, 2)])
  genes <- base::unique(genes[base::nzchar(genes)])
  if (base::length(genes) == 0) {
    return(NULL)
  }

  base::list(
    term = term,
    description = base::trimws(fields[2]),
    genes = genes,
    pmcid = pmcid
  )
}
