# Rummagene integration: enrich a gene set against rummagene.com's ~1M
# literature-mined gene sets (extracted from supplementary tables of PMC
# articles) via their public GraphQL API.
#   Rummagene: Communications Biology 2024, PMID 38643247, https://rummagene.com
#
# The endpoint is public (no auth) and we keep the dependency thin: one POST per
# enrichment, parsed into a tidy list the API/UI can render. Rummagene returns a
# `term` that encodes the source (PMCxxxx-<file>-<table>-<description>); we also
# pull the structured PMC info (pmcid/title/doi/year) so the UI can link back to
# the originating paper.

# Guarded so this file is also sourceable standalone (outside the API, where the
# shared common.R %||% isn't loaded) for testing against the live endpoint.
if (!base::exists("%||%")) {
  `%||%` <- function(a, b) if (base::is.null(a)) b else a
}

RUMMAGENE_GRAPHQL_URL <- base::Sys.getenv(
  "RUMMAGENE_GRAPHQL_URL",
  unset = "https://rummagene.com/graphql"
)

# Verified against the live schema (Background.enrich -> PaginatedEnrichResult).
RUMMAGENE_ENRICH_QUERY <- "query enrich($genes:[String]!, $first:Int, $overlapGe:Int, $pvalueLe:Float, $adjPvalueLe:Float, $filterTerm:String){
  currentBackground {
    enrich(genes:$genes, first:$first, overlapGe:$overlapGe, pvalueLe:$pvalueLe, adjPvalueLe:$adjPvalueLe, filterTerm:$filterTerm){
      totalCount
      nodes {
        pvalue adjPvalue oddsRatio nOverlap
        geneSets {
          nodes {
            term description nGeneIds
            geneSetPmcsById { nodes { pmcInfoByPmcid { pmcid title yr doi } } }
          }
        }
      }
    }
  }
}"

# PMCxxxxxxx prefix of a Rummagene term, when structured PMC info is absent.
rummagene_pmcid_from_term <- function(term) {
  if (base::is.null(term)) {
    return(NA_character_)
  }
  m <- base::regmatches(term, base::regexpr("^PMC[0-9]+", term))
  if (base::length(m) == 0) NA_character_ else m
}

# First structured PMC record attached to a gene set (pmcid/title/doi/yr), or an
# empty list if none.
rummagene_first_pmc <- function(gs) {
  nodes <- base::tryCatch(gs$geneSetPmcsById$nodes, error = function(e) NULL)
  for (n in nodes %||% base::list()) {
    info <- n$pmcInfoByPmcid
    if (!base::is.null(info)) {
      return(info)
    }
  }
  base::list()
}

# One enrich node -> a flat, JSON-friendly record. A single gene-set hash can be
# shared by several papers; we take the first as representative and report how
# many share it (n_sets).
rummagene_hit <- function(node) {
  gs_nodes <- base::tryCatch(node$geneSets$nodes, error = function(e) NULL)
  if (base::is.null(gs_nodes) || base::length(gs_nodes) == 0) {
    return(NULL)
  }
  gs <- gs_nodes[[1]]
  term <- gs$term %||% ""
  pmc <- rummagene_first_pmc(gs)
  pmcid <- (pmc$pmcid %||% NULL) %||% rummagene_pmcid_from_term(term)

  base::list(
    term = term,
    description = gs$description %||% NA_character_,
    n_geneset = gs$nGeneIds %||% NA_integer_,
    n_overlap = node$nOverlap %||% NA_integer_,
    odds_ratio = node$oddsRatio %||% NA_real_,
    pvalue = node$pvalue %||% NA_real_,
    adj_pvalue = node$adjPvalue %||% NA_real_,
    n_sets = base::length(gs_nodes),
    pmcid = pmcid %||% NA_character_,
    title = pmc$title %||% NA_character_,
    doi = pmc$doi %||% NA_character_,
    year = pmc$yr %||% NA_integer_,
    pmc_url = if (!base::is.null(pmcid) && !base::is.na(pmcid) && base::nzchar(pmcid)) {
      base::sprintf("https://www.ncbi.nlm.nih.gov/pmc/articles/%s/", pmcid)
    } else {
      NA_character_
    }
  )
}

# Enrich a gene set against Rummagene. Returns list(total_count, query_size, hits).
rummagene_enrich <- function(genes, limit = 25, min_overlap = 2, max_pvalue = 0.05,
                             filter_term = NULL, timeout = 30) {
  genes <- base::unique(base::toupper(base::trimws(base::as.character(genes))))
  genes <- genes[!genes %in% c("", NA)]
  if (base::length(genes) < 2) {
    base::stop("Provide at least two gene symbols to enrich against Rummagene.")
  }

  variables <- base::list(
    genes = base::as.list(genes),
    first = base::as.integer(limit),
    overlapGe = base::as.integer(min_overlap),
    pvalueLe = max_pvalue,
    adjPvalueLe = 1
  )
  if (!base::is.null(filter_term) && base::nzchar(filter_term)) {
    variables$filterTerm <- filter_term
  }

  res <- httr::POST(
    RUMMAGENE_GRAPHQL_URL,
    body = base::list(query = RUMMAGENE_ENRICH_QUERY, variables = variables),
    encode = "json",
    httr::content_type_json(),
    httr::timeout(timeout)
  )
  if (httr::status_code(res) != 200) {
    base::stop(base::sprintf("Rummagene API request failed (HTTP %s).", httr::status_code(res)))
  }

  payload <- jsonlite::fromJSON(
    httr::content(res, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
  if (!base::is.null(payload$errors)) {
    msg <- base::tryCatch(payload$errors[[1]]$message, error = function(e) "unknown error")
    base::stop(base::sprintf("Rummagene GraphQL error: %s", msg))
  }

  enrich <- payload$data$currentBackground$enrich
  if (base::is.null(enrich)) {
    return(base::list(total_count = 0, query_size = base::length(genes), hits = base::list()))
  }

  hits <- base::lapply(enrich$nodes %||% base::list(), rummagene_hit)
  hits <- base::Filter(base::Negate(base::is.null), hits)

  base::list(
    total_count = enrich$totalCount %||% base::length(hits),
    query_size = base::length(genes),
    hits = hits
  )
}

# Resolve a signature's CURATED gene symbols from its difexp table. This is the
# fallback for signatures whose features carry no gene_symbol in SigRepo's
# reference tables (e.g. non-human signatures stored as Ensembl IDs) but whose
# difexp file carries symbols directly. We join the signature's curated feature
# set to the difexp by probe_id so we return the SIGNATURE's genes, not the whole
# difexp background. Returns a character vector of symbols, or NULL if none.
# Depends on fetch_signature_context() (signature.R) and load_difexp_rds()
# (difexp.R), both resolved at call time.
rummagene_signature_symbols_from_difexp <- function(auth, signature_hashkey, difexp_dir) {
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = TRUE, max_features = 100000, auth = auth
  )
  if (base::is.null(context) || base::length(context$features) == 0) {
    return(NULL)
  }
  if (!base::isTRUE(base::as.logical(context$signature$has_difexp))) {
    return(NULL)
  }

  difexp <- base::tryCatch(load_difexp_rds(difexp_dir, signature_hashkey), error = function(e) NULL)
  if (base::is.null(difexp) || !base::is.data.frame(difexp) || base::nrow(difexp) == 0) {
    return(NULL)
  }

  cols <- base::colnames(difexp)
  sym_candidates <- c("gene_symbol", "symbol", "geneSymbol", "gene", "hgnc_symbol", "mgi_symbol")
  sym_col <- base::intersect(sym_candidates, cols)[1]
  if (base::is.na(sym_col)) {
    return(NULL)
  }
  symbols <- base::trimws(base::as.character(difexp[[sym_col]]))

  # Prefer restricting to the signature's curated features (join by probe_id).
  curated_probe <- base::vapply(
    context$features, function(f) base::as.character(f$probe_id %||% NA), character(1)
  )
  curated_probe <- curated_probe[!base::is.na(curated_probe) & base::nzchar(curated_probe)]

  if ("probe_id" %in% cols && base::length(curated_probe) > 0) {
    dmap <- stats::setNames(symbols, base::as.character(difexp$probe_id))
    picked <- dmap[curated_probe]
  } else {
    # No probe_id join key -- fall back to the difexp's own symbols.
    picked <- symbols
  }

  picked <- base::unique(picked[!base::is.na(picked) & base::nzchar(picked)])
  if (base::length(picked) == 0) NULL else base::unname(picked)
}
