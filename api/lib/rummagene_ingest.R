# Qualifying Rummagene gene sets for ingest into SigRepo as OmicSignatures.
#
# Rummagene (see api/lib/rummagene.R) serves ~1M gene sets scraped from PMC
# supplementary tables. Its schema carries NONE of the metadata SigRepo needs:
# GeneSet is term/geneIds/nGeneIds/created/description/hash/genes, and PmcInfo
# is pmcid/title/yr/doi. There is no organism field, no assay field, no
# phenotype, no direction. A filter for "sets where these are explicitly
# stated" therefore matches nothing in Rummagene alone.
#
# What Rummagene DOES give is a PMC id, and that is a key into PubMed's MeSH
# indexing -- descriptors assigned by NLM curators, not by us. So organism and
# assay_type are read out of MeSH and a set is accepted only when MeSH answers
# both unambiguously. Everything else is refused, with the field that failed
# recorded so a rejection can be audited.
#
# The rule, stated once: nothing here infers a value. A set either arrives with
# an authoritative answer or it does not qualify. The two fields Rummagene can
# never attest are handled by declaring what is actually true of the data:
#   phenotype      "unknown" -- a statement that we do not know, not a guess.
#   direction_type "uni-directional" -- an unordered gene list is exactly that,
#                  and it is the one direction_type whose checkSignature() does
#                  not demand a group_label, so nothing is fabricated there.
#
# Measured yield over 65 sampled papers (2026-08-31): 65/65 resolved to a PMID,
# 36 carried an unambiguous Homo sapiens / Mus musculus assignment, and 15 also
# carried an unambiguous assay type. Callers should expect to discard most of
# what they search.
#
# NOTE: MeSH describes the PAPER, not the individual supplementary table. It
# cannot tell you that a given table is a differential-expression contrast
# rather than a list of network nodes or a sample manifest. This module makes
# the metadata trustworthy; it does not make the set biologically meaningful.
# That judgement stays with whoever imports it.

if (!base::exists("%||%")) {
  `%||%` <- function(a, b) if (base::is.null(a)) b else a
}

# MeSH descriptors that name an organism SigRepo can hold, mapped to the value
# the `organisms` table stores.
RUMMAGENE_MESH_ORGANISM <- c(
  "Humans" = "Homo sapiens",
  "Mice"   = "Mus musculus"
)

# Species descriptors that disqualify a paper. Not exhaustive -- it does not
# need to be, because a paper is only ACCEPTED on a positive match above. This
# list exists to catch co-indexing: "Humans" plus "Rats" means the table could
# be either, so the human match must not be trusted.
RUMMAGENE_MESH_OTHER_SPECIES <- c(
  "Chickens", "Rats", "Zebrafish", "Swine", "Cattle", "Sheep", "Goats",
  "Dogs", "Horses", "Rabbits", "Drosophila melanogaster",
  "Saccharomyces cerevisiae", "Caenorhabditis elegans", "Xenopus laevis",
  "Arabidopsis", "Escherichia coli", "Macaca mulatta", "Pan troglodytes"
)

# MeSH descriptors that name an assay, mapped onto SigRepo's assay_type values.
# Several descriptors legitimately mean one assay (a transcriptomics paper is
# routinely indexed with both "Transcriptome" and "Gene Expression Profiling"),
# so agreement is collapsed rather than treated as ambiguity.
RUMMAGENE_MESH_ASSAY <- c(
  "Transcriptome"                          = "transcriptomics",
  "Gene Expression Profiling"              = "transcriptomics",
  "Sequence Analysis, RNA"                 = "transcriptomics",
  "RNA-Seq"                                = "transcriptomics",
  "High-Throughput Nucleotide Sequencing"  = "transcriptomics",
  "Oligonucleotide Array Sequence Analysis" = "transcriptomics",
  "Proteomics"                             = "proteomics",
  "Proteome"                               = "proteomics",
  "Metabolomics"                           = "metabolomics",
  "Metabolome"                             = "metabolomics",
  "DNA Methylation"                        = "methylomics",
  "Polymorphism, Single Nucleotide"        = "genetic_variants"
)

# The organism a paper's MeSH descriptors attest, or NULL when they do not
# attest exactly one that SigRepo holds. NULL covers four distinct situations
# -- unindexed, no organism descriptor, a species we do not hold, and two
# species at once -- all of which mean the same thing here: do not guess.
rummagene_mesh_organism <- function(mesh) {
  mesh <- base::as.character(mesh %||% base::character(0))
  if (base::length(mesh) == 0) {
    return(NULL)
  }

  # "Animals" is a qualifier PubMed adds alongside the species, never a species
  # itself, so it is simply not in the lookup and cannot match.
  matched <- base::unique(base::unname(RUMMAGENE_MESH_ORGANISM[
    base::intersect(mesh, base::names(RUMMAGENE_MESH_ORGANISM))
  ]))
  if (base::length(matched) != 1) {
    return(NULL)
  }
  if (base::length(base::intersect(mesh, RUMMAGENE_MESH_OTHER_SPECIES)) > 0) {
    return(NULL)
  }
  matched
}

# The assay type a paper's MeSH descriptors attest, or NULL when they attest
# none or more than one. A multi-omic paper is refused because the gene set
# could have come from either arm.
rummagene_mesh_assay_type <- function(mesh) {
  mesh <- base::as.character(mesh %||% base::character(0))
  if (base::length(mesh) == 0) {
    return(NULL)
  }

  matched <- base::unique(base::unname(RUMMAGENE_MESH_ASSAY[
    base::intersect(mesh, base::names(RUMMAGENE_MESH_ASSAY))
  ]))
  if (base::length(matched) != 1) {
    return(NULL)
  }
  matched
}

# The first PMC record attached to a gene set, or NULL. Mirrors
# rummagene_first_pmc() in rummagene.R but returns NULL rather than an empty
# list, because "no paper" is a rejection reason here rather than a missing
# display field.
.rummagene_pmc_record <- function(gene_set) {
  nodes <- base::tryCatch(gene_set$geneSetPmcsById$nodes, error = function(e) NULL)
  for (n in nodes %||% base::list()) {
    info <- n$pmcInfoByPmcid
    if (!base::is.null(info) && !base::is.null(info$pmcid)) {
      return(info)
    }
  }
  NULL
}

.rummagene_gene_symbols <- function(gene_set) {
  nodes <- base::tryCatch(gene_set$genes$nodes, error = function(e) NULL)
  syms <- base::vapply(
    nodes %||% base::list(),
    function(g) base::as.character(g$symbol %||% NA_character_),
    base::character(1)
  )
  syms <- base::trimws(syms)
  base::unique(syms[!base::is.na(syms) & base::nzchar(syms)])
}

# Decide whether one Rummagene gene set may be ingested.
#
# `mesh` is the paper's MeSH descriptors, fetched separately (see
# rummagene_parse_mesh_xml) so this stays a pure function -- the whole rule is
# testable without touching the network.
#
# Returns list(ok = TRUE, organism, assay_type, genes, pmcid, ...) or
# list(ok = FALSE, reason = <field that failed>). `reason` is one of
# "no_pmc", "too_few_genes", "no_mesh", "organism", "assay_type".
#
# min_genes matches rummagene_enrich()'s own floor: below two symbols there is
# no set to speak of.
rummagene_qualify <- function(gene_set, mesh, min_genes = 2) {
  pmc <- .rummagene_pmc_record(gene_set)
  if (base::is.null(pmc)) {
    return(base::list(ok = FALSE, reason = "no_pmc"))
  }

  genes <- .rummagene_gene_symbols(gene_set)
  if (base::length(genes) < min_genes) {
    return(base::list(ok = FALSE, reason = "too_few_genes"))
  }

  mesh <- base::as.character(mesh %||% base::character(0))
  if (base::length(mesh) == 0) {
    return(base::list(ok = FALSE, reason = "no_mesh"))
  }

  organism <- rummagene_mesh_organism(mesh)
  if (base::is.null(organism)) {
    return(base::list(ok = FALSE, reason = "organism"))
  }

  assay_type <- rummagene_mesh_assay_type(mesh)
  if (base::is.null(assay_type)) {
    return(base::list(ok = FALSE, reason = "assay_type"))
  }

  base::list(
    ok         = TRUE,
    term       = gene_set$term %||% NA_character_,
    description = gene_set$description %||% NA_character_,
    genes      = genes,
    organism   = organism,
    assay_type = assay_type,
    pmcid      = pmc$pmcid,
    title      = pmc$title %||% NA_character_,
    year       = pmc$yr %||% NA_integer_,
    doi        = pmc$doi %||% NA_character_,
    mesh       = mesh
  )
}

# Build the OmicSignature for a qualified candidate.
#
# Only the five mandatory metadata fields are asserted, plus provenance. Three
# come from an authoritative source; the other two say what is true of the data
# rather than guessing (see the header).
rummagene_build_signature <- function(qualified) {
  if (!base::isTRUE(qualified$ok)) {
    base::stop(
      "This Rummagene gene set did not qualify for ingest (reason: ",
      qualified$reason %||% "unknown", "). Only sets whose paper attests both ",
      "organism and assay_type in MeSH can be built."
    )
  }

  # Recorded so a reader can re-check every asserted field against the source.
  provenance <- base::sprintf(
    "source=rummagene; pmcid=%s; organism and assay_type from PubMed MeSH (%s); phenotype not stated by source",
    qualified$pmcid,
    base::paste(base::intersect(
      qualified$mesh,
      c(base::names(RUMMAGENE_MESH_ORGANISM), base::names(RUMMAGENE_MESH_ASSAY))
    ), collapse = ", ")
  )

  metadata <- base::list(
    signature_name = qualified$term,
    phenotype      = "unknown",
    organism       = qualified$organism,
    direction_type = "uni-directional",
    assay_type     = qualified$assay_type,
    description    = qualified$description,
    year           = qualified$year,
    others         = provenance
  )

  # feature_name only: uni-directional needs no group_label, and OmicSignature
  # fills probe_id itself with a positional index.
  signature <- base::data.frame(
    feature_name = qualified$genes,
    stringsAsFactors = FALSE
  )

  OmicSignature::OmicSignature$new(metadata = metadata, signature = signature)
}

# ---------------------------------------------------------------- NCBI I/O ---

# NCBI's PMC -> PMID converter. Returns a named character vector keyed by PMC
# id; records the converter could not resolve are dropped rather than carried
# as NA, since a candidate without a PMID cannot be qualified at all.
rummagene_parse_idconv <- function(json_text) {
  payload <- jsonlite::fromJSON(json_text, simplifyVector = FALSE)
  out <- base::character(0)
  for (rec in payload$records %||% base::list()) {
    if (base::is.null(rec$pmid) || base::is.null(rec$pmcid)) {
      next
    }
    out[[base::as.character(rec$pmcid)]] <- base::as.character(rec$pmid)
  }
  out
}

# PubMed efetch XML -> list of MeSH descriptors keyed by PMID. An article with
# no MeshHeadingList yields character(0), which rummagene_qualify() reads as
# "no_mesh" -- distinct from the article being absent from the response.
rummagene_parse_mesh_xml <- function(xml_text) {
  doc <- xml2::read_xml(xml_text)
  out <- base::list()
  for (art in xml2::xml_find_all(doc, ".//PubmedArticle")) {
    pmid <- xml2::xml_text(xml2::xml_find_first(art, ".//MedlineCitation/PMID"))
    if (base::is.na(pmid) || !base::nzchar(pmid)) {
      next
    }
    descriptors <- xml2::xml_text(
      xml2::xml_find_all(art, ".//MeshHeadingList/MeshHeading/DescriptorName")
    )
    out[[pmid]] <- base::as.character(descriptors)
  }
  out
}

# Qualify a batch of gene sets against a pmcid -> MeSH lookup.
#
# `mesh_by_pmcid` is keyed by PMC id, not PMID, because that is the key the
# gene sets themselves carry -- the caller does the pmcid -> pmid -> MeSH join
# once (see rummagene_fetch_mesh_by_pmcid) and hands the result here.
#
# Returns list(qualified, rejected, n_examined). `rejected` counts every
# reason, including the ones that did not occur, so a caller reporting yield
# can always account for the full batch rather than implying the discards were
# uninteresting.
rummagene_qualify_all <- function(gene_sets, mesh_by_pmcid, min_genes = 2) {
  reasons <- c("no_pmc", "too_few_genes", "no_mesh", "organism", "assay_type")
  rejected <- stats::setNames(base::as.list(base::rep(0, base::length(reasons))), reasons)
  qualified <- base::list()

  for (gs in gene_sets %||% base::list()) {
    pmc <- .rummagene_pmc_record(gs)
    # An article efetch had no record of is simply absent from the lookup;
    # character(0) then routes it to "no_mesh" like any other unindexed paper.
    mesh <- if (base::is.null(pmc)) {
      base::character(0)
    } else {
      mesh_by_pmcid[[base::as.character(pmc$pmcid)]] %||% base::character(0)
    }

    q <- rummagene_qualify(gs, mesh = mesh, min_genes = min_genes)
    if (base::isTRUE(q$ok)) {
      qualified[[base::length(qualified) + 1]] <- q
    } else {
      rejected[[q$reason]] <- rejected[[q$reason]] + 1
    }
  }

  base::list(
    qualified  = qualified,
    rejected   = rejected,
    n_examined = base::length(gene_sets %||% base::list())
  )
}

# Rummagene term search, pulling the gene members the enrich query in
# rummagene.R does not need. `max_genes` caps how many members come back per
# set; a set larger than that is still reported at its true nGeneIds.
RUMMAGENE_TERM_SEARCH_QUERY <- "query search($terms:[String]!, $first:Int, $maxGenes:Int){
  geneSetTermSearch(terms:$terms, first:$first){
    totalCount
    nodes {
      term description nGeneIds
      genes(first:$maxGenes){ nodes { symbol } }
      geneSetPmcsById(first:1){ nodes { pmcInfoByPmcid { pmcid title yr doi } } }
    }
  }
}"

rummagene_search_genesets <- function(terms, limit = 25, max_genes = 2000, timeout = 60) {
  res <- httr::POST(
    RUMMAGENE_GRAPHQL_URL,
    body = base::list(
      query = RUMMAGENE_TERM_SEARCH_QUERY,
      variables = base::list(
        terms = base::as.list(base::as.character(terms)),
        first = base::as.integer(limit),
        maxGenes = base::as.integer(max_genes)
      )
    ),
    encode = "json", httr::content_type_json(), httr::timeout(timeout)
  )
  if (httr::status_code(res) != 200) {
    base::stop(base::sprintf("Rummagene term search failed (HTTP %s).", httr::status_code(res)))
  }
  payload <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
  if (!base::is.null(payload$errors)) {
    base::stop(base::sprintf(
      "Rummagene GraphQL error: %s",
      base::tryCatch(payload$errors[[1]]$message, error = function(e) "unknown error")
    ))
  }
  payload$data$geneSetTermSearch$nodes %||% base::list()
}

# NCBI endpoints. The idconv host moved in 2025 (the old
# www.ncbi.nlm.nih.gov/pmc/utils/idconv path now 301s), so the current one is
# recorded here rather than left to a redirect that may not always be followed.
NCBI_IDCONV_URL <- base::Sys.getenv(
  "NCBI_IDCONV_URL",
  unset = "https://pmc.ncbi.nlm.nih.gov/tools/idconv/api/v1/articles/"
)
NCBI_EFETCH_URL <- base::Sys.getenv(
  "NCBI_EFETCH_URL",
  unset = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi"
)

# pmcid -> MeSH descriptors, via PMC -> PMID -> PubMed. Batched, because NCBI
# rate-limits to roughly 3 requests/second without an API key and a search can
# easily involve hundreds of papers.
#
# Returns a named list keyed by PMC id. A paper that could not be resolved, or
# that PubMed holds without MeSH indexing, is simply absent or empty --
# rummagene_qualify_all() reads either as "no_mesh".
rummagene_fetch_mesh_by_pmcid <- function(pmcids, batch_size = 100, timeout = 90,
                                          pause = 0.4) {
  pmcids <- base::unique(base::as.character(pmcids))
  pmcids <- pmcids[!base::is.na(pmcids) & base::nzchar(pmcids)]
  if (base::length(pmcids) == 0) {
    return(base::list())
  }

  pmid_of <- base::character(0)
  for (i in base::seq(1, base::length(pmcids), by = batch_size)) {
    chunk <- pmcids[i:base::min(i + batch_size - 1, base::length(pmcids))]
    res <- httr::GET(
      NCBI_IDCONV_URL,
      query = base::list(format = "json", ids = base::paste(chunk, collapse = ",")),
      httr::timeout(timeout)
    )
    if (httr::status_code(res) == 200) {
      parsed <- base::tryCatch(
        rummagene_parse_idconv(httr::content(res, as = "text", encoding = "UTF-8")),
        error = function(e) base::character(0)
      )
      pmid_of <- c(pmid_of, parsed)
    }
    base::Sys.sleep(pause)
  }
  if (base::length(pmid_of) == 0) {
    return(base::list())
  }

  mesh_by_pmid <- base::list()
  pmids <- base::unname(pmid_of)
  for (i in base::seq(1, base::length(pmids), by = batch_size)) {
    chunk <- pmids[i:base::min(i + batch_size - 1, base::length(pmids))]
    res <- httr::GET(
      NCBI_EFETCH_URL,
      query = base::list(db = "pubmed", retmode = "xml", id = base::paste(chunk, collapse = ",")),
      httr::timeout(timeout)
    )
    if (httr::status_code(res) == 200) {
      parsed <- base::tryCatch(
        rummagene_parse_mesh_xml(httr::content(res, as = "text", encoding = "UTF-8")),
        error = function(e) base::list()
      )
      mesh_by_pmid <- c(mesh_by_pmid, parsed)
    }
    base::Sys.sleep(pause)
  }

  # Re-key onto the PMC ids the gene sets actually carry.
  out <- base::list()
  for (pmcid in base::names(pmid_of)) {
    out[[pmcid]] <- mesh_by_pmid[[pmid_of[[pmcid]]]] %||% base::character(0)
  }
  out
}

# The whole pipeline: search Rummagene by term, ask PubMed what its papers are
# about, and return only the sets whose metadata is fully attested.
#
# The result deliberately carries the rejection tally alongside the survivors so
# a caller can always say what was discarded and why.
rummagene_candidates <- function(terms, limit = 25, max_genes = 2000, min_genes = 2) {
  gene_sets <- rummagene_search_genesets(terms, limit = limit, max_genes = max_genes)
  if (base::length(gene_sets) == 0) {
    return(base::list(qualified = base::list(), rejected = base::list(), n_examined = 0))
  }

  pmcids <- base::vapply(gene_sets, function(gs) {
    pmc <- .rummagene_pmc_record(gs)
    if (base::is.null(pmc)) NA_character_ else base::as.character(pmc$pmcid)
  }, base::character(1))

  mesh_by_pmcid <- rummagene_fetch_mesh_by_pmcid(pmcids[!base::is.na(pmcids)])
  rummagene_qualify_all(gene_sets, mesh_by_pmcid, min_genes = min_genes)
}

# signatures.signature_name is VARCHAR(255) and Rummagene terms routinely run
# longer. Truncating alone is not safe: UNIQUE(signature_name, user_name) means
# two sibling tables from one paper -- identical for their first 300 characters
# and differing only in a trailing "-cluster_1" / "-cluster_2" -- would collide
# and the second pull would be rejected as a duplicate. So a term that needs
# truncating carries a short digest of its FULL text, which restores
# uniqueness. The untruncated term is always preserved in
# rummagene_catalog.term and in the signature's `others` provenance string.
RUMMAGENE_SIGNATURE_NAME_MAX <- 255L

rummagene_signature_name <- function(term) {
  term <- base::as.character(term)[1]
  if (base::nchar(term) <= RUMMAGENE_SIGNATURE_NAME_MAX) {
    return(term)
  }
  # serialize = FALSE, not the digest::digest() default: this hash is
  # persisted in signatures.signature_name, so it must depend only on the
  # string's own bytes. The default (serialize = TRUE) hashes R's internal
  # serialize() byte stream instead, which is free to change across R
  # versions -- if it ever did, re-pulling the same term after an R upgrade
  # would produce a different signature_name, UNIQUE(signature_name,
  # user_name) would never catch the repeat, and the signature would be
  # silently duplicated.
  digest <- base::substr(
    digest::digest(term, algo = "md5", serialize = FALSE),
    1, 8
  )
  suffix <- base::paste0("~", digest)
  base::paste0(
    base::substr(term, 1, RUMMAGENE_SIGNATURE_NAME_MAX - base::nchar(suffix)),
    suffix
  )
}
