# Ortholog lookup over the gene_orthologs reference table, backing
# GET /features/orthologs (api/api.R). The same join the SigRepo client runs
# in mapOrthologs(), kept here so the Shiny app and the MCP server can
# translate ids without the R client installed.
#
# Two steps. Within the source organism, a UniProt accession resolves to its
# gene symbol through proteomics_features and an Ensembl gene id through
# transcriptomics_features (both keyed on (feature_name, organism_id); the
# reference tables hold no Entrez column, so an Entrez id joins the ortholog
# table directly). Across organisms, the symbol or Entrez id joins
# gene_orthologs, which holds babelgene's mouse to human pairs: human is
# always one side because babelgene maps only to and from human, so exactly
# one of `from` and `to` must be Homo sapiens.
#
# Every ortholog is returned (a gene with several yields several rows,
# flagged ambiguous) and an id that maps to nothing is kept as an NA row, so
# a caller sees coverage without a second request.
#
# Deliberately dependency-free beyond DBI, like api/lib/symbols.R. Depends on
# api/lib/common.R only for nothing: it is sourced beside it.

ORTHOLOG_RESULT_COLUMNS <- base::c(
  "query", "query_type", "query_symbol", "organism", "symbol", "ensembl", "entrez", "support_n", "ambiguous"
)

# Same rules as the SigRepo client's classifyFeatureIdType(); keep the two in
# step. Ensembl gene ids: ENS, optional species letters, G, six or more digits.
# UniProt accessions: the UniProt pattern with an optional isoform suffix.
# Digits only: Entrez. Anything else: a gene symbol. Blank: NA.
classify_feature_id_type <- function(ids) {
  x <- base::trimws(base::as.character(ids))
  out <- base::rep(NA_character_, base::length(x))
  known <- !base::is.na(x) & base::nzchar(x)
  is_ensembl <- known & base::grepl("^ENS[A-Z]*G[0-9]{6,}", x, ignore.case = TRUE)
  is_uniprot <- known & !is_ensembl &
    base::grepl("^([OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})(-[0-9]+)?$", x)
  is_entrez <- known & !is_ensembl & !is_uniprot & base::grepl("^[0-9]+$", x)
  out[is_ensembl] <- "ensembl"
  out[is_uniprot] <- "uniprot"
  out[is_entrez] <- "entrez"
  out[known & base::is.na(out)] <- "symbol"
  out
}

# Identifiers as text: blanks become NA and numbers are written out in full
# (as.character(100000000) would give "1e+08").
ortholog_id_text <- function(x) {
  out <- if (base::is.numeric(x)) {
    base::ifelse(base::is.na(x), NA_character_, base::format(x, scientific = FALSE, trim = TRUE))
  } else {
    base::trimws(base::as.character(x))
  }
  out[!base::is.na(out) & !base::nzchar(out)] <- NA_character_
  out
}

ortholog_chunks <- function(x, size = 1000) {
  x <- base::unique(x[!base::is.na(x)])
  if (base::length(x) == 0) return(base::list())
  base::split(x, base::ceiling(base::seq_along(x) / size))
}

ortholog_quote_list <- function(conn, x) {
  base::paste(base::vapply(x, function(v) base::as.character(DBI::dbQuoteLiteral(conn, v)), base::character(1)), collapse = ", ")
}

# feature_name -> gene_symbol rows of one reference table for the given names
# and organism. Superseded rows take part too: a retired accession still names
# the gene it always did, and older signatures still carry it.
ortholog_reference_symbols <- function(conn, ref_table, feature_names, organism_id) {
  frames <- base::lapply(ortholog_chunks(feature_names), function(chunk) {
    base::suppressWarnings(DBI::dbGetQuery(conn, base::sprintf(
      "SELECT feature_name, gene_symbol FROM %s WHERE organism_id = %s AND feature_name IN (%s)",
      ref_table, base::as.character(organism_id), ortholog_quote_list(conn, chunk)
    )))
  })
  if (base::length(frames) == 0) {
    return(base::data.frame(feature_name = base::character(), gene_symbol = base::character(), stringsAsFactors = FALSE))
  }
  base::do.call(base::rbind, frames)
}

# The within-species step: query -> query_symbol (NA when unknown).
ortholog_query_symbols <- function(conn, ids, id_types, organism_id) {
  out <- base::data.frame(query = ids, query_symbol = base::rep(NA_character_, base::length(ids)), stringsAsFactors = FALSE)
  is_symbol <- !base::is.na(id_types) & id_types == "symbol"
  out$query_symbol[is_symbol] <- ids[is_symbol]
  sources <- base::c(uniprot = "proteomics_features", ensembl = "transcriptomics_features")
  for (type in base::names(sources)) {
    sel <- !base::is.na(id_types) & id_types == type
    if (!base::any(sel)) next
    found <- ortholog_reference_symbols(conn, sources[[type]], ids[sel], organism_id)
    hit <- base::match(base::tolower(ids), base::tolower(found$feature_name))
    resolved <- sel & !base::is.na(hit)
    symbol <- base::trimws(base::as.character(found$gene_symbol[hit[resolved]]))
    symbol[base::is.na(symbol) | !base::nzchar(symbol)] <- NA_character_
    out$query_symbol[resolved] <- symbol
  }
  out
}

# The cross-species step: current pairs of the non-human organism whose
# source-side symbol or Entrez id is among those given, with the other side.
ortholog_pairs <- function(conn, symbols, entrez_ids, non_human_organism_id, from_is_human) {
  if (from_is_human) {
    select <- "human_symbol AS query_symbol, human_entrez_id AS query_entrez, gene_symbol AS symbol, ensembl_gene_id AS ensembl, entrez_id AS entrez, support_n"
    symbol_col <- "human_symbol"
    entrez_col <- "human_entrez_id"
  } else {
    select <- "gene_symbol AS query_symbol, entrez_id AS query_entrez, human_symbol AS symbol, human_ensembl_gene_id AS ensembl, human_entrez_id AS entrez, support_n"
    symbol_col <- "gene_symbol"
    entrez_col <- "entrez_id"
  }
  prefix <- base::sprintf(
    "SELECT %s FROM gene_orthologs WHERE is_current = 1 AND organism_id = %s AND ",
    select, base::as.character(non_human_organism_id)
  )
  run <- function(where) base::suppressWarnings(DBI::dbGetQuery(conn, base::paste0(prefix, where)))

  frames <- base::lapply(ortholog_chunks(symbols), function(chunk) {
    run(base::sprintf("%s IN (%s)", symbol_col, ortholog_quote_list(conn, chunk)))
  })
  entrez_ids <- entrez_ids[base::grepl("^[0-9]+$", entrez_ids)]
  frames <- base::c(frames, base::lapply(ortholog_chunks(entrez_ids), function(chunk) {
    run(base::sprintf("%s IN (%s)", entrez_col, base::paste(chunk, collapse = ", ")))
  }))

  if (base::length(frames) == 0) {
    return(base::data.frame(
      query_symbol = base::character(), query_entrez = base::character(), symbol = base::character(),
      ensembl = base::character(), entrez = base::character(), support_n = base::numeric(),
      stringsAsFactors = FALSE
    ))
  }
  out <- base::do.call(base::rbind, frames)
  out <- out[!base::duplicated(out), , drop = FALSE]
  for (column in base::c("query_symbol", "query_entrez", "symbol", "ensembl", "entrez")) {
    out[[column]] <- ortholog_id_text(out[[column]])
  }
  out$support_n <- base::suppressWarnings(base::as.numeric(out$support_n))
  base::rownames(out) <- NULL
  out
}

# Translate ids from one organism to the orthologous genes of the other.
# Returns list(rows, total, columns). Caller mistakes are plain errors whose
# messages the route maps to 400; see ORTHOLOG_CALLER_ERRORS.
lookup_orthologs <- function(conn, ids, from, to = "Homo sapiens", id_type = "auto", max_ids = 2000) {
  id_type <- base::match.arg(id_type, base::c("auto", "uniprot", "ensembl", "entrez", "symbol"))

  ids <- base::trimws(base::as.character(ids))
  ids <- base::unique(ids[!base::is.na(ids) & base::nzchar(ids)])
  if (base::length(ids) == 0) {
    base::stop("No ids were given.")
  }
  if (base::length(ids) > max_ids) {
    base::stop(base::sprintf("At most %d ids per request.", max_ids))
  }

  from <- base::trimws(base::as.character(from)[1])
  to <- base::trimws(base::as.character(to)[1])
  if (!base::nzchar(from) || base::is.na(from)) base::stop("'from' must name the organism the ids belong to.")
  if (!base::nzchar(to) || base::is.na(to)) base::stop("'to' must name the organism to map to.")
  from_key <- base::tolower(from)
  to_key <- base::tolower(to)
  if (from_key == to_key) {
    base::stop("'from' and 'to' must be different organisms.")
  }
  if (!"homo sapiens" %in% base::c(from_key, to_key)) {
    base::stop("One of 'from' or 'to' must be 'Homo sapiens'.")
  }
  from_is_human <- from_key == "homo sapiens"

  organisms <- base::suppressWarnings(DBI::dbGetQuery(conn, base::sprintf(
    "SELECT organism_id, organism FROM organisms WHERE LOWER(organism) IN (%s)",
    ortholog_quote_list(conn, base::c(from_key, to_key))
  )))
  organism_key <- base::tolower(base::trimws(base::as.character(organisms$organism)))
  from_row <- organisms[organism_key == from_key, , drop = FALSE]
  to_row <- organisms[organism_key == to_key, , drop = FALSE]
  if (base::nrow(from_row) == 0) base::stop(base::sprintf("Unknown organism '%s'.", from))
  if (base::nrow(to_row) == 0) base::stop(base::sprintf("Unknown organism '%s'.", to))
  non_human_row <- if (from_is_human) to_row else from_row
  non_human_id <- non_human_row$organism_id[1]

  n_current <- base::suppressWarnings(DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) AS n FROM gene_orthologs WHERE is_current = 1 AND organism_id = %s",
    base::as.character(non_human_id)
  )))$n[1]
  if (base::as.integer(n_current) == 0) {
    base::stop(base::sprintf("No ortholog reference is loaded for '%s'.", non_human_row$organism[1]))
  }

  types <- if (id_type == "auto") classify_feature_id_type(ids) else base::rep(id_type, base::length(ids))
  symbols_tbl <- ortholog_query_symbols(conn, ids, types, from_row$organism_id[1])
  query_symbol <- base::as.character(symbols_tbl$query_symbol[base::match(ids, symbols_tbl$query)])
  entrez_ids <- ids[!base::is.na(types) & types == "entrez"]

  pairs <- ortholog_pairs(
    conn,
    symbols = base::unique(query_symbol[!base::is.na(query_symbol)]),
    entrez_ids = entrez_ids,
    non_human_organism_id = non_human_id,
    from_is_human = from_is_human
  )
  pair_symbol_key <- base::tolower(base::trimws(base::as.character(pairs$query_symbol)))
  pair_entrez_key <- base::trimws(base::as.character(pairs$query_entrez))

  target_organism <- base::as.character(to_row$organism[1])
  rows <- base::vector("list", base::length(ids))
  for (i in base::seq_along(ids)) {
    type_i <- types[i]
    symbol_i <- query_symbol[i]
    if (!base::is.na(type_i) && type_i == "entrez") {
      hits <- pairs[!base::is.na(pair_entrez_key) & pair_entrez_key == ids[i], , drop = FALSE]
      if (base::nrow(hits) > 0 && base::is.na(symbol_i)) symbol_i <- base::as.character(hits$query_symbol[1])
    } else if (!base::is.na(symbol_i)) {
      hits <- pairs[pair_symbol_key == base::tolower(base::trimws(symbol_i)), , drop = FALSE]
    } else {
      hits <- pairs[0, , drop = FALSE]
    }

    if (base::nrow(hits) == 0) {
      rows[[i]] <- base::data.frame(
        query = ids[i], query_type = type_i, query_symbol = symbol_i, organism = target_organism,
        symbol = NA_character_, ensembl = NA_character_, entrez = NA_character_,
        support_n = NA_real_, ambiguous = FALSE, stringsAsFactors = FALSE
      )
    } else {
      ord <- base::order(-hits$support_n, hits$symbol, na.last = TRUE)
      hits <- hits[ord, , drop = FALSE]
      rows[[i]] <- base::data.frame(
        query = ids[i], query_type = type_i, query_symbol = symbol_i, organism = target_organism,
        symbol = hits$symbol, ensembl = hits$ensembl, entrez = hits$entrez,
        support_n = hits$support_n, ambiguous = base::nrow(hits) > 1, stringsAsFactors = FALSE
      )
    }
  }
  out <- base::do.call(base::rbind, rows)
  base::rownames(out) <- NULL

  base::list(rows = out, total = base::nrow(out), columns = ORTHOLOG_RESULT_COLUMNS)
}

# The messages lookup_orthologs() raises for a caller's mistake, so the route
# can answer 400 for these and 500 for anything else.
ORTHOLOG_CALLER_ERRORS <- base::c(
  "No ids were given", "At most", "must name the organism", "must be different organisms",
  "must be 'Homo sapiens'", "Unknown organism", "No ortholog reference is loaded"
)

is_ortholog_caller_error <- function(message) {
  base::any(base::vapply(ORTHOLOG_CALLER_ERRORS, function(p) base::grepl(p, message, fixed = TRUE), base::logical(1)))
}
