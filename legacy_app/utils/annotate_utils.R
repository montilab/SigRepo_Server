# Helpers for the Shiny Annotate tab (modules/annotate_module.R).
#
# The tab is a front end to SigRepo::runHypeR(), which builds hypeR query
# vectors from SigRepo signatures, runs hypergeometric, KS or fgsea enrichment,
# and returns hypeR's own hyp/multihyp. Nothing here enriches anything: these
# helpers turn the tab's inputs into runHypeR()'s arguments, load the genesets
# it runs against, and read the result back for display and for the R code that
# reproduces it. Keeping them free of Shiny lets them be tested on their own
# (tests/testthat/test-shiny-annotate-helpers.R).
#
# Shared with the Compare tab (utils/compare_utils.R): the signature picker
# filters, the OmicSignature upload reader, the background text parser and the
# warning-capturing runner. Shared with the API (api/lib/msigdb_cache.R): the
# on-disk MSigDB cache.

# ---- defaults ---------------------------------------------------------------

# runHypeR()'s defaults for every setting the tab exposes. The inputs start at
# these values and their labels print them; a test holds them to the function's
# formals so the two cannot drift apart. direction is "up" except for fgsea,
# where runHypeR() defaults a direction that was not passed to "both".
ANNOTATE_DEFAULTS <- base::list(
  test = "hypergeometric",
  split = TRUE,
  direction = "up",
  ks_source = "difexp",
  score_col = "score",
  min_query_genes = 4,
  fdr_scope = "run",
  seed = 1,
  background = NULL,
  power = 1,
  absolute = FALSE,
  pval = 1,
  fdr = 1,
  msigdb_clean = FALSE
)

# The fgsea::fgseaMultilevel() arguments runHypeR() merges fgsea_args over.
ANNOTATE_FGSEA_DEFAULTS <- base::list(sampleSize = 101, minSize = 1, maxSize = Inf)

# The most signatures (or gene lists) one run takes.
ANNOTATE_MAX_SIGNATURES <- 10

ANNOTATE_TEST_CHOICES <- c(
  "Hypergeometric" = "hypergeometric",
  "KS test" = "kstest",
  "GSEA (fgsea)" = "fgsea"
)

ANNOTATE_TEST_LABELS <- c(hypergeometric = "Hypergeometric", kstest = "KS test", fgsea = "GSEA (fgsea)")

# The SigRepo keys runHypeR() appends to every hyp's info, in order.
ANNOTATE_PROVENANCE_KEYS <- c(
  "SigRepo Signature ID", "SigRepo Signature Name", "Group Label", "Symbol Source",
  "SigRepo Direction", "SigRepo Ranked Table", "SigRepo Score Column", "SigRepo Split",
  "SigRepo Background Source", "SigRepo Features Unmapped", "SigRepo Query Genes Removed",
  "SigRepo Genesets Dropped", "SigRepo Genesets Dropped List", "SigRepo FDR Scope"
)

# A default as the tab's labels print it: "default (0.05)".
annotate_default_text <- function(name, test = NULL) {
  value <- if (name %in% base::names(ANNOTATE_FGSEA_DEFAULTS)) ANNOTATE_FGSEA_DEFAULTS[[name]] else ANNOTATE_DEFAULTS[[name]]
  if (base::identical(name, "direction") && base::identical(test, "fgsea")) {
    value <- "both"
  }
  shown <- if (base::is.null(value)) {
    "auto"
  } else if (base::is.logical(value)) {
    if (base::isTRUE(value)) "on" else "off"
  } else {
    base::format(value)
  }
  base::sprintf("default (%s)", shown)
}

# The direction runHypeR() uses when none is passed.
annotate_default_direction <- function(test) {
  if (base::identical(test, "fgsea")) "both" else ANNOTATE_DEFAULTS$direction
}

# ---- gene lists (hypeR-native `signature` input) ------------------------------

# Gene lists typed into the tab. Blocks start with a "# name" or ">name" line;
# genes before any header form one list named "genes". In a block, genes are
# separated by commas, semicolons, spaces or new lines -- unless every line is
# "GENE score", which makes the block a ranked list (a named numeric vector, in
# the order given, as hypeR takes it). Blocks that mix the two, repeat a name,
# repeat a scored gene or hold no genes are errors.
annotate_parse_gene_lists <- function(text) {
  if (base::is.null(text) || base::length(text) == 0) {
    return(base::list())
  }
  lines <- base::trimws(base::unlist(base::strsplit(base::paste(text, collapse = "\n"), "\r?\n")))

  blocks <- base::list()
  current <- NULL
  for (line in lines) {
    if (!base::nzchar(line)) {
      next
    }
    if (base::grepl("^(#|>)", line)) {
      name <- base::trimws(base::sub("^(#|>)", "", line))
      if (!base::nzchar(name)) {
        base::stop("A gene list header ('# name') needs a name.", call. = FALSE)
      }
      if (name %in% base::names(blocks)) {
        base::stop(base::sprintf("Gene list '%s' appears more than once.", name), call. = FALSE)
      }
      blocks[[name]] <- base::character()
      current <- name
      next
    }
    if (base::is.null(current)) {
      current <- "genes"
      blocks[[current]] <- base::character()
    }
    blocks[[current]] <- c(blocks[[current]], line)
  }

  base::lapply(stats::setNames(base::names(blocks), base::names(blocks)), function(name) {
    annotate_parse_gene_block(name, blocks[[name]])
  })
}

annotate_parse_gene_block <- function(name, lines) {
  if (base::length(lines) == 0) {
    base::stop(base::sprintf("Gene list '%s' has no genes.", name), call. = FALSE)
  }
  tokens <- base::strsplit(lines, "[,;[:space:]]+")
  tokens <- base::lapply(tokens, function(x) x[base::nzchar(x)])
  scored <- base::vapply(tokens, function(x) {
    base::length(x) == 2 && !base::is.na(base::suppressWarnings(base::as.numeric(x[2])))
  }, base::logical(1))

  if (base::all(scored)) {
    genes <- base::vapply(tokens, `[`, base::character(1), 1)
    if (base::anyDuplicated(genes)) {
      base::stop(base::sprintf(
        "Gene list '%s' scores %s more than once; a ranked list needs one score per gene.",
        name, base::paste(base::unique(genes[base::duplicated(genes)]), collapse = ", ")
      ), call. = FALSE)
    }
    return(stats::setNames(base::as.numeric(base::vapply(tokens, `[`, base::character(1), 2)), genes))
  }
  if (base::any(scored)) {
    base::stop(base::sprintf(
      "Gene list '%s' mixes 'GENE score' lines with plain genes; score every gene or none.", name
    ), call. = FALSE)
  }
  base::unique(base::unlist(tokens))
}

# One uploaded gene list file. GMT gives one list per line; a CSV/TSV with a
# gene column (gene_symbol, symbol, gene) gives lists by its list column (list,
# signature, name, set) or one list named after the file, ranked when it has a
# score column; anything else is read as typed text.
annotate_read_gene_list_file <- function(path, file_name) {
  lower <- base::tolower(file_name)
  if (base::grepl("\\.gmt$", lower)) {
    return(annotate_parse_custom_geneset_gmt(path))
  }

  table <- if (base::grepl("\\.(csv|tsv)$", lower)) {
    base::tryCatch(
      utils::read.table(path, header = TRUE, sep = if (base::grepl("\\.tsv$", lower)) "\t" else ",",
                        stringsAsFactors = FALSE, check.names = FALSE, quote = "\"", comment.char = ""),
      error = function(e) NULL
    )
  }
  gene_col <- if (!base::is.null(table)) annotate_find_column(table, c("gene_symbol", "symbol", "gene", "feature_name"))
  if (base::is.null(gene_col)) {
    return(annotate_parse_gene_lists(base::readLines(path, warn = FALSE)))
  }

  score_col <- annotate_find_column(table, c("score", "stat", "logfc", "weight"))
  list_col <- annotate_find_column(table, c("list", "signature", "name", "set", "geneset"))
  genes <- base::trimws(base::as.character(table[[gene_col]]))
  keep <- !base::is.na(genes) & base::nzchar(genes)
  groups <- if (base::is.null(list_col)) {
    base::rep(base::sub("\\.[^.]*$", "", base::basename(file_name)), base::length(genes))
  } else {
    base::trimws(base::as.character(table[[list_col]]))
  }
  keep <- keep & !base::is.na(groups) & base::nzchar(groups)

  out <- base::lapply(base::split(base::which(keep), base::factor(groups[keep], levels = base::unique(groups[keep]))), function(rows) {
    if (base::is.null(score_col)) {
      return(base::unique(genes[rows]))
    }
    scores <- base::suppressWarnings(base::as.numeric(table[[score_col]][rows]))
    ok <- !base::is.na(scores)
    stats::setNames(scores[ok], genes[rows][ok])
  })
  if (base::length(out) == 0) {
    base::stop(base::sprintf("'%s' has no gene rows.", file_name), call. = FALSE)
  }
  out
}

annotate_find_column <- function(table, candidates) {
  normalized <- base::tolower(base::gsub("[^A-Za-z0-9]+", "", base::names(table)))
  idx <- base::match(base::gsub("[^a-z0-9]+", "", candidates), normalized, nomatch = 0)
  idx <- idx[idx > 0]
  if (base::length(idx) == 0) NULL else base::names(table)[idx[1]]
}

# What is wrong with gene lists for a test, as messages for the readiness box.
# Hypergeometric tests sets of symbols; fgsea ranks by score; the KS test takes
# either (a plain list is ranked in the order given).
annotate_check_gene_lists <- function(lists, test) {
  if (base::length(lists) == 0) {
    return("Enter at least one gene list.")
  }
  problems <- base::character()
  if (base::length(lists) > ANNOTATE_MAX_SIGNATURES) {
    problems <- c(problems, base::sprintf("A run takes at most %d gene lists; there are %d.", ANNOTATE_MAX_SIGNATURES, base::length(lists)))
  }
  ranked <- base::vapply(lists, base::is.numeric, base::logical(1))
  if (base::identical(test, "hypergeometric") && base::any(ranked)) {
    problems <- c(problems, base::sprintf(
      "The hypergeometric test needs plain gene lists, but %s has scores.",
      base::paste(base::sprintf("'%s'", base::names(lists)[ranked]), collapse = ", ")
    ))
  }
  if (base::identical(test, "fgsea") && !base::all(ranked)) {
    problems <- c(problems, base::sprintf(
      "GSEA (fgsea) ranks by score, so every list needs 'GENE score' lines; %s has none.",
      base::paste(base::sprintf("'%s'", base::names(lists)[!ranked]), collapse = ", ")
    ))
  }
  problems
}

annotate_gene_list_preview <- function(lists) {
  if (base::length(lists) == 0) {
    return(base::data.frame(name = base::character(), kind = base::character(), n_genes = base::integer(), stringsAsFactors = FALSE))
  }
  base::data.frame(
    name = base::names(lists),
    kind = base::ifelse(base::vapply(lists, base::is.numeric, base::logical(1)), "ranked", "set"),
    n_genes = base::vapply(lists, base::length, base::integer(1)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

# ---- background ---------------------------------------------------------------

# runHypeR()'s background from the tab's background mode:
#   default        NULL, runHypeR()'s guarded choice
#   number         a population size
#   difexp         each signature's measured genes
#   genes          a typed gene universe
#   per_signature  a named list, one entry per signature, from `per_signature`
#                  (data frame: key = signature id or list name, mode = "number"
#                  or "difexp", value)
annotate_build_background <- function(mode = "default", number = NULL, genes_text = NULL, per_signature = NULL) {
  as_size <- function(value, what) {
    size <- base::suppressWarnings(base::as.numeric(value))
    if (base::length(size) != 1 || base::is.na(size) || !base::is.finite(size) || size <= 0) {
      base::stop(base::sprintf("%s must be a positive number.", what), call. = FALSE)
    }
    size
  }
  switch(
    mode %||% "default",
    default = NULL,
    number = as_size(number, "The background size"),
    difexp = "difexp",
    genes = {
      genes <- compare_parse_background(genes_text)
      if (base::length(genes) < 2) {
        base::stop("A background gene universe needs at least two genes.", call. = FALSE)
      }
      genes
    },
    per_signature = {
      if (base::is.null(per_signature) || base::nrow(per_signature) == 0) {
        base::stop("Add signatures before setting a background for each one.", call. = FALSE)
      }
      stats::setNames(base::lapply(base::seq_len(base::nrow(per_signature)), function(i) {
        if (base::identical(per_signature$mode[i], "difexp")) {
          "difexp"
        } else {
          as_size(per_signature$value[i], base::sprintf("The background size for '%s'", per_signature$key[i]))
        }
      }), base::as.character(per_signature$key))
    },
    base::stop(base::sprintf("Unknown background mode '%s'.", mode), call. = FALSE)
  )
}

# ---- genesets -------------------------------------------------------------------

# MSigDB collections the picker offers. Human collections are offered for every
# species (msigdbr maps them to orthologs); the mouse-native ones only for
# Mus musculus, where SigRepo::getHypeRGenesets() reads the mouse database.
ANNOTATE_MSIGDB_COLLECTIONS <- base::data.frame(
  collection = c(
    "H", "C1", base::rep("C2", 8), base::rep("C3", 4), base::rep("C4", 3), base::rep("C5", 4), "C6", base::rep("C7", 2), "C8",
    "MH", "M1", base::rep("M2", 4), base::rep("M3", 2), base::rep("M5", 4), "M7", "M8"
  ),
  label = c(
    "Hallmark (H)", "Positional (C1)", base::rep("Curated (C2)", 8), base::rep("Regulatory target (C3)", 4),
    base::rep("Computational (C4)", 3), base::rep("Ontology (C5)", 4), "Oncogenic signature (C6)",
    base::rep("Immunologic signature (C7)", 2), "Cell type signature (C8)",
    "Mouse hallmark (MH)", "Mouse positional (M1)", base::rep("Mouse curated (M2)", 4),
    base::rep("Mouse regulatory target (M3)", 2), base::rep("Mouse ontology (M5)", 4),
    "Mouse immunologic signature (M7)", "Mouse cell type signature (M8)"
  ),
  subcollection = c(
    "", "", "CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY", "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME", "CP:WIKIPATHWAYS",
    "MIR:MIRDB", "MIR:MIR_LEGACY", "TFT:GTRD", "TFT:TFT_LEGACY", "3CA", "CGN", "CM", "GO:BP", "GO:CC", "GO:MF", "HPO",
    "", "IMMUNESIGDB", "VAX", "",
    "", "", "CGP", "CP:BIOCARTA", "CP:REACTOME", "CP:WIKIPATHWAYS", "GTRD", "MIRDB", "GO:BP", "GO:CC", "GO:MF", "MPT", "", ""
  ),
  mouse_only = c(base::rep(FALSE, 25), base::rep(TRUE, 14)),
  stringsAsFactors = FALSE
)

# Collection dropdown choices for a species, label -> collection.
annotate_msigdb_collections <- function(species) {
  rows <- ANNOTATE_MSIGDB_COLLECTIONS[!ANNOTATE_MSIGDB_COLLECTIONS$mouse_only | base::identical(species, "Mus musculus"), , drop = FALSE]
  rows <- rows[!base::duplicated(rows$collection), , drop = FALSE]
  stats::setNames(rows$collection, rows$label)
}

# Subcollections of a collection; "" alone when it has none.
annotate_msigdb_subcollections <- function(collection) {
  subs <- ANNOTATE_MSIGDB_COLLECTIONS$subcollection[ANNOTATE_MSIGDB_COLLECTIONS$collection == collection]
  if (base::length(subs) == 0) "" else subs
}

# A hypeR::gsets from a named list of symbol vectors or an msigdbr-style table
# (gs_name, gene_symbol) -- the on-disk cache holds both forms. gsets and rgsets
# pass through.
annotate_as_gsets <- function(x, name, version, clean = FALSE) {
  if (methods::is(x, "gsets") || methods::is(x, "rgsets")) {
    return(x)
  }
  if (base::is.data.frame(x)) {
    if (!base::all(c("gs_name", "gene_symbol") %in% base::names(x))) {
      base::stop("A geneset table needs gs_name and gene_symbol columns.", call. = FALSE)
    }
    x <- base::lapply(base::split(base::as.character(x$gene_symbol), base::as.character(x$gs_name)), base::unique)
  }
  if (!base::is.list(x) || base::length(x) == 0 || base::is.null(base::names(x))) {
    base::stop("Genesets must be a non-empty named list of gene symbols.", call. = FALSE)
  }
  hypeR::gsets$new(genesets = x, name = name, version = version, clean = clean, quiet = TRUE)
}

# One MSigDB collection as gsets: from the on-disk cache when it is there,
# otherwise from msigdbr through SigRepo::getHypeRGenesets() when this server
# allows live fetches. The description names where it came from, for the tab
# and for the R code.
annotate_load_msigdb <- function(species, collection, subcollection = "", clean = FALSE, cache_dir,
                                 allow_fetch = runtime_msigdb_fetch_allowed(),
                                 fetcher = SigRepo::getHypeRGenesets) {
  subcollection <- subcollection %||% ""
  name <- if (base::nzchar(subcollection)) base::paste0(collection, ".", subcollection) else collection
  cache_file <- msigdb_cache_file(cache_dir, species, collection, subcollection)
  cached <- load_cached_msigdb_genesets(cache_dir, species, collection, subcollection)

  if (!base::is.null(cached)) {
    version <- if (base::is.data.frame(cached) && "db_version" %in% base::names(cached)) {
      base::as.character(base::unique(cached$db_version)[1])
    } else {
      base::format(base::as.Date(base::file.info(cache_file)$mtime))
    }
    genesets <- annotate_as_gsets(cached, name = name, version = version, clean = clean)
    origin <- "cache"
  } else if (base::isTRUE(allow_fetch)) {
    genesets <- fetcher(
      "msigdb",
      msigdb_species = species,
      msigdb_collection = collection,
      msigdb_subcollection = if (base::nzchar(subcollection)) subcollection else NULL,
      msigdb_clean = clean
    )
    origin <- "msigdbr"
  } else {
    base::stop(base::sprintf(
      "%s %s is not in this server's MSigDB cache (%s), and live MSigDB fetches are switched off (MSIGDB_ALLOW_RUNTIME_FETCH).",
      species, name, cache_file
    ), call. = FALSE)
  }

  base::list(
    genesets = genesets,
    description = base::list(
      source = "msigdb", species = species, collection = collection, subcollection = subcollection,
      clean = base::isTRUE(clean), origin = origin, name = genesets$name, version = genesets$version,
      n = base::length(genesets$genesets)
    )
  )
}

# hyperdb rgsets (genesets with a hierarchy, which hierarchy maps need). The
# repository lists files as <source>/<source>_v<version>.rds; `available` is
# hypeR::hyperdb_available(), or NULL for the ones hypeR documents.
annotate_rgsets_choices <- function(available = NULL) {
  fallback <- base::data.frame(source = c("KEGG", "REACTOME"), version = c("92.0", "70.0"), stringsAsFactors = FALSE)
  out <- if (base::is.null(available) || base::nrow(available) == 0) {
    fallback
  } else {
    source <- base::as.character(available$source)
    file <- base::as.character(available$gsets)
    match <- base::startsWith(file, base::paste0(source, "_v")) & base::grepl("\\.rds$", file)
    base::data.frame(
      source = source[match],
      version = base::sub("\\.rds$", "", base::substring(file[match], base::nchar(source[match]) + 3)),
      stringsAsFactors = FALSE
    )
  }
  if (base::nrow(out) == 0) {
    out <- fallback
  }
  out$label <- base::sprintf("%s (v%s)", out$source, out$version)
  out
}

annotate_load_rgsets <- function(source, version, loader = hypeR::hyperdb_rgsets) {
  genesets <- loader(source, version)
  if (!methods::is(genesets, "rgsets")) {
    base::stop(base::sprintf("hyperdb's %s v%s is not a hierarchy (rgsets), so it cannot be used here.", source, version), call. = FALSE)
  }
  base::list(
    genesets = genesets,
    description = base::list(
      source = "rgsets", collection = source, clean = FALSE, origin = "hyperdb",
      name = source, version = version, n = base::length(genesets$genesets)
    )
  )
}

annotate_load_custom <- function(genesets, clean = FALSE) {
  gsets <- annotate_as_gsets(genesets, name = "Custom", version = base::format(base::Sys.Date()), clean = clean)
  base::list(
    genesets = gsets,
    description = base::list(
      source = "custom", clean = base::isTRUE(clean), origin = "upload",
      name = gsets$name, version = gsets$version, n = base::length(gsets$genesets)
    )
  )
}

annotate_genesets_label <- function(description) {
  if (base::is.null(description)) {
    return("No genesets loaded")
  }
  where <- switch(
    description$source,
    msigdb = base::sprintf("%s, MSigDB %s %s", description$species, if (description$origin == "cache") "cache" else "live", description$version),
    rgsets = base::sprintf("hyperdb v%s", description$version),
    custom = "custom upload"
  )
  base::sprintf("%s: %s genesets (%s)", description$name, base::format(description$n, big.mark = ","), where)
}

# Custom genesets typed in (one set) or uploaded (GMT, or CSV with a geneset
# name column and a gene column).
annotate_parse_custom_geneset_text <- function(geneset_name, genes_text) {
  geneset_name <- base::trimws(base::as.character(geneset_name %||% ""))
  if (!base::nzchar(geneset_name)) {
    base::stop("Give the custom geneset a name.", call. = FALSE)
  }
  genes <- base::unique(base::trimws(base::unlist(base::strsplit(base::paste(genes_text, collapse = "\n"), "[,;[:space:]]+"))))
  genes <- genes[base::nzchar(genes)]
  if (base::length(genes) == 0) {
    base::stop("Enter at least one gene symbol for the custom geneset.", call. = FALSE)
  }
  stats::setNames(base::list(genes), geneset_name)
}

annotate_parse_custom_geneset_gmt <- function(path) {
  lines <- base::readLines(path, warn = FALSE)
  lines <- lines[base::nzchar(base::trimws(lines))]
  sets <- base::lapply(base::strsplit(lines, "\t", fixed = TRUE), function(parts) {
    genes <- base::unique(base::trimws(parts[-c(1, 2)]))
    genes <- genes[base::nzchar(genes)]
    if (base::length(parts) < 3 || !base::nzchar(base::trimws(parts[1])) || base::length(genes) == 0) NULL else genes
  })
  names <- base::trimws(base::vapply(base::strsplit(lines, "\t", fixed = TRUE), `[`, base::character(1), 1))
  keep <- !base::vapply(sets, base::is.null, base::logical(1))
  if (!base::any(keep)) {
    base::stop("The GMT file has no valid genesets (name, description, then genes, tab separated).", call. = FALSE)
  }
  stats::setNames(sets[keep], names[keep])
}

annotate_parse_custom_geneset_file <- function(path, file_name) {
  if (base::grepl("\\.gmt$", base::tolower(file_name))) {
    return(annotate_parse_custom_geneset_gmt(path))
  }
  table <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  set_col <- annotate_find_column(table, c("geneset_name", "geneset", "set_name", "set"))
  gene_col <- annotate_find_column(table, c("gene_symbol", "symbol", "gene", "feature_name"))
  if (base::is.null(set_col) || base::is.null(gene_col)) {
    base::stop("A custom geneset CSV needs a geneset_name column and a gene_symbol column.", call. = FALSE)
  }
  sets <- base::trimws(base::as.character(table[[set_col]]))
  genes <- base::trimws(base::as.character(table[[gene_col]]))
  keep <- !base::is.na(sets) & base::nzchar(sets) & !base::is.na(genes) & base::nzchar(genes)
  if (!base::any(keep)) {
    base::stop("The custom geneset CSV has no rows with both a geneset name and a gene.", call. = FALSE)
  }
  base::lapply(base::split(genes[keep], sets[keep]), base::unique)
}
