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

# runHypeR()'s defaults for every optional setting the tab exposes. The inputs
# start at these values and their labels print them; a test holds them to the
# function's formals so the two cannot drift apart. `test` has no default in
# runHypeR() (it is required, like `organism`); the tab starts at
# hypergeometric. The ranked tests always test both ends of the ranking, so
# there is no direction setting.
ANNOTATE_DEFAULTS <- base::list(
  test = "hypergeometric",
  split = TRUE,
  ks_source = "difexp",
  score_col = "score",
  min_query_genes = 4,
  fdr_scope = "run",
  seed = 1,
  background = NULL,
  power = 1,
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

# The SigRepo keys runHypeR() appends to every hyp's info, in order. A test
# holds them to the client's own list.
ANNOTATE_PROVENANCE_KEYS <- c(
  "SigRepo Signature ID", "SigRepo Signature Name", "Group Label", "Symbol Source",
  "SigRepo Direction", "SigRepo Ranked Table", "SigRepo Score Column", "SigRepo Split",
  "SigRepo Background Source", "SigRepo Features Unmapped", "SigRepo Query Genes Removed",
  "SigRepo Genesets Dropped", "SigRepo Genesets Dropped List", "SigRepo FDR Scope"
)

# A default as the tab's labels print it: "default (0.05)".
annotate_default_text <- function(name) {
  value <- if (name %in% base::names(ANNOTATE_FGSEA_DEFAULTS)) ANNOTATE_FGSEA_DEFAULTS[[name]] else ANNOTATE_DEFAULTS[[name]]
  shown <- if (base::is.null(value)) {
    "auto"
  } else if (base::is.logical(value)) {
    if (base::isTRUE(value)) "on" else "off"
  } else {
    base::format(value)
  }
  base::sprintf("default (%s)", shown)
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
# (gs_name, gene_symbol) -- the on-disk cache holds both forms. A gsets object
# passes through.
annotate_as_gsets <- function(x, name, version, clean = FALSE) {
  if (methods::is(x, "gsets")) {
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

# ---- arguments ------------------------------------------------------------------

# The argument list for SigRepo::runHypeR(). `source` is where the signatures
# come from, and only that source's argument is sent, since runHypeR() takes
# one: repository picks as signature_id, uploads as a named omic_signature
# list, gene lists as hypeR-native `signature`. The connection goes with the
# SigRepo sources (uploads use it to look up gene symbols). `organism` and
# `test` are required by runHypeR() and always sent. Test settings are sent
# only for the test that uses them, and the ranking settings hypeR-native input
# cannot take are left out.
annotate_build_args <- function(source, conn_handler = NULL, signature_ids = NULL, omic_signatures = NULL,
                                gene_lists = NULL, genesets, settings) {
  test <- settings$test %||% ANNOTATE_DEFAULTS$test
  native <- base::identical(source, "genes")

  args <- switch(
    source,
    repository = base::list(conn_handler = conn_handler, signature_id = signature_ids),
    upload = base::list(conn_handler = conn_handler, omic_signature = omic_signatures),
    genes = base::list(signature = gene_lists),
    base::stop(base::sprintf("Unknown signature source '%s'.", source), call. = FALSE)
  )
  if (!native && base::is.null(conn_handler)) {
    args$conn_handler <- NULL
  }
  args$organism <- settings$organism
  args$test <- test
  args$genesets <- genesets

  setting <- function(name) settings[[name]] %||% ANNOTATE_DEFAULTS[[name]]
  if (base::identical(test, "hypergeometric")) {
    if (!native) args$split <- setting("split")
    args$min_query_genes <- setting("min_query_genes")
  } else {
    if (!native) {
      args$ks_source <- setting("ks_source")
      args$score_col <- setting("score_col")
    }
    args$power <- setting("power")
    if (base::identical(test, "fgsea")) {
      args$seed <- setting("seed")
      fgsea_args <- settings$fgsea_args %||% base::list()
      # Browser inputs arrive as integers (101L), so compare values, not types.
      fgsea_args <- base::lapply(fgsea_args, base::as.numeric)
      changed <- base::vapply(base::names(fgsea_args), function(nm) {
        !base::isTRUE(base::all.equal(fgsea_args[[nm]], ANNOTATE_FGSEA_DEFAULTS[[nm]]))
      }, base::logical(1))
      args$fgsea_args <- fgsea_args[changed]
    }
  }
  args$fdr_scope <- setting("fdr_scope")
  if (!base::is.null(settings$background)) {
    args$background <- settings$background
  }
  args$pval <- setting("pval")
  args$fdr <- setting("fdr")
  args$verbose <- FALSE
  # Numbers typed into the tab arrive as integers; send and print them as
  # the doubles runHypeR()'s defaults are.
  for (nm in base::intersect(c("min_query_genes", "power", "seed", "pval", "fdr"), base::names(args))) {
    if (base::is.integer(args[[nm]])) args[[nm]] <- base::as.numeric(args[[nm]])
  }
  args
}

# The same inputs for SigRepo::prepareHypeRSignatures(), which builds the
# queries runHypeR() would test without testing them.
annotate_prepare_args <- function(args) {
  args[base::intersect(base::names(args), base::names(base::formals(SigRepo::prepareHypeRSignatures)))]
}

# The readiness problem for signatures that are not of the run's organism, or
# character() when they all are. runHypeR() compares organisms ignoring case and
# surrounding spaces and stops on a mismatch, so the tab says so before a run.
annotate_organism_problem <- function(names, organisms, organism) {
  if (base::is.null(organism) || !base::nzchar(base::trimws(organism)) || base::length(names) == 0) {
    return(base::character())
  }
  found <- base::trimws(base::as.character(organisms))
  wrong <- base::is.na(found) | base::tolower(found) != base::tolower(base::trimws(organism))
  if (!base::any(wrong)) {
    return(base::character())
  }
  described <- base::sprintf("'%s' (%s)", names[wrong], base::ifelse(base::is.na(found[wrong]) | !base::nzchar(found[wrong]), "no organism recorded", found[wrong]))
  base::sprintf(
    "A run tests one organism: %s %s not %s. Change the organism in Step 2 or run %s separately.",
    base::paste(described, collapse = ", "), if (base::sum(wrong) == 1) "is" else "are", organism,
    if (base::sum(wrong) == 1) "it" else "them"
  )
}

# ---- running --------------------------------------------------------------------

# runHypeR() and prepareHypeRSignatures() through compare_run(), which keeps
# their warnings (backgrounds replaced, genes removed, genesets or signatures
# dropped) for the tab to show and returns errors instead of raising them.
annotate_run <- function(args, runner = SigRepo::runHypeR) {
  compare_run(args, runner = runner)
}

annotate_preview <- function(args, runner = SigRepo::prepareHypeRSignatures) {
  compare_run(annotate_prepare_args(args), runner = runner)
}

# ---- reading the result -----------------------------------------------------------

# The result's queries as a named list of hyps. runHypeR() returns a hyp for a
# single query by construction; it is named the way SigRepo's plot functions
# name it (signature | group | direction), so the names work as their `query`.
annotate_result_hyps <- function(result) {
  if (methods::is(result, "multihyp")) {
    return(result$data)
  }
  if (!methods::is(result, "hyp")) {
    base::stop("The result is not a hypeR hyp or multihyp.", call. = FALSE)
  }
  parts <- base::vapply(c("SigRepo Signature Name", "Group Label", "SigRepo Direction"), function(key) {
    annotate_info_value(result, key)
  }, base::character(1))
  parts <- parts[base::nzchar(parts)]
  stats::setNames(base::list(result), if (base::length(parts) == 0) "query" else base::paste(parts, collapse = " | "))
}

# One info value as a string, "" when absent.
annotate_info_value <- function(hyp, key) {
  value <- hyp$info[[key]]
  if (base::is.null(value) || base::length(value) == 0 || base::is.na(value[1])) "" else base::as.character(value[1])
}

annotate_result_test <- function(result) {
  annotate_info_value(annotate_result_hyps(result)[[1]], "Test")
}

annotate_is_ranked <- function(result) {
  annotate_result_test(result) %in% c("kstest", "fgsea")
}

annotate_result_genesets <- function(result) {
  annotate_result_hyps(result)[[1]]$args$genesets
}

# Every query's result table stacked, headed by which query and signature each
# row belongs to. Columns are hypeR's own for the test that ran.
annotate_results_table <- function(result) {
  hyps <- annotate_result_hyps(result)
  parts <- base::lapply(base::names(hyps), function(query) {
    hyp <- hyps[[query]]
    data <- hyp$data
    if (base::is.null(data) || base::nrow(data) == 0) {
      return(NULL)
    }
    base::rownames(data) <- NULL
    base::cbind(
      base::data.frame(
        query = query,
        signature_name = annotate_info_value(hyp, "SigRepo Signature Name"),
        group_label = annotate_info_value(hyp, "Group Label"),
        direction = annotate_info_value(hyp, "SigRepo Direction"),
        stringsAsFactors = FALSE
      ),
      data
    )
  })
  parts <- parts[!base::vapply(parts, base::is.null, base::logical(1))]
  if (base::length(parts) == 0) {
    return(base::data.frame(query = base::character(), label = base::character(), pval = base::numeric(),
                            fdr = base::numeric(), stringsAsFactors = FALSE))
  }
  out <- base::do.call(base::rbind, parts)
  base::rownames(out) <- NULL
  out
}

# A query's genesets, most significant first.
annotate_query_genesets <- function(result, query) {
  hyps <- annotate_result_hyps(result)
  data <- if (query %in% base::names(hyps)) hyps[[query]]$data else NULL
  if (base::is.null(data) || base::nrow(data) == 0) {
    return(base::character())
  }
  data$label[base::order(data$fdr, data$pval, data$label)]
}

# Headline numbers for the results card.
annotate_summary <- function(result, fdr = 0.05) {
  hyps <- annotate_result_hyps(result)
  table <- annotate_results_table(result)
  significant <- table$label[!base::is.na(table$fdr) & table$fdr <= fdr]
  genesets <- annotate_result_genesets(result)
  value_set <- function(key) {
    values <- base::unique(base::vapply(hyps, annotate_info_value, base::character(1), key = key, USE.NAMES = FALSE))
    values[base::nzchar(values)]
  }
  base::list(
    test = annotate_result_test(result),
    n_queries = base::length(hyps),
    genesets_name = base::sprintf("%s (%s)", genesets$name %||% "genesets", genesets$version %||% ""),
    n_genesets = base::length(genesets$genesets),
    n_rows = base::nrow(table),
    n_significant = base::length(base::unique(significant)),
    fdr = fdr,
    fdr_scope = value_set("SigRepo FDR Scope"),
    backgrounds = value_set("SigRepo Background Source")
  )
}

# Plot size in inches for plotHypeRDots(), from the rule in its documentation,
# plus a line of caption for each signature the key lists.
annotate_dot_size <- function(dots, n_queries) {
  if (base::is.null(dots) || base::nrow(dots) == 0) {
    return(base::list(width = 7, height = 3.2))
  }
  clamp <- function(x, lo, hi) base::min(base::max(x, lo), hi)
  label_chars <- base::max(base::nchar(base::as.character(dots$label_abrv)))
  query_chars <- base::max(base::nchar(base::as.character(dots$query_label)))
  n_signatures <- base::length(base::unique(dots$signature_code))
  base::list(
    width = clamp(5 + 0.085 * label_chars + 1.2 * n_queries, 7, 20),
    height = clamp(1.6 + 0.30 * base::length(base::unique(dots$label)) + 0.05 * query_chars +
                     if (n_signatures > 1) 0.2 * n_signatures else 0, 3.2, 22)
  )
}

# What one geneset scored for one query: a Measure/Value table and the genes
# behind it (leading edge for ranked tests, overlap for hypergeometric).
annotate_enrichment_summary <- function(result, query, geneset) {
  hyps <- annotate_result_hyps(result)
  hyp <- hyps[[query]]
  test <- annotate_info_value(hyp, "Test")
  format_number <- function(x) if (base::length(x) == 0 || base::is.na(x)) "" else base::format(base::signif(x, 3))
  split_genes <- function(x) {
    genes <- base::trimws(base::unlist(base::strsplit(base::as.character(x %||% ""), ",")))
    genes[base::nzchar(genes)]
  }

  if (base::identical(test, "hypergeometric")) {
    row <- hyp$data[hyp$data$label == geneset, , drop = FALSE]
    genes <- if (base::nrow(row) > 0) split_genes(row$hits[1]) else base::character()
    table <- base::data.frame(
      Measure = c("Query", "Geneset", "Query genes", "Geneset size", "Overlap", "Background", "p-value", "FDR"),
      Value = c(
        query, geneset, base::length(hyp$args$signature),
        if (base::nrow(row) > 0) row$geneset[1] else "",
        if (base::nrow(row) > 0) row$overlap[1] else "",
        if (base::nrow(row) > 0) row$background[1] else "",
        if (base::nrow(row) > 0) format_number(row$pval[1]) else "",
        if (base::nrow(row) > 0) format_number(row$fdr[1]) else ""
      ),
      stringsAsFactors = FALSE
    )
    return(base::list(table = table, genes = genes, genes_label = "Overlapping genes"))
  }

  e <- SigRepo::hypeREnrichmentData(result, geneset, query = query)$summary
  measures <- c(
    Query = query,
    Ranking = e$ranking,
    Geneset = geneset,
    Direction = e$direction %||% "",
    `Enrichment score (ES)` = format_number(e$es),
    NES = if (base::identical(test, "fgsea")) format_number(e$nes) else NA,
    `hypeR score` = if (base::identical(test, "kstest")) format_number(e$score) else NA,
    `ES position` = if (base::is.na(e$es_position)) "" else base::format(e$es_position),
    `Hits in ranking` = base::sprintf("%d of %d ranked genes", e$n_hits, e$n_ranked),
    `p-value` = format_number(e$pval),
    FDR = format_number(e$fdr)
  )
  measures <- measures[!base::is.na(measures)]
  base::list(
    table = base::data.frame(Measure = base::names(measures), Value = base::unname(measures), stringsAsFactors = FALSE),
    genes = e$leading_edge_genes,
    genes_label = "Leading edge genes"
  )
}

# ---- the equivalent R code ----------------------------------------------------------

# An R script that reproduces the run and the plots on screen. Only arguments
# that differ from runHypeR()'s defaults are written. The connection handler,
# uploaded signatures, long gene lists and custom genesets cannot be printed,
# so they are referred to by name with a comment saying where they come from
# (the tab offers the genesets and gene lists as downloads).
annotate_r_code <- function(args, genesets_description = NULL, plots = base::list()) {
  test <- args$test %||% ANNOTATE_DEFAULTS$test
  by_reference <- c("conn_handler", "omic_signature", "genesets")
  lines <- base::character()

  if ("conn_handler" %in% base::names(args)) {
    lines <- c(lines, "conn_handler <- SigRepo::newConnHandler(...)  # your SigRepo login")
  }
  if ("omic_signature" %in% base::names(args)) {
    lines <- c(lines, "omic_signature <- readRDS(\"<the OmicSignature file(s) uploaded to the app>.rds\")  # a named list")
  }
  if ("signature" %in% base::names(args)) {
    inline <- base::paste(base::deparse(args$signature, width.cutoff = 500L), collapse = "")
    if (base::nchar(inline) <= 2000) {
      lines <- c(lines, base::paste("signature <-", inline))
    } else {
      lines <- c(lines, "signature <- readRDS(\"annotate_gene_lists.rds\")  # the gene lists download")
    }
    by_reference <- c(by_reference, "signature")
  }

  d <- genesets_description
  genesets_line <- if (base::is.null(d)) {
    "genesets <- readRDS(\"annotate_genesets.rds\")  # the genesets download"
  } else if (base::identical(d$source, "msigdb")) {
    base::paste0(
      "genesets <- SigRepo::getHypeRGenesets(\"msigdb\", msigdb_species = ", base::deparse(d$species),
      ", msigdb_collection = ", base::deparse(d$collection),
      if (base::nzchar(d$subcollection %||% "")) base::paste0(", msigdb_subcollection = ", base::deparse(d$subcollection)) else "",
      if (base::isTRUE(d$clean)) ", msigdb_clean = TRUE" else "",
      ")",
      if (base::identical(d$origin, "cache")) base::sprintf("  # the app used its MSigDB cache, version %s", d$version) else ""
    )
  } else {
    "genesets <- readRDS(\"annotate_genesets.rds\")  # the genesets download"
  }
  lines <- c(lines, genesets_line, "")

  differs <- function(nm) {
    if (nm %in% by_reference || nm %in% c("signature_id", "organism", "test")) {
      return(TRUE)
    }
    if (nm == "verbose") {
      return(FALSE)
    }
    default <- if (nm == "fgsea_args") base::list() else ANNOTATE_DEFAULTS[[nm]]
    !base::isTRUE(base::all.equal(args[[nm]], default, check.attributes = FALSE))
  }
  written <- base::Filter(differs, base::names(args))
  call_lines <- base::vapply(written, function(nm) {
    value <- if (nm %in% by_reference) nm else base::paste(base::deparse(args[[nm]], width.cutoff = 500L), collapse = " ")
    base::sprintf("  %s = %s", nm, value)
  }, base::character(1))
  lines <- c(lines, base::paste0("res <- SigRepo::runHypeR(\n", base::paste(call_lines, collapse = ",\n"), "\n)"))

  plot_call <- function(fun, first, arguments) {
    arguments <- arguments[!base::vapply(arguments, base::is.null, base::logical(1))]
    rendered <- base::vapply(base::names(arguments), function(nm) {
      base::sprintf("%s = %s", nm, base::paste(base::deparse(arguments[[nm]], width.cutoff = 500L), collapse = " "))
    }, base::character(1))
    base::sprintf("SigRepo::%s(%s)", fun, base::paste(c(first, rendered), collapse = ", "))
  }
  plot_lines <- base::character()
  if (!base::is.null(plots$dots)) plot_lines <- c(plot_lines, plot_call("plotHypeRDots", "res", plots$dots))
  if (!base::is.null(plots$enrichment)) plot_lines <- c(plot_lines, plot_call("plotHypeREnrichment", "res", plots$enrichment))
  if (base::length(plot_lines) > 0) {
    lines <- c(lines, "", plot_lines)
  }
  lines <- c(lines, "SigRepo::hypeRToExcel(res, file_path = \"annotate_results.xlsx\")")
  base::paste(lines, collapse = "\n")
}

# ---- module support ---------------------------------------------------------------

# The MSigDB cache the API and MCP server read: MSIGDB_CACHE_DIR, or
# data/msigdb_genesets in the SigRepo_Server checkout the app runs from.
annotate_msigdb_cache_dir <- function(shiny_path = base::getOption("sigrepo.shiny_path", "")) {
  server_root <- base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "")
  if (!base::nzchar(server_root)) {
    server_root <- base::normalizePath(base::file.path(shiny_path, ".."), mustWork = FALSE)
  }
  default_msigdb_cache_dir(server_root)
}

# The arguments of a plot call that differ from the plot function's defaults,
# for the R code. `defaults` is a named list of the defaults to compare with.
annotate_changed_args <- function(values, defaults) {
  keep <- base::vapply(base::names(values), function(nm) {
    !base::is.null(values[[nm]]) && !(nm %in% base::names(defaults) && base::isTRUE(base::all.equal(values[[nm]], defaults[[nm]])))
  }, base::logical(1))
  values[keep]
}

# Copy, CSV and Excel buttons that save as `filename` with no title row. As in
# the Compare tab, they export the unrounded values ('export' orthogonal data)
# and write NA cells as blanks rather than "null".
annotate_export_buttons <- function(filename) {
  format <- base::list(body = DT::JS("function(data, row, column) { return data === null || data === undefined ? '' : data; }"))
  base::lapply(c("copy", "csv", "excel"), function(kind) {
    button <- base::list(extend = kind, title = "", exportOptions = base::list(orthogonal = "export", format = format))
    if (kind != "copy") {
      button$filename <- filename
    }
    button
  })
}

# A plot control's number: `default` when the box is empty, otherwise at least `min`.
annotate_number_or <- function(x, default, min = -Inf) {
  if (base::is.null(x) || base::length(x) == 0 || base::is.na(x[1])) {
    return(default)
  }
  base::max(min, x[1])
}
