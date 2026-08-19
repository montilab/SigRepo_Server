# MSigDB gene set resolution: on-disk cache first, live fetch only if
# explicitly allowed. Mirrors the retired Shiny app's hypeR caching
# functions (msigdb_cache_dir/msigdb_cache_file/load_cached_msigdb_genesets/
# runtime_msigdb_fetch_allowed/fetch_msigdb_table) so the API and the Shiny
# app share the same cache directory and behavior -- ported rather than
# reused directly since the Shiny module isn't part of the SigRepo package
# and isn't sourced into the API process. Cache is built by
# scripts/build_msigdb_cache.R; see data/msigdb_genesets/manifest.csv.
#
# cache_dir is always passed explicitly (never read from a global), matching
# how difexp_dir is threaded through api/lib/difexp.R -- keeps these
# functions testable in isolation without booting the whole API.

# The same fixed Collection/Subcollection matrix Shiny's picker uses (see
# hypeR_module.R's msigdb_collection_metadata) -- exposed over REST so the
# React picker doesn't need to hardcode a second copy.
msigdb_collection_metadata <- function() {
  base::data.frame(
    collection = c(
      "H", "C1",
      base::rep("C2", 8),
      base::rep("C3", 4),
      base::rep("C4", 3),
      base::rep("C5", 4),
      "C6",
      base::rep("C7", 2),
      "C8"
    ),
    collection_label = c(
      "Hallmark (H)", "Positional (C1)",
      base::rep("Curated (C2)", 8),
      base::rep("Regulatory Target (C3)", 4),
      base::rep("Computational (C4)", 3),
      base::rep("Ontology (C5)", 4),
      "Oncogenic Signature (C6)",
      base::rep("Immunologic Signature (C7)", 2),
      "Cell Type Signature (C8)"
    ),
    subcollection = c(
      "", "",
      "CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY", "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME", "CP:WIKIPATHWAYS",
      "MIR:MIRDB", "MIR:MIR_LEGACY", "TFT:GTRD", "TFT:TFT_LEGACY",
      "3CA", "CGN", "CM",
      "GO:BP", "GO:CC", "GO:MF", "HPO",
      "",
      "IMMUNESIGDB", "VAX",
      ""
    ),
    stringsAsFactors = FALSE
  )
}

# Resolves the configured cache dir: MSIGDB_CACHE_DIR if set, else
# <sigrepo_server_path>/data/msigdb_genesets, the shared cache the API and the
# MCP server both read from. It is not committed: it is derived data, populated
# on demand by /init_db_genesets (or as part of /init_db), and a miss simply
# returns NULL so a fresh checkout works before the cache is built.
default_msigdb_cache_dir <- function(sigrepo_server_path) {
  env_cache_dir <- base::Sys.getenv("MSIGDB_CACHE_DIR", unset = "")
  if (base::nzchar(env_cache_dir)) {
    return(base::normalizePath(env_cache_dir, winslash = "/", mustWork = FALSE))
  }
  base::normalizePath(base::file.path(sigrepo_server_path, "data", "msigdb_genesets"), winslash = "/", mustWork = FALSE)
}

# Real species list msigdbr supports (static/local, no network) -- matches
# the Shiny app's species picker (selectInput fed by msigdbr_species()).
msigdb_species_options <- function() {
  msigdbr::msigdbr_species()$species_name
}

msigdb_slugify <- function(x) {
  x <- base::gsub("[^A-Za-z0-9]+", "_", x)
  x <- base::gsub("_+", "_", x)
  base::gsub("^_|_$", "", x)
}

msigdb_cache_file <- function(cache_dir, species, collection, subcollection = "") {
  species_slug <- msigdb_slugify(species)
  subcollection_slug <- if (!base::is.null(subcollection) && base::nzchar(subcollection)) {
    msigdb_slugify(subcollection)
  } else {
    "all"
  }
  base::file.path(cache_dir, base::sprintf("%s__%s__%s.rds", species_slug, collection, subcollection_slug))
}

load_cached_msigdb_genesets <- function(cache_dir, species, collection, subcollection = "") {
  cache_file <- msigdb_cache_file(cache_dir, species, collection, subcollection)
  if (!base::file.exists(cache_file)) {
    return(NULL)
  }
  base::readRDS(cache_file)
}

runtime_msigdb_fetch_allowed <- function() {
  value <- base::tolower(base::Sys.getenv("MSIGDB_ALLOW_RUNTIME_FETCH", unset = "false"))
  value %in% c("true", "1", "yes", "y")
}

fetch_msigdb_table_live <- function(species, collection, subcollection = "") {
  msigdbr_args <- base::list(species = species)
  msigdbr_formals <- base::names(base::formals(msigdbr::msigdbr))

  if ("collection" %in% msigdbr_formals) {
    msigdbr_args$collection <- collection
  } else if ("category" %in% msigdbr_formals) {
    msigdbr_args$category <- collection
  }

  if (!base::is.null(subcollection) && base::nzchar(subcollection)) {
    if ("subcollection" %in% msigdbr_formals) {
      msigdbr_args$subcollection <- subcollection
    } else if ("subcategory" %in% msigdbr_formals) {
      msigdbr_args$subcategory <- subcollection
    }
  }

  msigdb_tbl <- base::do.call(msigdbr::msigdbr, msigdbr_args)
  gs <- base::split(msigdb_tbl$gene_symbol, msigdb_tbl$gs_name)
  base::lapply(gs, base::unique)
}

# Cache-first, live-fallback-if-allowed resolution of an MSigDB collection
# into a named list of gene symbol vectors. Returns
# list(ok = TRUE, genesets = <named list>, source = "cache" | "live") or
# list(ok = FALSE, reason = "not_cached" | "fetch_failed", message = ...).
resolve_msigdb_genesets <- function(cache_dir, species, collection, subcollection = "") {
  cached <- load_cached_msigdb_genesets(cache_dir, species, collection, subcollection)
  if (!base::is.null(cached)) {
    return(base::list(ok = TRUE, genesets = cached, source = "cache"))
  }

  if (!runtime_msigdb_fetch_allowed()) {
    return(base::list(
      ok = FALSE, reason = "not_cached",
      message = base::sprintf(
        "MSigDB cache file was not found: %s. Build the cache (scripts/build_msigdb_cache.R) or set MSIGDB_ALLOW_RUNTIME_FETCH=true.",
        msigdb_cache_file(cache_dir, species, collection, subcollection)
      )
    ))
  }

  base::tryCatch(
    base::list(ok = TRUE, genesets = fetch_msigdb_table_live(species, collection, subcollection), source = "live"),
    error = function(err) base::list(ok = FALSE, reason = "fetch_failed", message = err$message)
  )
}
