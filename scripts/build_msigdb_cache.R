#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(msigdbr)
})

species_to_cache <- c(
  "Homo sapiens",
  "Mus musculus"
)

default_output_dir <- file.path("shiny", "data", "msigdb_genesets")
output_dir <- Sys.getenv("MSIGDB_CACHE_DIR", unset = default_output_dir)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

slugify <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

fetch_msigdb <- function(species, collection, subcollection = "") {
  msigdbr_args <- list(species = species)
  msigdbr_formals <- names(formals(msigdbr::msigdbr))

  if ("collection" %in% msigdbr_formals) {
    msigdbr_args$collection <- collection
  } else if ("category" %in% msigdbr_formals) {
    msigdbr_args$category <- collection
  }

  if (nzchar(subcollection)) {
    if ("subcollection" %in% msigdbr_formals) {
      msigdbr_args$subcollection <- subcollection
    } else if ("subcategory" %in% msigdbr_formals) {
      msigdbr_args$subcategory <- subcollection
    }
  }

  do.call(msigdbr::msigdbr, msigdbr_args)
}

geneset_cache_path <- function(species, collection, subcollection) {
  species_slug <- slugify(species)
  subcollection_slug <- if (nzchar(subcollection)) slugify(subcollection) else "all"
  file.path(
    output_dir,
    sprintf("%s__%s__%s.rds", species_slug, collection, subcollection_slug)
  )
}

collections <- msigdbr::msigdbr_collections() |>
  dplyr::select(gs_collection, gs_subcollection) |>
  dplyr::distinct() |>
  dplyr::arrange(gs_collection, gs_subcollection)

manifest <- list()

for (species in species_to_cache) {
  message("Building MSigDB cache for: ", species)

  for (i in seq_len(nrow(collections))) {
    collection <- collections$gs_collection[[i]]
    subcollection <- collections$gs_subcollection[[i]]
    cache_file <- geneset_cache_path(species, collection, subcollection)

    message(
      "  - ",
      collection,
      if (nzchar(subcollection)) paste0(" / ", subcollection) else "",
      " -> ",
      cache_file
    )

    msigdb_tbl <- fetch_msigdb(
      species = species,
      collection = collection,
      subcollection = subcollection
    ) |>
      dplyr::select(gs_name, gene_symbol)

    genesets <- split(msigdb_tbl$gene_symbol, msigdb_tbl$gs_name)
    genesets <- lapply(genesets, unique)

    saveRDS(genesets, cache_file, compress = "xz")

    manifest[[length(manifest) + 1]] <- data.frame(
      species = species,
      gs_collection = collection,
      gs_subcollection = subcollection,
      file = basename(cache_file),
      genesets = length(genesets),
      genes = length(unique(unlist(genesets, use.names = FALSE))),
      stringsAsFactors = FALSE
    )

    rm(msigdb_tbl, genesets)
    gc(verbose = FALSE)
  }
}

manifest_df <- do.call(rbind, manifest)
manifest_file <- file.path(output_dir, "manifest.rds")
manifest_csv <- file.path(output_dir, "manifest.csv")

saveRDS(manifest_df, manifest_file, compress = "xz")
utils::write.csv(manifest_df, manifest_csv, row.names = FALSE)

message("MSigDB cache complete.")
message("Manifest: ", manifest_file)
