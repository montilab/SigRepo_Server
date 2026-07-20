# Populates the on-disk MSigDB cache (the same cache api/lib/msigdb_cache.R's
# /annotate/* routes read from) and registers it into
# geneset_resources/geneset_entries. Backs /init_db_genesets, and /init_db's
# combined bootstrap.
#
# This is the same algorithm as scripts/build_msigdb_cache.R +
# scripts/register_msigdb_cache.R, restructured as plain functions with no
# top-level execution and no commandArgs() -- those two scripts remain
# untouched for manual/offline use; this is what lets the same population
# step run from a running API process instead. Reuses msigdb_cache_file()
# from api/lib/msigdb_cache.R for the cache filename convention so anything
# written here is immediately visible to /annotate/genesets -- duplicating
# that logic a third time would risk the two silently drifting apart.

msigdb_genesets_species_default <- c("Homo sapiens", "Mus musculus")

# H (Hallmark), C2 (curated: canonical pathways), C5 (ontology: GO terms) --
# what typical signature enrichment work actually reaches for. Everything
# else msigdbr knows about (positional, regulatory-target, computational,
# oncogenic, immunologic, cell-type) is real but specialized, and pulling
# all of it for every species by default made a fresh bootstrap
# disproportionately slow for collections most installs won't touch.
# Derived from msigdb_collection_metadata() (api/lib/msigdb_cache.R) rather
# than hardcoded a second time, so the two can't drift apart.
msigdb_curated_collection_table <- function() {
  msigdb_collection_metadata() |>
    dplyr::filter(.data$collection %in% c("H", "C2", "C5")) |>
    dplyr::transmute(gs_collection = .data$collection, gs_subcollection = .data$subcollection) |>
    dplyr::mutate(gs_subcollection = dplyr::na_if(.data$gs_subcollection, ""))
}

# One species/collection/subcollection combo's gene sets, as a named list of
# gene-symbol vectors -- the same shape resolve_msigdb_genesets() expects
# when it reads a cache file back for /annotate/genesets.
fetch_msigdb_geneset_table <- function(species, collection, subcollection = "") {
  msigdbr_args <- base::list(species = species)
  msigdbr_formals <- base::names(base::formals(msigdbr::msigdbr))

  if ("collection" %in% msigdbr_formals) {
    msigdbr_args$collection <- collection
  } else if ("category" %in% msigdbr_formals) {
    msigdbr_args$category <- collection
  }

  if (base::nzchar(subcollection)) {
    if ("subcollection" %in% msigdbr_formals) {
      msigdbr_args$subcollection <- subcollection
    } else if ("subcategory" %in% msigdbr_formals) {
      msigdbr_args$subcategory <- subcollection
    }
  }

  msigdb_tbl <- base::do.call(msigdbr::msigdbr, msigdbr_args) |>
    dplyr::select(.data$gs_name, .data$gene_symbol)

  genesets <- base::split(msigdb_tbl$gene_symbol, msigdb_tbl$gs_name)
  base::lapply(genesets, base::unique)
}

# Writes one .rds per species/collection/subcollection to cache_dir (via
# msigdb_cache_file(), so /annotate/* finds them under the same names) and
# returns a manifest data.frame describing what was written -- the same
# shape scripts/build_msigdb_cache.R's manifest.csv has.
#
# collection_table: NULL (default) uses the curated H/C2/C5 set
# (msigdb_curated_collection_table()); pass the literal string "all" for
# every collection msigdbr knows about; pass a data.frame
# (gs_collection/gs_subcollection columns) for anything else, e.g. one
# specific collection for a quick test or an on-demand fetch.
build_msigdb_geneset_cache <- function(cache_dir, species_list = msigdb_genesets_species_default,
                                        collection_table = NULL) {
  base::dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

  if (base::is.null(collection_table)) {
    collection_table <- msigdb_curated_collection_table()
  } else if (base::identical(collection_table, "all")) {
    collection_table <- msigdbr::msigdbr_collections() |>
      dplyr::select(.data$gs_collection, .data$gs_subcollection) |>
      dplyr::distinct() |>
      dplyr::arrange(.data$gs_collection, .data$gs_subcollection)
  }

  msigdb_version <- base::as.character(utils::packageVersion("msigdbr"))
  manifest_rows <- base::list()

  for (species in species_list) {
    base::print(base::sprintf("Building MSigDB cache for: %s", species))

    for (i in base::seq_len(base::nrow(collection_table))) {
      collection <- collection_table$gs_collection[[i]]
      subcollection <- collection_table$gs_subcollection[[i]]
      subcollection <- if (base::is.na(subcollection)) "" else subcollection
      cache_file <- msigdb_cache_file(cache_dir, species, collection, subcollection)

      genesets <- fetch_msigdb_geneset_table(species, collection, subcollection)
      base::saveRDS(genesets, cache_file, compress = "xz")

      manifest_rows[[base::length(manifest_rows) + 1]] <- base::data.frame(
        source = "msigdb",
        species = species,
        collection = collection,
        subcollection = if (base::nzchar(subcollection)) subcollection else NA_character_,
        version = base::format(base::Sys.Date(), "%Y-%m-%d"),
        source_version = base::paste0("msigdbr_", msigdb_version),
        format = "rds",
        storage_path = cache_file,
        checksum = digest::digest(file = cache_file, algo = "md5", serialize = FALSE),
        n_genesets = base::length(genesets),
        n_features = base::length(base::unique(base::unlist(genesets, use.names = FALSE))),
        is_current = 1L,
        stringsAsFactors = FALSE
      )
    }
  }

  manifest_df <- base::do.call(base::rbind, manifest_rows)
  utils::write.csv(manifest_df, base::file.path(cache_dir, "manifest.csv"), row.names = FALSE)
  base::saveRDS(manifest_df, base::file.path(cache_dir, "manifest.rds"), compress = "xz")

  manifest_df
}

# Registers a manifest (as returned by build_msigdb_geneset_cache()) into
# geneset_resources, and optionally geneset_entries for pathway-level
# detail. Existing resources with the same
# source/species/collection/subcollection/version are left alone --
# SigRepo::addGenesetResource() is responsible for that de-duplication, not
# this function.
register_msigdb_genesets <- function(conn_handler, manifest_df, register_entries = TRUE) {
  resource_tbl <- manifest_df |>
    dplyr::mutate(
      notes = dplyr::case_when(
        .data$source == "msigdb" & .data$collection %in% c("H", "MH") ~ "MSigDB hallmark cache",
        .data$source == "msigdb" ~ "MSigDB cached geneset resource",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(
      .data$source, .data$species, .data$collection, .data$subcollection,
      .data$version, .data$source_version, .data$format, .data$storage_path,
      .data$checksum, .data$n_genesets, .data$n_features, .data$is_current, .data$notes
    )

  base::print("Registering geneset resources...")
  SigRepo::addGenesetResource(conn_handler = conn_handler, geneset_resource = resource_tbl, verbose = TRUE)

  if (!register_entries) {
    return(invisible(NULL))
  }

  base::print("Looking up registered geneset resources...")
  registered_resources <- SigRepo::searchGenesetResource(
    conn_handler = conn_handler,
    source = base::unique(resource_tbl$source),
    verbose = FALSE
  )

  entry_tbl_list <- base::vector("list", base::nrow(manifest_df))

  for (i in base::seq_len(base::nrow(manifest_df))) {
    manifest_row <- manifest_df[i, , drop = FALSE]

    resource_match <- registered_resources |>
      dplyr::filter(
        base::tolower(base::trimws(.data$source)) == base::tolower(base::trimws(manifest_row$source[[1]])),
        base::tolower(base::trimws(.data$species)) == base::tolower(base::trimws(manifest_row$species[[1]])),
        base::tolower(base::trimws(.data$collection)) == base::tolower(base::trimws(manifest_row$collection[[1]])),
        base::tolower(base::trimws(dplyr::coalesce(.data$subcollection, ""))) ==
          base::tolower(base::trimws(dplyr::coalesce(manifest_row$subcollection[[1]], ""))),
        base::tolower(base::trimws(.data$version)) == base::tolower(base::trimws(manifest_row$version[[1]]))
      )

    if (base::nrow(resource_match) == 0) {
      next
    }

    genesets <- base::readRDS(manifest_row$storage_path[[1]])
    entry_tbl_list[[i]] <- base::data.frame(
      geneset_resource_id = resource_match$geneset_resource_id[[1]],
      geneset_name = base::names(genesets),
      description = NA_character_,
      n_features = base::vapply(genesets, base::length, integer(1)),
      stringsAsFactors = FALSE
    )
  }

  entry_tbl <- dplyr::bind_rows(entry_tbl_list)

  base::print("Registering geneset entries...")
  SigRepo::addGenesetEntry(conn_handler = conn_handler, geneset_entry = entry_tbl, verbose = TRUE)

  invisible(NULL)
}

# Convenience wrapper: build the cache, then register it -- the whole thing
# /init_db_genesets (and /init_db) calls in one step.
generate_msigdb_genesets <- function(conn_handler, cache_dir, species_list = msigdb_genesets_species_default,
                                      register_entries = TRUE, collection_table = NULL) {
  manifest_df <- build_msigdb_geneset_cache(cache_dir, species_list, collection_table)
  register_msigdb_genesets(conn_handler, manifest_df, register_entries)
  manifest_df
}

# One species/collection/subcollection, on demand: returns its
# geneset_resources row if it's already registered and current, or fetches
# + registers it from MSigDB right now if not. Backs POST
# /geneset_resources/ensure, and is meant to be reusable from anywhere else
# that needs "make sure this collection exists" -- an MCP tool, a future
# route -- rather than duplicating the fetch-then-register logic per caller.
#
# conn_handler here should be the API's own privileged connection, not one
# built from whichever end-user's api_key called the route:
# SigRepo::addGenesetResource()/addGenesetEntry() both require an
# 'admin'-role connection internally regardless of the caller's own role,
# same as every other /init_db* route already does. The end-user's api_key
# still gates *whether* they can call this at all -- see
# ensure_geneset_resource_route() in api.R -- just not what the underlying
# write runs as.
#
# subcollection should be "" only for collections that genuinely have none
# (H, C1, C6, C8) -- passing "" for a collection with several
# subcollections (e.g. C2) skips the subcollection filter entirely and can
# match/return an arbitrary one of them.
ensure_msigdb_geneset_resource <- function(conn_handler, cache_dir, species, collection, subcollection = "") {
  subcollection <- if (base::is.null(subcollection)) "" else base::trimws(subcollection[1])
  subcollection_filter <- if (base::nzchar(subcollection)) subcollection else NULL

  existing <- SigRepo::searchGenesetResource(
    conn_handler = conn_handler,
    species = species,
    collection = collection,
    subcollection = subcollection_filter,
    is_current = 1,
    verbose = FALSE
  )

  if (base::nrow(existing) > 0) {
    return(base::list(resource = existing[1, , drop = FALSE], fetched = FALSE))
  }

  collection_table <- base::data.frame(
    gs_collection = collection,
    gs_subcollection = if (base::nzchar(subcollection)) subcollection else NA_character_,
    stringsAsFactors = FALSE
  )

  manifest_df <- build_msigdb_geneset_cache(cache_dir, species_list = species, collection_table = collection_table)
  register_msigdb_genesets(conn_handler, manifest_df, register_entries = TRUE)

  registered <- SigRepo::searchGenesetResource(
    conn_handler = conn_handler,
    species = species,
    collection = collection,
    subcollection = subcollection_filter,
    is_current = 1,
    verbose = FALSE
  )

  base::list(resource = registered[1, , drop = FALSE], fetched = TRUE)
}
