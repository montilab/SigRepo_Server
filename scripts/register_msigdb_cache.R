#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(devtools)
})

find_repo_root <- function() {
  file_arg <- commandArgs(trailingOnly = FALSE)
  file_prefix <- "--file="
  file_entry <- file_arg[startsWith(file_arg, file_prefix)][1]

  script_path <- if (!is.na(file_entry)) {
    normalizePath(sub(file_prefix, "", file_entry), winslash = "/", mustWork = FALSE)
  } else {
    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  }

  script_dir <- if (dir.exists(script_path)) script_path else dirname(script_path)
  normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = FALSE)
}

repo_root <- find_repo_root()
renviron_path <- file.path(repo_root, ".Renviron")

if (!file.exists(renviron_path)) {
  stop(
    sprintf(
      "Missing %s. Start with: cp %s %s",
      renviron_path,
      file.path(repo_root, "setup", "config", ".Renviron.example"),
      renviron_path
    ),
    call. = FALSE
  )
}

readRenviron(renviron_path)

resolve_host_repo_path <- function(path_value, fallback_relative) {
  if (!nzchar(path_value)) {
    return(normalizePath(file.path(repo_root, fallback_relative), winslash = "/", mustWork = FALSE))
  }

  if (dir.exists(path_value) && file.exists(file.path(path_value, "DESCRIPTION"))) {
    return(normalizePath(path_value, winslash = "/", mustWork = FALSE))
  }

  if (startsWith(path_value, "/")) {
    fallback_path <- normalizePath(file.path(repo_root, fallback_relative), winslash = "/", mustWork = FALSE)
    if (dir.exists(fallback_path) && file.exists(file.path(fallback_path, "DESCRIPTION"))) {
      return(fallback_path)
    }
  }

  normalizePath(path_value, winslash = "/", mustWork = FALSE)
}

sigrepo_dir <- resolve_host_repo_path(Sys.getenv("SIGREPO_DIR", unset = ""), "../SigRepo")

if (!dir.exists(sigrepo_dir) || !file.exists(file.path(sigrepo_dir, "DESCRIPTION"))) {
  stop(
    sprintf(
      "Could not resolve a local SigRepo package path. Checked: %s",
      sigrepo_dir
    ),
    call. = FALSE
  )
}

devtools::load_all(sigrepo_dir, quiet = TRUE)

cache_dir <- Sys.getenv(
  "MSIGDB_CACHE_DIR",
  unset = file.path(repo_root, "shiny", "data", "msigdb_genesets")
)
cache_dir <- normalizePath(cache_dir, winslash = "/", mustWork = FALSE)

manifest_csv <- file.path(cache_dir, "manifest.csv")
if (!file.exists(manifest_csv)) {
  stop(
    sprintf(
      "Missing manifest.csv in %s. Run scripts/build_msigdb_cache.R first.",
      cache_dir
    ),
    call. = FALSE
  )
}

args <- commandArgs(trailingOnly = TRUE)
register_entries <- "--with-entries" %in% args

message("Loading manifest: ", manifest_csv)
manifest_tbl <- utils::read.csv(
  manifest_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE
) |>
  dplyr::mutate(
    subcollection = dplyr::if_else(
      is.na(.data$subcollection) | !nzchar(.data$subcollection),
      NA_character_,
      .data$subcollection
    ),
    notes = dplyr::case_when(
      .data$source == "msigdb" & .data$collection %in% c("H", "MH") ~ "MSigDB hallmark cache",
      .data$source == "msigdb" ~ "MSigDB cached geneset resource",
      TRUE ~ NA_character_
    )
  )

resource_tbl <- manifest_tbl |>
  dplyr::select(
    .data$source,
    .data$species,
    .data$collection,
    .data$subcollection,
    .data$version,
    .data$source_version,
    .data$format,
    .data$storage_path,
    .data$checksum,
    .data$n_genesets,
    .data$n_features,
    .data$is_current,
    .data$notes
  )

conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DB_NAME"),
  host = {
    db_host <- Sys.getenv("DB_HOST", unset = "")
    db_local_host <- Sys.getenv("DB_LOCAL_HOST", unset = "")
    if (db_host %in% c("sigrepo-mysql", "") && db_local_host %in% c("sigrepo-mysql", "")) {
      "127.0.0.1"
    } else if (!db_local_host %in% c("", "sigrepo-mysql")) {
      db_local_host
    } else {
      db_host
    }
  },
  port = as.integer(Sys.getenv("DB_PORT")),
  user = if (nzchar(Sys.getenv("DB_USER", unset = ""))) Sys.getenv("DB_USER") else "root",
  password = if (nzchar(Sys.getenv("DB_PASSWORD", unset = ""))) Sys.getenv("DB_PASSWORD") else Sys.getenv("MYSQL_ROOT_PASSWORD", unset = "sigrepo")
)

message("Registering geneset resources in SigRepo...")
SigRepo::addGenesetResource(
  conn_handler = conn_handler,
  geneset_resource = resource_tbl,
  verbose = TRUE
)

if (!register_entries) {
  message("Skipping geneset entry registration. Use --with-entries to load pathway-level metadata.")
  quit(save = "no", status = 0)
}

message("Looking up registered geneset resources...")
registered_resources <- SigRepo::searchGenesetResource(
  conn_handler = conn_handler,
  source = unique(resource_tbl$source),
  verbose = FALSE
)

entry_tbl_list <- vector("list", nrow(manifest_tbl))

for (i in seq_len(nrow(manifest_tbl))) {
  manifest_row <- manifest_tbl[i, , drop = FALSE]

  resource_match <- registered_resources |>
    dplyr::filter(
      tolower(trimws(.data$source)) == tolower(trimws(manifest_row$source[[1]])),
      tolower(trimws(.data$species)) == tolower(trimws(manifest_row$species[[1]])),
      tolower(trimws(.data$collection)) == tolower(trimws(manifest_row$collection[[1]])),
      tolower(trimws(dplyr::coalesce(.data$subcollection, ""))) ==
        tolower(trimws(dplyr::coalesce(manifest_row$subcollection[[1]], ""))),
      tolower(trimws(.data$version)) == tolower(trimws(manifest_row$version[[1]]))
    )

  if (nrow(resource_match) == 0) {
    stop(
      sprintf(
        "Could not find registered geneset resource for %s / %s / %s / %s / %s",
        manifest_row$source[[1]],
        manifest_row$species[[1]],
        manifest_row$collection[[1]],
        dplyr::coalesce(manifest_row$subcollection[[1]], "all"),
        manifest_row$version[[1]]
      ),
      call. = FALSE
    )
  }

  genesets <- readRDS(manifest_row$storage_path[[1]])
  geneset_names <- names(genesets)
  geneset_sizes <- vapply(genesets, length, integer(1))

  entry_tbl_list[[i]] <- data.frame(
    geneset_resource_id = resource_match$geneset_resource_id[[1]],
    geneset_name = geneset_names,
    description = NA_character_,
    n_features = as.integer(geneset_sizes),
    stringsAsFactors = FALSE
  )
}

entry_tbl <- dplyr::bind_rows(entry_tbl_list)

message("Registering geneset entries in SigRepo...")
SigRepo::addGenesetEntry(
  conn_handler = conn_handler,
  geneset_entry = entry_tbl,
  verbose = TRUE
)

message("Finished registering geneset resources and entries.")
