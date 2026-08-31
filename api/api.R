
# For API
library(plumber)
library(httr)
library(jsonlite)

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning
library(dplyr)

load_repo_package <- function(repo_dir, package_name, required = TRUE) {
  repo_dir <- base::Sys.getenv(repo_dir, unset = repo_dir)

  if (base::nzchar(repo_dir) && base::dir.exists(repo_dir)) {
    if (requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(path = repo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
      return(invisible(TRUE))
    }

    if (requireNamespace("devtools", quietly = TRUE)) {
      devtools::load_all(path = repo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
      return(invisible(TRUE))
    }
  }

  if (requireNamespace(package_name, quietly = TRUE)) {
    base::library(package_name, character.only = TRUE)
    return(invisible(TRUE))
  }

  if (!required) {
    return(invisible(FALSE))
  }

  base::stop(
    base::sprintf(
      "Cannot load package '%s'. Checked repo path '%s' and installed packages, but neither pkgload/devtools nor the installed package were available.",
      package_name,
      repo_dir
    )
  )
}

# Load SigRepo package
load_repo_package("SIGREPO_DIR", "SigRepo")

## Create a database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"),
  host = base::Sys.getenv("DB_LOCAL_HOST"),
  port = base::as.integer(base::Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD")
)

# Create difexp directory
difexp_dir <- base::file.path(base::Sys.getenv("DIFEXP_DIR"))
base::dir.create(path = difexp_dir, showWarnings = FALSE, recursive = TRUE, mode = "0777")

# Get sigrepo server path
sigrepo_server_path <- base::Sys.getenv("SIGREPO_SERVER_DIR")

# Get the
#
# This is a Plumber API. You can run the API by clicking the 'run API' button above
#
# Found out more about building APIs with Plumber here:
#
#       https://www.rplumber.io/
#

# API title and description
#* @apiTitle Plumber API
#* @apiDescription This is a server for accessing data on the SigRepo Database
#* @apiContact list(name = "Reina Chau", email = "rchau88@bu.edu")

# Set-up header access

#* @filter cors
cors <- function(res){
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Log some information about the incoming request
#* @filter logger
function(req){
  base::cat(base::as.character(Sys.time()), "-",
            req$REQUEST_METHOD, req$PATH_INFO, "-",
            req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

# Create a list of serializers to return the object ####
serializers <- base::list(
  "html" = plumber::serializer_html(),
  # Encode json_response() payloads exactly once, with the same options the
  # helper used to apply by hand -- see api/lib/common.R. (The difexp routes
  # keep their own hand-rolled jsonlite::toJSON + plumber's default
  # serializer, which the SigRepo R client double-decodes on purpose.)
  "json" = plumber::serializer_json(auto_unbox = TRUE, null = "null", na = "null", pretty = TRUE),
  "csv" = plumber::serializer_csv(),
  "rds" = plumber::serializer_rds(),
  "pdf" = plumber::serializer_pdf(),
  "text" = plumber::serializer_text(),
  "htmlwidget" = plumber::serializer_htmlwidget()
)

# Load domain logic (auth, DB access, business logic) from api/lib/*.R.
# Endpoints below stay in this file because Plumber only parses `#*` route
# annotations from the file it plumb()s directly; everything else lives in
# lib/ so it can be unit/integration tested without booting the API.
for (lib_file in base::sort(base::list.files(base::file.path(sigrepo_server_path, "api", "lib"), pattern = "\\.R$", full.names = TRUE))) {
  base::source(lib_file, local = TRUE)
}

# Resolved once at boot (default_msigdb_cache_dir lives in api/lib/msigdb_cache.R)
# so /init_db_genesets, /geneset_resources/ensure, /init_db's combined
# bootstrap, and every /annotate/* request all share the same cache lookup.
msigdb_cache_dir <- default_msigdb_cache_dir(sigrepo_server_path)

#* Initiate database with schemas and reference tables
#* @param admin_key
#' @post /init_db
init_db <- function(res, admin_key){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  base::tryCatch({
    print("Initiate schema for the database...")
    generate_db_schema(sigrepo_server_path)

    print("Upload reference tables to the database...")
    generate_db_tables(conn_handler = conn_handler, sigrepo_server_path = sigrepo_server_path)

    print("Building and registering MSigDB gene sets...")
    generate_msigdb_genesets(conn_handler = conn_handler, cache_dir = msigdb_cache_dir)

    json_response(res, 200, base::data.frame(MESSAGES = "Finish initialized the database."))
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Reset schemas and reference tables in the database
#* @param admin_key
#' @post /reset_db
reset_db <- function(res, admin_key){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  base::tryCatch({
    print("Reset tables in the database...")
    reset_db_tables(conn_handler = conn_handler)

    json_response(res, 200, base::data.frame(MESSAGES = "Finish reset the database."))
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Initiate schema for the database
#* @param admin_key
#' @post /init_db_schema
init_db_schema <- function(res, admin_key){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  base::tryCatch({
    print("Initiate schema for the database...")
    generate_db_schema(sigrepo_server_path)

    json_response(res, 200, base::data.frame(MESSAGES = "Finish initialized schema for the database."))
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Initiate reference tables in the database
#* @param admin_key
#' @post /init_db_tables
init_db_tables <- function(res, admin_key){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  base::tryCatch({
    print("Upload reference tables to the database...")
    generate_db_tables(conn_handler = conn_handler, sigrepo_server_path = sigrepo_server_path)

    json_response(res, 200, base::data.frame(MESSAGES = "Finish initialized reference tables for the database."))
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Build and register the MSigDB gene-set cache (geneset_resources/geneset_entries).
#* Defaults to the curated H/C2/C5 set; pass full_sweep=true for every
#* collection msigdbr knows about (meaningfully slower).
#* @param admin_key
#* @param full_sweep
#' @post /init_db_genesets
init_db_genesets <- function(res, admin_key, full_sweep = FALSE){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  collection_table <- if (normalize_flag(full_sweep, default = FALSE)) "all" else NULL

  base::tryCatch({
    print("Building and registering MSigDB gene sets...")
    manifest_df <- generate_msigdb_genesets(
      conn_handler = conn_handler, cache_dir = msigdb_cache_dir, collection_table = collection_table
    )

    json_response(res, 200, base::data.frame(MESSAGES = base::sprintf(
      "Finished building and registering %d MSigDB geneset resource(s).", base::nrow(manifest_df)
    )))
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Return a geneset_resources row for one species/collection/subcollection,
#* fetching and registering it from MSigDB on the fly if it isn't already in
#* the database. Available to any authenticated user, not admin-gated --
#* MSigDB content is public reference data, not user content; the write
#* itself still runs via the API's own privileged connection regardless of
#* the caller's own role, same as every other geneset-registering route.
#* @parser json
#* @param api_key
#* @param species
#* @param collection
#* @param subcollection
#' @post /geneset_resources/ensure
ensure_geneset_resource_route <- function(req, res, api_key = "", species = "", collection = "", subcollection = ""){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  species <- if (identical(json_scalar(species), "")) json_scalar(body$species) else json_scalar(species)
  collection <- if (identical(json_scalar(collection), "")) json_scalar(body$collection) else json_scalar(collection)
  subcollection <- if (identical(json_scalar(subcollection), "")) json_scalar(body$subcollection) else json_scalar(subcollection)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (identical(species, "") || identical(collection, "")) {
    return(json_error(res, 404, "species and collection are required."))
  }

  base::tryCatch({
    result <- ensure_msigdb_geneset_resource(
      conn_handler = conn_handler, cache_dir = msigdb_cache_dir,
      species = species, collection = collection, subcollection = subcollection
    )

    payload <- result$resource
    payload$fetched_on_demand <- result$fetched
    json_response(res, 200, payload)
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Extract data from biomaRt package and update transcriptomics feature set in the database.
#* @param admin_key
#* @param organism
#' @post /update_transcriptomics
update_transcriptomics <- function(res, admin_key, organism = NULL){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  organism <- parse_organism_filter(organism)

  print("Getting organisms from the database...")
  organism_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler, organism = organism) |>
    dplyr::filter(!.data$biomart_db %in% c(NA, "") & !.data$biomart_dataset %in% c(NA, ""))

  if (base::nrow(organism_tbl) == 0) {
    return(json_response(res, 200, base::data.frame(MESSAGES = "There are no organisms returned from the search parameters.")))
  }

  print("Updating transcriptomics features to the database for each given organism...")
  for (s in base::seq_len(base::nrow(organism_tbl))) {
    base::tryCatch({
      SigRepo::updateTranscriptomicsFeatureSet(
        conn_handler = conn_handler,
        organism = organism_tbl$organism[s]
      )
    }, error = function(e){
      json_response(res, 200, base::data.frame(MESSAGES = base::as.character(e)))
    })
  }

  json_response(res, 200, base::data.frame(MESSAGES = "Finish updating transcriptomics feature set."))
}

#* Retrieve FTP UniProt data from NCBI and update proteomics feature set in the database
#* @param admin_key
#* @param organism
#' @post /update_proteomics
update_proteomics <- function(res, admin_key, organism = NULL){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  organism <- parse_organism_filter(organism)

  print("Getting organisms from the database...")
  organism_tbl <- SigRepo::searchOrganism(conn_handler = conn_handler, organism = organism) |>
    dplyr::filter(!.data$prot_organism_code %in% c(NA, "") & !.data$prot_organism_code %in% c(NA, ""))

  if (base::nrow(organism_tbl) == 0) {
    return(json_response(res, 200, base::data.frame(MESSAGES = "There are no organisms returned from the search parameters.")))
  }

  print("Updating proteomics features in the database for each given organism...")
  for (s in base::seq_len(base::nrow(organism_tbl))) {
    base::tryCatch({
      SigRepo::updateProteomicsFeatureSet(
        conn_handler = conn_handler,
        organism = organism_tbl$organism[s]
      )
    }, error = function(e){
      json_response(res, 200, base::data.frame(MESSAGES = base::as.character(e)))
    })
  }

  json_response(res, 200, base::data.frame(MESSAGES = "Finish updating proteomics feature set."))
}

#* Show a list of tables in the database
#* @param admin_key
#' @get /show_db_tables
show_db_tables <- function(res, admin_key){
  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  base::tryCatch({
    conn <- db_connect_local()
    table_result <- base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "SHOW TABLES;"))
    base::suppressWarnings(DBI::dbDisconnect(conn))

    if (base::nrow(table_result) > 0) {
      json_response(res, 200, table_result)
    } else {
      json_response(res, 200, base::data.frame(MESSAGES = "Currently, there are no tables existed in the database."))
    }
  }, error = function(err){
    print(err)
    json_error(res, 500, "Something went wrong. Contact admin for support.")
  })
}

#* Retrieve a specific table from the database
#* @param admin_key
#* @param db_table_name
#* @param search_var
#* @param search_val
#' @get /retrieve_db_table
retrieve_db_table <- function(res, admin_key, db_table_name, search_var = "", search_val = ""){
  if (base::missing(db_table_name)) {
    return(json_error(res, 404, "Missing required parameter(s): db_table_name"))
  }

  admin_error <- require_admin_key(res, admin_key)
  if (!is.null(admin_error)) {
    return(admin_error)
  }

  db_table_name <- base::trimws(db_table_name[1])
  filter_coln_var <- if (search_var[1] %in% c(NA, "")) NULL else base::trimws(search_var[1])
  filter_coln_val <- if (search_val[1] %in% c(NA, "")) NULL else base::trimws(base::strsplit(search_val[1], ",", fixed = TRUE)[[1]])

  if (base::length(filter_coln_var) > 0 && base::length(filter_coln_val) > 0) {
    base::names(filter_coln_val) <- filter_coln_var
  }

  base::tryCatch({
    conn <- db_connect_local()

    print(base::sprintf("Check if '%s' table exists in the database", db_table_name))
    SigRepo::checkDBTable(conn = conn, db_table_name = db_table_name)

    print(base::sprintf("Check if values exist in '%s' table of the database", db_table_name))
    table_result <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = db_table_name,
      filter_coln_var = filter_coln_var,
      filter_coln_val = filter_coln_val
    )

    base::suppressWarnings(DBI::dbDisconnect(conn))

    if (base::nrow(table_result) > 0) {
      json_response(res, 200, table_result)
    } else {
      json_response(res, 200, base::data.frame(MESSAGES = "There are no values returned from the search parameters.\n"))
    }
  }, error = function(err){
    print(err)
    json_error(res, 500, base::sprintf("ERROR: %s", err))
  })
}

#* Store difexp in the database
#* @parser multi
#* @parser rds
#* @param api_key
#* @param signature_hashkey
#* @param difexp:file
#' @post /store_difexp
store_difexp <- function(res, api_key, signature_hashkey, difexp){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (base::missing(signature_hashkey) || base::trimws(signature_hashkey[1]) %in% c(NA, "")) {
    return(json_error(res, 404, "signature_hashkey cannot be empty."))
  }
  signature_hashkey <- base::trimws(signature_hashkey[1])

  if (!save_difexp_rds(difexp_dir, signature_hashkey, difexp[[1]])) {
    return(jsonlite::toJSON(base::data.frame(MESSAGES = "difexp is not a valid file."), pretty = TRUE))
  }
}

#* Get difexp from the database
#* @param api_key
#* @param signature_hashkey
#' @get /get_difexp
get_difexp <- function(res, api_key, signature_hashkey){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (base::missing(signature_hashkey) || base::trimws(signature_hashkey[1]) %in% c(NA, "")) {
    return(json_error(res, 404, "signature_hashkey cannot be empty."))
  }
  signature_hashkey <- base::trimws(signature_hashkey[1])

  difexp <- load_difexp_rds(difexp_dir, signature_hashkey)
  if (!is.null(difexp)) {
    return(jsonlite::toJSON(difexp, pretty = TRUE))
  }

  jsonlite::toJSON(
    base::data.frame(MESSAGES = base::sprintf("There is no difexp file found for signature_hashkey = '%s'", signature_hashkey)),
    pretty = TRUE
  )
}

#* Return read-only signature metadata and feature context
#* @parser json
#* @param api_key
#* @param signature_hashkey
#* @param include_features
#* @param max_features
#' @post /read/signature_context
read_signature_context <- function(req, res, api_key = "", signature_hashkey = "", include_features = "true", max_features = 50) {

  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  signature_hashkey <- if (identical(json_scalar(signature_hashkey), "")) json_scalar(body$signature_hashkey) else json_scalar(signature_hashkey)
  include_features <- if (is.null(body$include_features)) include_features else body$include_features
  max_features <- if (is.null(body$max_features)) max_features else body$max_features

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (identical(signature_hashkey, "")) {
    return(json_error(res, 404, "signature_hashkey cannot be empty."))
  }

  include_features <- normalize_flag(include_features, default = TRUE) == 1
  max_features <- base::suppressWarnings(base::as.integer(max_features[1]))
  if (base::is.na(max_features) || max_features < 1) {
    max_features <- 50
  }

  base::tryCatch({
    context <- fetch_signature_context(
      signature_hashkey = signature_hashkey,
      include_features = include_features,
      max_features = max_features,
      auth = auth
    )

    if (is.null(context)) {
      return(json_error(res, 404, base::sprintf("No signature found for signature_hashkey = '%s'.", signature_hashkey)))
    }

    json_response(res, payload = base::list(
      endpoint = "/read/signature_context",
      user_name = auth$user_name,
      signature_hashkey = signature_hashkey,
      context = context
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Read-only signature context failed: %s", err$message))
  })
}

#* Group signatures by deterministic feature overlap
#* @parser json
#* @param api_key
#* @param signature_hashkeys
#* @param max_features
#* @param similarity_threshold
#' @post /read/group_signatures
read_group_signatures <- function(req, res, api_key = "", signature_hashkeys = "", max_features = 200, similarity_threshold = 0.10) {

  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  signature_hashkeys <- if (identical(json_scalar(signature_hashkeys), "")) {
    json_vector(body$signature_hashkeys)
  } else {
    json_vector(base::strsplit(signature_hashkeys, ",", fixed = TRUE)[[1]])
  }
  max_features <- if (is.null(body$max_features)) max_features else body$max_features
  similarity_threshold <- if (is.null(body$similarity_threshold)) similarity_threshold else body$similarity_threshold

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (base::length(signature_hashkeys) < 2) {
    return(json_error(res, 404, "Provide at least two signature_hashkeys."))
  }

  max_features <- base::suppressWarnings(base::as.integer(max_features[1]))
  if (base::is.na(max_features) || max_features < 1) {
    max_features <- 200
  }

  similarity_threshold <- base::suppressWarnings(base::as.numeric(similarity_threshold[1]))
  if (base::is.na(similarity_threshold) || similarity_threshold < 0 || similarity_threshold > 1) {
    similarity_threshold <- 0.10
  }

  base::tryCatch({
    contexts <- fetch_signature_contexts(
      signature_hashkeys = signature_hashkeys,
      include_features = TRUE,
      max_features = max_features,
      auth = auth
    )

    missing_hashkeys <- base::setdiff(signature_hashkeys, base::names(contexts))
    if (base::length(contexts) < 2) {
      return(json_error(res, 404, "Fewer than two requested signatures could be found."))
    }

    similarity_tbl <- signature_similarity_summary(contexts)
    groups <- draft_signature_groups(similarity_tbl, threshold = similarity_threshold)

    json_response(res, payload = base::list(
      endpoint = "/read/group_signatures",
      user_name = auth$user_name,
      requested_signature_hashkeys = signature_hashkeys,
      missing_signature_hashkeys = missing_hashkeys,
      similarity_threshold = similarity_threshold,
      groups = groups,
      similarity = similarity_tbl,
      signatures = base::lapply(contexts, function(x) x$signature)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Read-only signature grouping failed: %s", err$message))
  })
}

#* Compare multiple signatures (overlap / KS) via OmicSignature::compare_omic_signatures
#* @param api_key
#* @param signature_hashkeys
#* @param method
#* @param score_cutoff
#* @param adj_p_cutoff
#* @param min_features
#* @param reference_hashkeys
#* @param max_feature
#* @param label_pairing
#* @param label_pairing2
#* @param adjust
#* @param gsea_score
#' @post /signatures/compare
compare_signatures_route <- function(req, res, api_key = "", signature_hashkeys = "",
                                     method = "overlap", score_cutoff = 0,
                                     adj_p_cutoff = 0.05, min_features = 5,
                                     max_feature = 500) {

  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  # The React client POSTs signature_hashkeys as a JSON array. Read it straight
  # from the parsed body first: plumber also binds that array onto the
  # signature_hashkeys parameter, so testing the parameter with json_scalar()
  # (which only sees its first element) would wrongly take the comma-split
  # branch and keep a single hashkey. The query-string branch is a fallback for
  # non-JSON callers (?signature_hashkeys=a,b).
  signature_hashkeys <- if (!base::is.null(body$signature_hashkeys)) {
    json_vector(body$signature_hashkeys)
  } else {
    json_vector(base::strsplit(json_scalar(signature_hashkeys), ",", fixed = TRUE)[[1]])
  }
  # Optional second list: a two-list (query vs reference) comparison. Same
  # array-binding caveat as signature_hashkeys above.
  reference_hashkeys <- if (!base::is.null(body$reference_hashkeys)) {
    json_vector(body$reference_hashkeys)
  } else {
    base::character()
  }
  method <- if (is.null(body$method)) json_scalar(method) else json_scalar(body$method)
  score_cutoff <- if (is.null(body$score_cutoff)) score_cutoff else body$score_cutoff
  adj_p_cutoff <- if (is.null(body$adj_p_cutoff)) adj_p_cutoff else body$adj_p_cutoff
  min_features <- if (is.null(body$min_features)) min_features else body$min_features
  max_feature <- if (is.null(body$max_feature)) max_feature else body$max_feature
  # {hashkey|name: [level1, level2], ...}; translated to sig-name keys in compare.R.
  label_pairing <- body$label_pairing
  label_pairing2 <- body$label_pairing2
  adjust <- base::isTRUE(base::as.logical(json_scalar(body$adjust, "false")))
  gsea_score <- json_scalar(body$gsea_score, "NES")
  if (!gsea_score %in% c("NES", "ES")) gsea_score <- "NES"

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  two_list <- base::length(reference_hashkeys) > 0
  if (!two_list && base::length(signature_hashkeys) < 2) {
    return(json_error(res, 404, "Provide at least two signature_hashkeys, or a reference_hashkeys set to compare against."))
  }
  if (two_list && base::length(signature_hashkeys) < 1) {
    return(json_error(res, 404, "Provide at least one query signature_hashkey."))
  }

  score_cutoff <- base::suppressWarnings(base::as.numeric(score_cutoff[1]))
  if (base::is.na(score_cutoff) || score_cutoff < 0) score_cutoff <- 0
  adj_p_cutoff <- base::suppressWarnings(base::as.numeric(adj_p_cutoff[1]))
  if (base::is.na(adj_p_cutoff) || adj_p_cutoff < 0 || adj_p_cutoff > 1) adj_p_cutoff <- 0.05
  min_features <- base::suppressWarnings(base::as.integer(min_features[1]))
  if (base::is.na(min_features) || min_features < 3) min_features <- 5
  max_feature <- base::suppressWarnings(base::as.integer(max_feature[1]))
  if (base::is.na(max_feature) || max_feature < 10) max_feature <- 500

  base::tryCatch({
    payload <- compare_signatures_result(
      auth = auth,
      signature_hashkeys = signature_hashkeys,
      reference_hashkeys = reference_hashkeys,
      method = method,
      difexp_dir = difexp_dir,
      score_cutoff = score_cutoff,
      adj_p_cutoff = adj_p_cutoff,
      min_features = min_features,
      max_feature = max_feature,
      label_pairing = label_pairing,
      label_pairing2 = label_pairing2,
      adjust = adjust,
      gsea_score = gsea_score
    )
    json_response(res, 200, payload)
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Signature comparison failed: %s", err$message))
  })
}

#* GSEA leading-edge / enrichment-plot data for one geneset-vs-ranking pair
#* @param api_key
#* @param geneset_hashkey
#* @param ranking_hashkey
#* @param geneset_level
#* @param ranking_level
#* @param score_cutoff
#* @param adj_p_cutoff
#* @param min_features
#' @post /signatures/compare/leading_edge
compare_leading_edge_route <- function(req, res, api_key = "", geneset_hashkey = "", ranking_hashkey = "",
                                       geneset_level = 1, ranking_level = 1,
                                       score_cutoff = 0, adj_p_cutoff = 0.05, min_features = 5) {

  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  geneset_hashkey <- if (identical(json_scalar(geneset_hashkey), "")) json_scalar(body$geneset_hashkey) else json_scalar(geneset_hashkey)
  ranking_hashkey <- if (identical(json_scalar(ranking_hashkey), "")) json_scalar(body$ranking_hashkey) else json_scalar(ranking_hashkey)
  geneset_level <- if (is.null(body$geneset_level)) geneset_level else body$geneset_level
  ranking_level <- if (is.null(body$ranking_level)) ranking_level else body$ranking_level
  score_cutoff <- if (is.null(body$score_cutoff)) score_cutoff else body$score_cutoff
  adj_p_cutoff <- if (is.null(body$adj_p_cutoff)) adj_p_cutoff else body$adj_p_cutoff
  min_features <- if (is.null(body$min_features)) min_features else body$min_features

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (identical(geneset_hashkey, "") || identical(ranking_hashkey, "")) {
    return(json_error(res, 404, "Provide geneset_hashkey and ranking_hashkey."))
  }

  base::tryCatch({
    payload <- compare_leading_edge(
      auth = auth,
      geneset_hashkey = geneset_hashkey,
      ranking_hashkey = ranking_hashkey,
      geneset_level = geneset_level,
      ranking_level = ranking_level,
      difexp_dir = difexp_dir,
      score_cutoff = base::suppressWarnings(base::as.numeric(score_cutoff[1])),
      adj_p_cutoff = base::suppressWarnings(base::as.numeric(adj_p_cutoff[1])),
      min_features = base::suppressWarnings(base::as.integer(min_features[1]))
    )
    json_response(res, 200, payload)
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Leading-edge computation failed: %s", err$message))
  })
}

#* Delete difexp from the database
#* @param api_key
#* @param signature_hashkey
#' @delete /delete_difexp
delete_difexp <- function(res, api_key, signature_hashkey){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (base::missing(signature_hashkey) || base::trimws(signature_hashkey[1]) %in% c(NA, "")) {
    return(json_error(res, 404, "signature_hashkey cannot be empty."))
  }
  signature_hashkey <- base::trimws(signature_hashkey[1])

  delete_difexp_rds(difexp_dir, signature_hashkey)

  jsonlite::toJSON(
    base::data.frame(MESSAGES = base::sprintf("difexp file has been removed for signature_hashkey = '%s'", signature_hashkey)),
    pretty = TRUE
  )
}

#* Activate registered users in the database
#* @param user_name
#* @param api_key
#' @get /activate_user
activate_user <- function(res, user_name, api_key){
  if (base::missing(user_name) || base::missing(api_key)) {
    return(json_error(res, 404, "Missing required parameter(s): user_name, api_key"))
  }

  user_name <- base::trimws(user_name[1])
  api_key <- base::trimws(api_key[1])

  if (user_name %in% c(NA, "")) {
    return(json_error(res, 404, "user_name cannot be empty."))
  }

  check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
  if (base::nrow(check_user_tbl) == 0) {
    return(json_error(res, 404, base::sprintf("User = '%s' does not exist in our database. Please choose a different name.", user_name)))
  }

  if (api_key %in% "") {
    return(json_error(res, 404, "api_key cannot be empty."))
  } else if (!api_key %in% base::Sys.getenv("SENDMAIL_KEY")) {
    return(json_error(res, 404, "Invalid Sendmail API Key."))
  }

  mark_user_active(conn_handler, user_name)

  notify_res <- send_user_activation_email(user_name, api_key)

  if (notify_res$status_code != 200) {
    MESSAGES <- "Something went wrong with the API. Cannot activate user. Please contact admin for support."
  } else {
    MESSAGES <- base::sprintf("User = '%s' has been activated. A notified email has been sent to user.", user_name)
  }

  json_response(res, 200, base::data.frame(MESSAGES = MESSAGES))
}

#* Log in with a username and password; returns the account's api_key on success
#* @parser json
#* @param user_name
#* @param password
#' @post /login
login <- function(req, res, user_name = "", password = ""){
  body <- request_json_body(req)
  user_name <- if (identical(json_scalar(user_name), "")) json_scalar(body$user_name) else json_scalar(user_name)
  password <- if (identical(json_scalar(password), "")) json_scalar(body$password) else json_scalar(password)

  if (identical(user_name, "") || identical(password, "")) {
    return(json_error(res, 400, "user_name and password are required."))
  }

  # One generic 401 for unknown user / wrong password / inactive account --
  # authenticate_user() deliberately doesn't distinguish them.
  auth <- base::tryCatch(
    authenticate_user(user_name, password),
    error = function(err) NULL
  )

  if (is.null(auth)) {
    return(json_error(res, 401, "Invalid username or password."))
  }

  json_response(res, 200, payload = base::list(
    user_name = auth$user_name,
    user_role = auth$user_role,
    api_key = auth$api_key
  ))
}

#* Self-service registration, matching the Shiny portal's form. Creates the
#* account inactive and emails the administrator to approve it; the account
#* cannot log in until /activate_user runs. No api_key: the caller by
#* definition does not have one yet.
#* @parser json
#* @param user_name
#* @param password
#* @param user_email
#* @param user_first
#* @param user_last
#* @param user_affiliation
#' @post /register
register_route <- function(req, res, user_name = "", password = "", user_email = "",
                           user_first = "", user_last = "", user_affiliation = ""){
  body <- request_json_body(req)
  pick <- function(param, key) {
    if (identical(json_scalar(param), "")) json_scalar(body[[key]]) else json_scalar(param)
  }

  result <- register_new_user(
    user_name = pick(user_name, "user_name"),
    password = if (identical(json_scalar(password), "")) json_scalar(body$password) else json_scalar(password),
    user_email = pick(user_email, "user_email"),
    user_first = pick(user_first, "user_first"),
    user_last = pick(user_last, "user_last"),
    user_affiliation = pick(user_affiliation, "user_affiliation")
  )

  if (!base::isTRUE(result$ok)) {
    return(json_error(res, 400, result$reason))
  }

  json_response(res, 200, base::data.frame(MESSAGES = result$reason, stringsAsFactors = FALSE))
}

#* Request a temporary password. Takes either a username or the email on the
#* account. Always answers the same way whether or not the account exists, so
#* this cannot be used to find out who has one.
#* @parser json
#* @param identifier
#' @post /forgot_password
forgot_password_route <- function(req, res, identifier = ""){
  body <- request_json_body(req)
  identifier <- if (identical(json_scalar(identifier), "")) json_scalar(body$identifier) else json_scalar(identifier)

  result <- request_password_reset(identifier)

  if (!base::isTRUE(result$ok)) {
    return(json_error(res, 400, result$reason))
  }

  json_response(res, 200, base::data.frame(MESSAGES = result$reason, stringsAsFactors = FALSE))
}

#* Resolve the account name + role for an api_key. Used by the agent
#* skill-runner (agent/runner.py) to admin-gate the website assistant --
#* it never trusts the browser's own role claim, it re-checks here.
#* @parser json
#* @param api_key
#' @post /whoami
whoami <- function(req, res, api_key = ""){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  json_response(res, 200, payload = base::list(
    user_name = auth$user_name,
    user_role = auth$user_role
  ))
}

#* Enrich a gene set against Rummagene's ~1M literature-mined gene sets
#* (rummagene.com -- gene sets extracted from PMC supplementary tables). Accepts
#* an explicit `genes` list, or a `signature_hashkey` whose curated gene symbols
#* are resolved server-side (the same resolution over-representation enrichment
#* uses). Returns the matching published gene sets with their PMC links.
#* @parser json
#* @param api_key
#* @param genes
#* @param signature_hashkey
#* @param limit
#' @post /rummagene/enrich
rummagene_enrich_route <- function(req, res, api_key = "", genes = NULL, signature_hashkey = "", limit = 25){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  genes_vec <- if (!base::is.null(body$genes)) {
    json_vector(body$genes)
  } else if (!base::is.null(genes)) {
    json_vector(genes)
  } else {
    base::character()
  }
  genes_vec <- genes_vec[!genes_vec %in% c("", NA)]

  signature_name <- NULL
  hk <- if (identical(json_scalar(signature_hashkey), "")) json_scalar(body$signature_hashkey) else json_scalar(signature_hashkey)

  # No explicit genes but a signature was given -> resolve its curated gene
  # symbols. First try the reference-table path over-representation enrichment
  # uses; if that yields nothing (common for non-human signatures whose features
  # are Ensembl IDs with no gene_symbol in the reference tables), fall back to
  # the signature's difexp, which often carries symbols directly.
  if (base::length(genes_vec) == 0 && !identical(hk, "")) {
    resolved <- base::tryCatch(
      resolve_single_enrichment_query(auth, hk, "hypergeometric", difexp_dir),
      error = function(e) base::list(ok = FALSE, reason = "error", message = base::conditionMessage(e))
    )
    if (base::isTRUE(resolved$ok)) {
      genes_vec <- resolved$query
      signature_name <- resolved$signature_name
    } else {
      difexp_syms <- base::tryCatch(
        rummagene_signature_symbols_from_difexp(auth, hk, difexp_dir),
        error = function(e) NULL
      )
      if (!base::is.null(difexp_syms) && base::length(difexp_syms) >= 2) {
        genes_vec <- difexp_syms
        signature_name <- resolved$signature_name
      } else {
        reason <- resolved$reason %||% "unknown"
        msg <- base::switch(
          reason,
          no_gene_symbols = base::paste0(
            "No gene symbols are available for this signature. Its features are stored as ",
            "Ensembl/other IDs with no gene-symbol mapping in SigRepo's reference tables ",
            "(common for non-human signatures), and its difexp table did not provide symbols ",
            "either, so there is nothing to match against Rummagene, which is keyed on gene symbols."
          ),
          no_features = "This signature has no features recorded to enrich.",
          unsupported_assay_type = resolved$message %||%
            "Rummagene enrichment needs gene symbols, which this signature's assay type does not provide.",
          not_found = "Signature not found, or you do not have access to it.",
          resolved$message %||% base::sprintf("Could not resolve gene symbols for this signature (%s).", reason)
        )
        return(json_error(res, 422, msg))
      }
    }
  }

  if (base::length(genes_vec) < 2) {
    return(json_error(res, 400, "Provide at least two gene symbols, or a signature_hashkey with resolvable gene symbols."))
  }

  result <- base::tryCatch(
    rummagene_enrich(genes = genes_vec, limit = base::as.integer(json_scalar(limit, "25"))),
    error = function(e) e
  )
  if (base::inherits(result, "error")) {
    return(json_error(res, 502, base::sprintf("Rummagene enrichment failed: %s", base::conditionMessage(result))))
  }

  result$signature_name <- signature_name
  json_response(res, 200, payload = result)
}


#* Find signatures by the genes they contain -- the reverse of
#* /rummagene/enrich. Give it gene symbols directly, or a signature_hashkey to
#* use that signature's own genes (which is how the "related signatures" panel
#* works, with the source signature excluded from its own results).
#* @parser json
#* @param api_key
#* @param genes
#* @param signature_hashkey
#* @param limit
#* @param min_overlap
#' @post /signatures/search_by_genes
search_by_genes_route <- function(req, res, api_key = "", genes = NULL, signature_hashkey = "",
                                  limit = 20, min_overlap = 1){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  genes_vec <- if (!base::is.null(body$genes)) {
    json_vector(body$genes)
  } else if (!base::is.null(genes)) {
    json_vector(genes)
  } else {
    base::character()
  }

  hk <- if (identical(json_scalar(signature_hashkey), "")) {
    json_scalar(body$signature_hashkey)
  } else {
    json_scalar(signature_hashkey)
  }

  # Given a signature and no genes, resolve its symbols the same way the
  # Rummagene route does -- reference tables first, then the difexp fallback for
  # signatures stored as Ensembl/other IDs. Sharing the resolution keeps the two
  # gene-based features from disagreeing about what a signature's genes are.
  source_name <- NULL
  if (base::length(genes_vec) == 0 && !identical(hk, "")) {
    resolved <- base::tryCatch(
      resolve_single_enrichment_query(auth, hk, "hypergeometric", difexp_dir),
      error = function(e) base::list(ok = FALSE, reason = "error")
    )
    if (base::isTRUE(resolved$ok)) {
      genes_vec <- resolved$query
      source_name <- resolved$signature_name
    } else {
      difexp_syms <- base::tryCatch(
        rummagene_signature_symbols_from_difexp(auth, hk, difexp_dir),
        error = function(e) NULL
      )
      if (!base::is.null(difexp_syms) && base::length(difexp_syms) > 0) {
        genes_vec <- difexp_syms
        source_name <- resolved$signature_name
      } else {
        return(json_error(res, 422, base::paste0(
          "Could not resolve gene symbols for this signature. Its features are stored as ",
          "Ensembl/other IDs with no gene-symbol mapping in SigRepo's reference tables, and ",
          "its difexp table did not provide symbols either."
        )))
      }
    }
  }

  if (base::length(genes_vec) == 0) {
    return(json_error(res, 400, "Provide gene symbols, or a signature_hashkey whose genes can be resolved."))
  }

  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  hits <- base::tryCatch(
    search_signatures_by_genes(
      conn = conn,
      genes = genes_vec,
      limit = base::as.integer(json_scalar(limit, "20")),
      min_overlap = base::as.integer(json_scalar(min_overlap, "1")),
      # A signature is never a hit for its own genes.
      exclude_hashkey = if (identical(hk, "")) NULL else hk,
      is_admin = identical(auth$user_role, "admin")
    ),
    error = function(e) e
  )
  if (base::inherits(hits, "error")) {
    return(json_error(res, 500, base::sprintf("Gene search failed: %s", base::conditionMessage(hits))))
  }

  json_response(res, 200, payload = base::list(
    query_size = base::length(genes_vec),
    source_signature = source_name,
    total = base::nrow(hits),
    hits = compact_table(hits, max_rows = 100)
  ))
}

#* Running-enrichment curve and leading edge for one gene set against one
#* signature. hypeR reports a row per gene set but nothing about WHERE in the
#* ranking the hits fell, which is the part people interpret -- so the curve is
#* computed here (api/lib/gsea_curve.R) rather than returned by hypeR.
#*
#* Kept as its own call rather than embedded in /annotate/run: a run returns up
#* to hundreds of gene sets and only the one a reader clicks needs a curve.
#* @parser json
#* @param api_key
#* @param signature_hashkey
#* @param geneset_label
#* @param species
#* @param collection
#* @param subcollection
#* @param power
#' @post /annotate/leading_edge
annotate_leading_edge_route <- function(req, res, api_key = "", signature_hashkey = "", geneset_label = "",
                                        species = "Homo sapiens", collection = "", subcollection = "", power = 1){
  body <- request_json_body(req)
  pick <- function(param, key, default = "") {
    v <- if (identical(json_scalar(param), "")) json_scalar(body[[key]]) else json_scalar(param)
    if (identical(v, "")) default else v
  }

  auth <- validate_api_key(res, pick(api_key, "api_key"))
  if (is_json_error(auth)) {
    return(auth)
  }

  hk <- pick(signature_hashkey, "signature_hashkey")
  label <- pick(geneset_label, "geneset_label")
  if (identical(hk, "") || identical(label, "")) {
    return(json_error(res, 400, "signature_hashkey and geneset_label are both required."))
  }

  # The ranked query, resolved exactly the way a gsea run resolves it, so the
  # curve describes the same ranking the run scored.
  resolved <- base::tryCatch(
    resolve_single_enrichment_query(auth, hk, "gsea", difexp_dir),
    error = function(e) base::list(ok = FALSE, reason = "error", message = base::conditionMessage(e))
  )
  if (!base::isTRUE(resolved$ok)) {
    status <- if (identical(resolved$reason, "not_found")) 404L else 422L
    return(json_error(res, status, resolved$message %||%
      base::sprintf("Could not resolve a ranked signature for this run (%s).", resolved$reason %||% "unknown")))
  }

  geneset_result <- resolve_msigdb_genesets(
    msigdb_cache_dir,
    pick(species, "species", "Homo sapiens"),
    pick(collection, "collection"),
    pick(subcollection, "subcollection")
  )
  if (!base::isTRUE(geneset_result$ok)) {
    return(json_error(res, 404, geneset_result$message %||% "Gene sets are not available for that selection."))
  }

  genes <- geneset_result$genesets[[label]]
  if (base::is.null(genes)) {
    return(json_error(res, 404, base::sprintf("'%s' is not in the selected gene set collection.", label)))
  }

  pw <- base::suppressWarnings(base::as.numeric(json_scalar(power, "1")))
  if (base::is.na(pw) || pw < 0) pw <- 1

  curve <- compute_gsea_curve(resolved$query, genes, power = pw)
  if (base::is.null(curve)) {
    return(json_error(res, 422, base::sprintf("'%s' does not overlap this signature's ranked genes.", label)))
  }

  json_response(res, 200, payload = base::list(
    geneset_label = label,
    signature_name = resolved$signature_name,
    n_total = curve$n_total,
    es_score = curve$es_score,
    es_index = curve$es_index,
    es_direction = curve$es_direction,
    n_leading = curve$n_leading,
    leading_edge_genes = curve$leading_edge_genes,
    hit_positions = curve$hit_positions,
    curve = curve$curve
  ))
}
#* Distinct organism/phenotype/sample_type/platform/assay_type values currently in use
#* @param api_key
#' @get /vocabulary
vocabulary <- function(res, api_key = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    conn <- db_connect_local()
    vocab <- list_vocabulary(conn)
    base::suppressWarnings(DBI::dbDisconnect(conn))
    json_response(res, 200, payload = vocab)
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Vocabulary lookup failed: %s", err$message))
  })
}

#* Repository-wide summary stats for the web frontend's Dashboard/Insights
#* page (totals, signature counts by organism/assay/contributor, and the
#* most recently created signatures).
#* @param api_key
#* @param recent_limit
#' @get /insights
insights_route <- function(res, api_key = "", recent_limit = 5){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    conn <- db_connect_local()
    result <- repository_insights(conn = conn, is_admin = identical(auth$user_role, "admin"), recent_limit = recent_limit)
    base::suppressWarnings(DBI::dbDisconnect(conn))

    json_response(res, 200, payload = base::list(
      total_signatures = result$total_signatures,
      total_users = result$total_users,
      total_organisms = result$total_organisms,
      total_assays = result$total_assays,
      by_organism = compact_table(result$by_organism),
      by_assay = compact_table(result$by_assay),
      top_contributors = compact_table(result$top_contributors),
      recent_signatures = compact_table(result$recent_signatures)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Insights lookup failed: %s", err$message))
  })
}

#* Search signatures by organism/phenotype/assay_type/keyword
#* @param api_key
#* @param organism
#* @param phenotype
#* @param assay_type
#* @param keyword
#* @param limit
#* @param offset
#* @param sort_by One of signature_name, organism, assay_type, direction_type,
#*   phenotype, sample_type, platform_name, year, user_name, visibility.
#*   Anything else falls back to signature_name.
#* @param sort_dir asc (default) or desc
#' @get /signatures/search
search_signatures_route <- function(res, api_key = "", organism = "", phenotype = "", assay_type = "", keyword = "", limit = 20, offset = 0, sort_by = "", sort_dir = "asc"){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    conn <- db_connect_local()
    result <- search_signatures(
      conn = conn,
      organism = json_scalar(organism),
      phenotype = json_scalar(phenotype),
      assay_type = json_scalar(assay_type),
      keyword = json_scalar(keyword),
      limit = limit,
      offset = offset,
      sort_by = json_scalar(sort_by),
      sort_dir = json_scalar(sort_dir, "asc"),
      is_admin = identical(auth$user_role, "admin")
    )
    base::suppressWarnings(DBI::dbDisconnect(conn))
    # `count` is the TOTAL number of matching rows (for pagination), not the
    # size of this page. `signatures` is just the requested page.
    json_response(res, 200, payload = base::list(
      count = result$total,
      limit = base::as.integer(limit),
      offset = base::as.integer(offset),
      signatures = compact_table(result$rows, max_rows = 100)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Signature search failed: %s", err$message))
  })
}

#* Delete a signature (editor/admin, and owner unless admin)
#* @param api_key
#* @param signature_hashkey
#' @delete /signatures/delete
delete_signature_route <- function(res, api_key = "", signature_hashkey = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  signature_hashkey <- json_scalar(signature_hashkey)
  if (identical(signature_hashkey, "")) {
    return(json_error(res, 404, "signature_hashkey cannot be empty."))
  }

  base::tryCatch({
    result <- delete_signature(auth = auth, signature_hashkey = signature_hashkey)

    if (!result$ok && identical(result$reason, "not_found")) {
      return(json_error(res, 404, base::sprintf("No signature found for signature_hashkey = '%s'.", signature_hashkey)))
    }

    if (!result$ok && identical(result$reason, "forbidden")) {
      return(json_error(res, 403, "You do not have permission to delete this signature."))
    }

    json_response(res, 200, payload = base::list(
      MESSAGES = base::sprintf("Signature '%s' has been deleted.", result$signature_name)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Signature delete failed: %s", err$message))
  })
}

#* Search collections visible to the caller
#* @param api_key
#* @param keyword
#* @param limit
#' @get /collections/search
search_collections_route <- function(res, api_key = "", keyword = "", limit = 50){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    conn <- db_connect_local()
    results <- search_collections(conn = conn, auth = auth, keyword = json_scalar(keyword), limit = limit)
    base::suppressWarnings(DBI::dbDisconnect(conn))
    json_response(res, 200, payload = base::list(
      count = base::nrow(results),
      collections = compact_table(results, max_rows = 200)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Collection search failed: %s", err$message))
  })
}

#* Collection metadata plus its member signatures
#* @parser json
#* @param api_key
#* @param collection_hashkey
#' @post /collections/detail
collection_detail_route <- function(req, res, api_key = "", collection_hashkey = ""){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  collection_hashkey <- if (identical(json_scalar(collection_hashkey), "")) json_scalar(body$collection_hashkey) else json_scalar(collection_hashkey)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }
  if (identical(collection_hashkey, "")) {
    return(json_error(res, 404, "collection_hashkey cannot be empty."))
  }

  base::tryCatch({
    result <- get_collection_detail(auth = auth, collection_hashkey = collection_hashkey)

    if (!result$ok && identical(result$reason, "not_found")) {
      return(json_error(res, 404, base::sprintf("No collection found for collection_hashkey = '%s'.", collection_hashkey)))
    }
    if (!result$ok && identical(result$reason, "forbidden")) {
      return(json_error(res, 403, "You do not have permission to view this collection."))
    }

    json_response(res, 200, payload = base::list(
      collection = result$collection,
      signatures = result$signatures
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Collection detail lookup failed: %s", err$message))
  })
}

#* Create a collection owned by the caller (editor/admin)
#* @parser json
#* @param api_key
#* @param collection_name
#* @param description
#* @param visibility
#' @post /collections/create
create_collection_route <- function(req, res, api_key = "", collection_name = "", description = "", visibility = "false"){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  collection_name <- if (identical(json_scalar(collection_name), "")) json_scalar(body$collection_name) else json_scalar(collection_name)
  description <- if (identical(json_scalar(description), "")) json_scalar(body$description) else json_scalar(description)
  visibility <- if (is.null(body$visibility)) visibility else body$visibility

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    result <- create_collection(
      auth = auth,
      collection_name = collection_name,
      description = description,
      visibility = normalize_flag(visibility, default = FALSE) == 1
    )

    if (!result$ok && identical(result$reason, "forbidden")) {
      return(json_error(res, 403, "You do not have permission to create a collection."))
    }
    if (!result$ok && identical(result$reason, "invalid")) {
      return(json_error(res, 400, result$message))
    }
    if (!result$ok && identical(result$reason, "duplicate")) {
      return(json_error(res, 409, result$message))
    }

    json_response(res, 200, payload = base::list(collection_hashkey = result$collection_hashkey))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Collection create failed: %s", err$message))
  })
}

#* Delete a collection (editor/admin, and owner unless admin)
#* @param api_key
#* @param collection_hashkey
#' @delete /collections/delete
delete_collection_route <- function(res, api_key = "", collection_hashkey = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  collection_hashkey <- json_scalar(collection_hashkey)
  if (identical(collection_hashkey, "")) {
    return(json_error(res, 404, "collection_hashkey cannot be empty."))
  }

  base::tryCatch({
    result <- delete_collection_by_hashkey(auth = auth, collection_hashkey = collection_hashkey)

    if (!result$ok && identical(result$reason, "not_found")) {
      return(json_error(res, 404, base::sprintf("No collection found for collection_hashkey = '%s'.", collection_hashkey)))
    }
    if (!result$ok && identical(result$reason, "forbidden")) {
      return(json_error(res, 403, "You do not have permission to delete this collection."))
    }

    json_response(res, 200, payload = base::list(
      MESSAGES = base::sprintf("Collection '%s' has been deleted.", result$collection_name)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Collection delete failed: %s", err$message))
  })
}

#* Add a signature to a collection (editor/admin, with access to both)
#* @parser json
#* @param api_key
#* @param collection_hashkey
#* @param signature_hashkey
#' @post /collections/signatures/add
add_signature_to_collection_route <- function(req, res, api_key = "", collection_hashkey = "", signature_hashkey = ""){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  collection_hashkey <- if (identical(json_scalar(collection_hashkey), "")) json_scalar(body$collection_hashkey) else json_scalar(collection_hashkey)
  signature_hashkey <- if (identical(json_scalar(signature_hashkey), "")) json_scalar(body$signature_hashkey) else json_scalar(signature_hashkey)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }
  if (identical(collection_hashkey, "") || identical(signature_hashkey, "")) {
    return(json_error(res, 404, "collection_hashkey and signature_hashkey are required."))
  }

  base::tryCatch({
    result <- add_signature_to_collection(auth = auth, collection_hashkey = collection_hashkey, signature_hashkey = signature_hashkey)
    collection_signature_error_response(res, result, collection_hashkey, signature_hashkey)
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Adding signature to collection failed: %s", err$message))
  })
}

#* Remove a signature from a collection (editor/admin, with access to both)
#* @param api_key
#* @param collection_hashkey
#* @param signature_hashkey
#' @delete /collections/signatures/remove
remove_signature_from_collection_route <- function(res, api_key = "", collection_hashkey = "", signature_hashkey = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }
  collection_hashkey <- json_scalar(collection_hashkey)
  signature_hashkey <- json_scalar(signature_hashkey)
  if (identical(collection_hashkey, "") || identical(signature_hashkey, "")) {
    return(json_error(res, 404, "collection_hashkey and signature_hashkey are required."))
  }

  base::tryCatch({
    result <- remove_signature_from_collection(auth = auth, collection_hashkey = collection_hashkey, signature_hashkey = signature_hashkey)
    collection_signature_error_response(res, result, collection_hashkey, signature_hashkey)
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Removing signature from collection failed: %s", err$message))
  })
}

#* MSigDB species options for the Annotate picker (matches the Shiny app's
#* species picker; static/local, no network)
#* @param api_key
#' @get /annotate/msigdb-species
msigdb_species_route <- function(res, api_key = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    json_response(res, 200, payload = base::list(species = msigdb_species_options()))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Could not list MSigDB species: %s", err$message))
  })
}

#* MSigDB collection/subcollection options for the Annotate picker (the
#* fixed Collection/Subcollection matrix, with human-readable labels --
#* matches the Shiny app's picker, see api/lib/msigdb_cache.R)
#* @param api_key
#' @get /annotate/msigdb-collections
msigdb_collections_route <- function(res, api_key = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    json_response(res, 200, payload = base::list(collections = msigdb_collection_metadata()))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Could not list MSigDB collections: %s", err$message))
  })
}

#* Resolve (from cache, or live if MSIGDB_ALLOW_RUNTIME_FETCH) a gene set
#* collection ahead of running enrichment, mirroring the Shiny app's
#* separate "Fetch Genesets" step
#* @parser json
#* @param api_key
#* @param species
#* @param collection
#* @param subcollection
#' @post /annotate/genesets
annotate_genesets_route <- function(req, res, api_key = "", species = "Homo sapiens", collection = "H", subcollection = ""){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  species <- if (is.null(body$species)) species else json_scalar(body$species)
  collection <- if (is.null(body$collection)) collection else json_scalar(body$collection)
  subcollection <- if (is.null(body$subcollection)) subcollection else json_scalar(body$subcollection)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    result <- resolve_msigdb_genesets(msigdb_cache_dir, species, collection, subcollection)
    if (!result$ok) {
      return(json_error(res, 404, result$message))
    }
    json_response(res, 200, payload = base::list(
      n_genesets = base::length(result$genesets),
      source = result$source
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Could not resolve gene sets: %s", err$message))
  })
}

#* Run gene set enrichment for one or more signatures at once (hypeR's own
#* multi-signature support) against an MSigDB collection
#* @parser json
#* @param api_key
#* @param signature_hashkeys
#* @param test
#* @param species
#* @param collection
#* @param subcollection
#* @param fdr
#' @post /annotate/run
annotate_run_route <- function(req, res, api_key = "", signature_hashkeys = "", test = "hypergeometric",
                                species = "Homo sapiens", collection = "H", subcollection = "", fdr = 0.05){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  # Accepts either signature_hashkeys (array) or the older singular
  # signature_hashkey, so a single-signature run still works either way.
  signature_hashkeys <- if (base::is.null(body$signature_hashkeys)) {
    json_vector(signature_hashkeys)
  } else {
    json_vector(body$signature_hashkeys)
  }
  if (base::length(signature_hashkeys) == 0) {
    singular <- if (base::is.null(body$signature_hashkey)) NULL else json_scalar(body$signature_hashkey)
    if (!base::is.null(singular) && base::nzchar(singular)) {
      signature_hashkeys <- singular
    }
  }
  test <- if (identical(json_scalar(test), "")) "hypergeometric" else json_scalar(test)
  species <- if (is.null(body$species)) species else json_scalar(body$species)
  collection <- if (is.null(body$collection)) collection else json_scalar(body$collection)
  subcollection <- if (is.null(body$subcollection)) subcollection else json_scalar(body$subcollection)
  fdr <- if (is.null(body$fdr)) fdr else body$fdr

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }
  if (base::length(signature_hashkeys) == 0) {
    return(json_error(res, 404, "signature_hashkeys cannot be empty."))
  }
  if (!test %in% c("hypergeometric", "kstest", "gsea", "gem_hypergeo", "gem_weighted")) {
    return(json_error(res, 400, "test must be 'hypergeometric', 'kstest', 'gsea', 'gem_hypergeo' or 'gem_weighted'."))
  }

  fdr <- base::suppressWarnings(base::as.numeric(fdr[1]))
  if (base::is.na(fdr) || fdr <= 0 || fdr > 1) {
    fdr <- 0.05
  }

  # GEM is a different pipeline, not a hypeR variant: hypeR.GEM maps
  # metabolites to enzyme-coding genes through a genome-scale metabolic model
  # before enriching, so it takes the OmicSignature rather than a resolved gene
  # vector. It also runs one signature at a time.
  if (is_gem_test(test)) {
    gem <- run_gem_enrichment(
      auth = auth,
      signature_hashkey = signature_hashkeys[1],
      test = test,
      difexp_dir = difexp_dir,
      msigdb_cache_dir = msigdb_cache_dir,
      species = species,
      collection = collection,
      subcollection = subcollection,
      directional = normalize_flag(body$gem_directional, default = TRUE) == 1L,
      reference_key = { rk <- json_scalar(body$gem_reference_key); if (base::identical(rk, "")) NULL else rk },
      fdr = fdr
    )
    if (!base::isTRUE(gem$ok)) {
      status <- base::switch(
        gem$reason %||% "",
        unavailable = 503L,
        not_found = 404L,
        gem_failed = 502L,
        422L
      )
      return(json_error(res, status, gem$message %||% "GEM enrichment could not run."))
    }
    # Same envelope as the hypeR path below, so the UI has one result handler.
    # GEM adds reference_key / gem_method / the mapping counts. Neither path
    # embeds a dot plot in this response -- that figure lives behind GET
    # /annotate/dotplot for the hypeR path, and GEM has no equivalent at all
    # (hypeR.GEM returns plain tables, not a hypeR object to render one from).
    return(json_response(res, 200, payload = base::list(
      test = test,
      collection = collection,
      subcollection = subcollection,
      fdr = fdr,
      geneset_source = gem$geneset_source,
      reference_key = gem$reference_key,
      gem_method = gem$method,
      n_metabolites = gem$n_metabolites,
      n_genes = gem$n_genes,
      # Same per-signature envelope as the hypeR path. GEM runs one signature,
      # so this is always a single entry; `info` mirrors hyp$info's role of
      # recording what the run actually did.
      signatures = base::list(base::list(
        signature_hashkey = signature_hashkeys[1],
        signature_name = gem$signature_name,
        label = gem$signature_name,
        n_query = gem$n_metabolites,
        n_enriched = base::length(gem$results),
        info = base::list(
          "Metabolites" = base::as.character(gem$n_metabolites),
          "Mapped Genes" = base::as.character(gem$n_genes),
          "Reference Key" = base::as.character(gem$reference_key),
          "Method" = base::as.character(gem$method),
          "Genesets" = base::as.character(collection),
          "FDR" = base::as.character(fdr)
        ),
        results = gem$results
      )),
      skipped = base::list()
    )))
  }

  base::tryCatch({
    result <- run_enrichment(
      auth = auth,
      signature_hashkeys = signature_hashkeys,
      test = test,
      species = species,
      collection = collection,
      subcollection = if (identical(subcollection, "")) NULL else subcollection,
      fdr = fdr,
      difexp_dir = difexp_dir,
      msigdb_cache_dir = msigdb_cache_dir
    )

    if (!result$ok) {
      err <- enrichment_error_response(result$reason, result$message, test = test)
      return(json_error(res, err$status, err$message))
    }

    json_response(res, 200, payload = base::list(
      test = test,
      collection = collection,
      subcollection = subcollection,
      fdr = fdr,
      geneset_source = result$geneset_source,
      # One entry per signature, each with its own hyp$info and results --
      # the multihyp shape, rather than one interleaved table.
      signatures = result$signatures,
      skipped = result$skipped
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Enrichment failed: %s", err$message))
  })
}

#* Download hypeR's own dot plot for an enrichment run as a PNG
#* @param api_key
#* @param signature_hashkeys
#* @param test
#* @param species
#* @param collection
#* @param subcollection
#* @param fdr
#' @get /annotate/dotplot
annotate_dotplot_route <- function(res, api_key = "", signature_hashkeys = "", test = "hypergeometric",
                                   species = "Homo sapiens", collection = "H", subcollection = "", fdr = 0.05){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  hashkeys <- base::unlist(base::strsplit(json_scalar(signature_hashkeys), ",", fixed = TRUE))
  hashkeys <- base::trimws(hashkeys)
  hashkeys <- hashkeys[base::nzchar(hashkeys)]
  if (base::length(hashkeys) == 0) {
    return(json_error(res, 400, "signature_hashkeys is required."))
  }

  test <- json_scalar(test)
  if (!test %in% c("hypergeometric", "kstest", "gsea")) {
    return(json_error(res, 400, "The dot plot is a hypeR figure; test must be 'hypergeometric', 'kstest' or 'gsea'."))
  }

  fdr <- base::suppressWarnings(base::as.numeric(fdr[1]))
  if (base::is.na(fdr) || fdr <= 0 || fdr > 1) {
    fdr <- 0.05
  }

  base::tryCatch({
    built <- run_enrichment_hyp_object(
      auth = auth, signature_hashkeys = hashkeys, test = test,
      species = json_scalar(species), collection = json_scalar(collection),
      subcollection = { sc <- json_scalar(subcollection); if (base::identical(sc, "")) NULL else sc },
      fdr = fdr, difexp_dir = difexp_dir, msigdb_cache_dir = msigdb_cache_dir
    )
    if (!base::isTRUE(built$ok)) {
      err <- enrichment_error_response(built$reason, built$message, test = test)
      return(json_error(res, err$status, err$message))
    }

    uri <- render_hyp_dots_png(built$hyp, fdr)
    if (base::is.null(uri)) {
      return(json_error(res, 502, "hypeR could not render a dot plot for this run."))
    }

    raw_bytes <- jsonlite::base64_dec(base::sub("^data:image/png;base64,", "", uri))
    res$serializer <- plumber::serializer_content_type("image/png")
    plumber::as_attachment(raw_bytes, filename = "enrichment_dotplot.png")
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Dot plot failed: %s", err$message))
  })
}

#* Download a single signature as an RDS file (metadata + features + difexp)
#* @param api_key
#* @param signature_hashkey
#' @get /signatures/export
signature_export_route <- function(res, api_key = "", signature_hashkey = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  signature_hashkey <- json_scalar(signature_hashkey)
  if (identical(signature_hashkey, "")) {
    return(json_error(res, 404, "signature_hashkey cannot be empty."))
  }

  base::tryCatch({
    result <- build_signature_export(auth, signature_hashkey, difexp_dir)
    if (!result$ok) {
      return(json_error(res, 404, base::sprintf("No signature found for signature_hashkey = '%s'.", signature_hashkey)))
    }

    res$serializer <- serializers[["rds"]]
    plumber::as_attachment(
      result$export,
      filename = base::sprintf("signature_%s.rds", export_safe_filename(result$signature_name))
    )
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Signature export failed: %s", err$message))
  })
}

#* Download a zip of RDS exports for a basket of signatures (skips any the
#* caller can no longer see; check the X-Basket-Included/X-Basket-Skipped
#* response headers)
#* @parser json
#* @param api_key
#* @param signature_hashkeys
#' @post /signatures/export-batch
signature_export_batch_route <- function(req, res, api_key = "", signature_hashkeys = ""){
  body <- request_json_body(req)
  api_key <- if (identical(json_scalar(api_key), "")) json_scalar(body$api_key) else json_scalar(api_key)
  signature_hashkeys <- if (base::is.null(body$signature_hashkeys)) json_vector(signature_hashkeys) else json_vector(body$signature_hashkeys)

  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }
  if (base::length(signature_hashkeys) == 0) {
    return(json_error(res, 404, "signature_hashkeys cannot be empty."))
  }

  base::tryCatch({
    result <- build_signature_basket_zip(auth, signature_hashkeys, difexp_dir)
    if (!result$ok) {
      return(json_error(res, 404, "None of the requested signatures could be exported."))
    }

    raw_bytes <- base::readBin(result$zip_path, "raw", base::file.info(result$zip_path)$size)
    base::unlink(result$zip_path)

    res$setHeader("X-Basket-Included", base::length(result$included))
    res$setHeader("X-Basket-Skipped", base::length(result$skipped))
    res$serializer <- plumber::serializer_content_type("application/zip")
    plumber::as_attachment(
      raw_bytes,
      filename = base::sprintf("signature_basket_%s.zip", base::format(base::Sys.Date(), "%Y%m%d"))
    )
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Basket export failed: %s", err$message))
  })
}

#* Upload a signature from an .rds file shaped like /signatures/export's
#* own output (editor/admin; transcriptomics/proteomics only)
#* @parser multi
#* @parser rds
#* @param api_key
#* @param visibility
#* @param signature_file:file
#' @post /signatures/upload
signature_upload_route <- function(res, api_key = "", visibility = "false", signature_file){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  if (base::missing(signature_file) || base::length(signature_file) == 0) {
    return(json_error(res, 400, "signature_file is required."))
  }

  base::tryCatch({
    result <- build_signature_from_upload(
      auth = auth,
      uploaded = signature_file[[1]],
      visibility = normalize_flag(visibility, default = FALSE) == 1,
      difexp_dir = difexp_dir
    )

    if (!result$ok) {
      status <- switch(result$reason,
        "forbidden" = 403,
        "invalid_upload" = 400,
        "unsupported_assay_type" = 400,
        "unknown_features" = 400,
        "duplicate" = 409,
        500
      )
      message <- if (!base::is.null(result$message)) result$message else "Signature upload failed."
      return(json_error(res, status, message))
    }

    json_response(res, 200, payload = base::list(
      signature_hashkey = result$signature_hashkey,
      MESSAGES = base::sprintf("Signature '%s' uploaded.", result$signature_name)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Signature upload failed: %s", err$message))
  })
}

#* Browse the Rummagene catalog: literature-mined gene sets whose organism and
#* assay type are attested by PubMed MeSH, and whose every gene resolves in this
#* repository's transcriptomics reference table. Server-side paged and sorted.
#* Gene lists are omitted here and fetched per row on demand.
#* @param api_key
#* @param q Free text matched against term, title and description
#* @param organism
#* @param assay_type
#* @param year_min
#* @param year_max
#* @param n_genes_min
#* @param n_genes_max
#* @param limit
#* @param offset
#* @param sort_by One of term, title, year, n_genes, organism, assay_type
#* @param sort_dir asc (default) or desc
#' @get /rummagene/catalog
rummagene_catalog_route <- function(res, api_key = "", q = "", organism = "", assay_type = "",
                                    year_min = "", year_max = "", n_genes_min = "", n_genes_max = "",
                                    limit = 25, offset = 0, sort_by = "", sort_dir = "asc"){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  conn <- db_connect_local()
  base::on.exit(DBI::dbDisconnect(conn), add = TRUE)

  result <- base::tryCatch(
    search_rummagene_catalog(
      conn,
      q = json_scalar(q), organism = json_scalar(organism), assay_type = json_scalar(assay_type),
      year_min = json_scalar(year_min), year_max = json_scalar(year_max),
      n_genes_min = json_scalar(n_genes_min), n_genes_max = json_scalar(n_genes_max),
      limit = base::as.integer(json_scalar(limit, "25")),
      offset = base::as.integer(json_scalar(offset, "0")),
      sort_by = json_scalar(sort_by), sort_dir = json_scalar(sort_dir, "asc")
    ),
    error = function(e) e
  )
  if (base::inherits(result, "error")) {
    # search_rummagene_catalog() throws a plain stop("...must be a number...")
    # for a supplied-but-unparseable year_min/year_max/n_genes_min/n_genes_max
    # -- a caller mistake, not a server fault, so it must come back as 400
    # with the message rather than a generic 500. "nothing is invented": we
    # must not swallow that distinction and flatten every failure to 500.
    # Matched on the message text because the function raises a plain
    # condition rather than a classed one; anything that doesn't match this
    # specific, known wording still falls through to 500.
    error_message <- base::conditionMessage(result)
    status <- if (base::grepl("must be a number", error_message, fixed = TRUE)) 400 else 500
    return(json_error(res, status, base::sprintf("Catalog search failed: %s", error_message)))
  }

  json_response(res, 200, payload = base::list(count = result$count, rows = result$rows))
}

#* One Rummagene catalog entry, including its gene list. Separate from
#* /rummagene/catalog because the gene columns are large and only a detail view
#* needs them -- shipping them with every page of a 135k-row catalog would
#* dominate the response.
#* @param api_key
#* @param term The exact Rummagene term
#' @get /rummagene/catalog/entry
rummagene_catalog_entry_route <- function(res, api_key = "", term = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  term_value <- json_scalar(term)
  if (!base::nzchar(term_value)) {
    return(json_error(res, 400, "Provide the `term` of the catalog entry."))
  }

  conn <- db_connect_local()
  base::on.exit(DBI::dbDisconnect(conn), add = TRUE)

  entry <- get_rummagene_catalog_entry(conn, term_value)
  if (base::is.null(entry)) {
    return(json_error(res, 404, "No Rummagene catalog entry with that term."))
  }

  json_response(res, 200, payload = entry)
}
