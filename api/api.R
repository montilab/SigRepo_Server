
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

#* Search signatures by organism/phenotype/assay_type/keyword
#* @param api_key
#* @param organism
#* @param phenotype
#* @param assay_type
#* @param keyword
#* @param limit
#' @get /signatures/search
search_signatures_route <- function(res, api_key = "", organism = "", phenotype = "", assay_type = "", keyword = "", limit = 20){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  base::tryCatch({
    conn <- db_connect_local()
    results <- search_signatures(
      conn = conn,
      organism = json_scalar(organism),
      phenotype = json_scalar(phenotype),
      assay_type = json_scalar(assay_type),
      keyword = json_scalar(keyword),
      limit = limit,
      is_admin = identical(auth$user_role, "admin")
    )
    base::suppressWarnings(DBI::dbDisconnect(conn))
    json_response(res, 200, payload = base::list(
      count = base::nrow(results),
      signatures = compact_table(results, max_rows = 100)
    ))
  }, error = function(err) {
    json_error(res, 500, base::sprintf("Signature search failed: %s", err$message))
  })
}
