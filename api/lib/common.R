# Shared request/response/DB helpers used across every endpoint domain.
# Depends on the `serializers` list defined in api.R.

# Connection handling.
#
# We keep a small pool of warm MySQL connections instead of opening a fresh one
# per request. Opening a connection costs ~100ms against a remote DB, and every
# authenticated request otherwise opened at least two (one to validate the
# api_key, one to do the work); pooling reuses a warm connection (~0ms) instead.
#
# The important property: callers keep using db_connect_local() +
# dbGetQuery(conn, ...) + dbDisconnect(conn) EXACTLY as before. db_connect_local()
# now hands out a checked-out pooled connection; dbDisconnect() on it *returns it
# to the pool* rather than closing it (this is pool's documented behavior, and is
# verified). So no call site needs to change. This is only safe because nothing
# in this codebase relies on same-connection session state -- no transactions,
# no LAST_INSERT_ID, no temporary tables, no session variables.
.db_pool <- base::new.env(parent = base::emptyenv())

db_pool <- function() {
  if (base::is.null(.db_pool$pool) || !pool::dbIsValid(.db_pool$pool)) {
    .db_pool$pool <- pool::dbPool(
      drv = RMySQL::MySQL(),
      dbname = base::Sys.getenv("DB_NAME"),
      host = base::Sys.getenv("DB_LOCAL_HOST"),
      port = base::as.integer(base::Sys.getenv("DB_PORT")),
      user = base::Sys.getenv("DB_USER"),
      password = base::Sys.getenv("DB_PASSWORD"),
      minSize = 1,
      maxSize = 8,
      idleTimeout = 300,
      # Validate a connection at most once per 5 min instead of on every
      # checkout -- otherwise, since each query checks a connection out, we'd pay
      # an extra validation round-trip per query against the remote DB. idle
      # connections are closed after idleTimeout, so a checked-out one is always
      # either fresh or recently validated.
      validationInterval = 300
    )
  }
  .db_pool$pool
}

db_connect_local <- function() {
  db_pool()
}

# Endpoints call dbDisconnect(conn) at the end of each request. Now that conn is
# the pool, there is nothing for the caller to disconnect -- the pool checks a
# connection out and returns it around every individual query on its own. Pool's
# own dbDisconnect method errors ("Not supported for pool objects"), so we make
# it a harmless no-op. This is what lets every existing dbDisconnect(conn) call
# site stay unchanged, with no risk of leaking a checked-out connection.
#
# The generic is passed as DBI::dbDisconnect rather than by name: setMethod()
# resolves a character name against attached packages, and this file is also
# sourced standalone by the tests, where DBI is loaded but not attached. Naming
# it there fails with "no existing definition for function 'dbDisconnect'".
if (base::requireNamespace("pool", quietly = TRUE) &&
    base::requireNamespace("DBI", quietly = TRUE)) {
  methods::setMethod(DBI::dbDisconnect, "Pool", function(conn, ...) base::invisible(TRUE))
}

json_response <- function(res, status = 200, payload = NULL) {
  # Return the payload object and let the json serializer (configured in
  # api.R with auto_unbox/null/na/pretty) encode it once. Encoding here as
  # well would double-wrap the body as ["<json string>"].
  res$serializer <- serializers[["json"]]
  res$status <- status
  payload
}

json_error <- function(res, status = 400, message) {
  payload <- base::data.frame(MESSAGES = as.character(message), stringsAsFactors = FALSE)
  # Tag the payload so callers (e.g. validate_api_key()'s "return the error
  # straight through to Plumber" pattern) can detect an error result with
  # is_json_error() instead of inspecting its type -- json_response() used to
  # return a JSON string here, and code that checked is.character(auth) broke
  # silently once json_response() started returning the payload object itself.
  base::class(payload) <- c("sigrepo_json_error", base::class(payload))
  json_response(res = res, status = status, payload = payload)
}

is_json_error <- function(x) {
  base::inherits(x, "sigrepo_json_error")
}

normalize_flag <- function(x, default = TRUE) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(as.integer(default))
  }

  value <- tolower(trimws(as.character(x[1])))
  as.integer(value %in% c("1", "true", "yes", "y"))
}

json_scalar <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return(default)
  }

  base::trimws(base::as.character(x[1]))
}

json_vector <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(base::character())
  }

  if (base::is.list(x) && !base::is.data.frame(x)) {
    x <- base::unlist(x, recursive = TRUE, use.names = FALSE)
  }

  x <- base::trimws(base::as.character(x))
  x[!base::is.na(x) & x != ""]
}

request_json_body <- function(req) {
  if (is.null(req) || is.null(req$postBody) || !base::nzchar(req$postBody)) {
    return(base::list())
  }

  base::tryCatch(
    jsonlite::fromJSON(req$postBody, simplifyVector = FALSE),
    error = function(err) base::list()
  )
}

compact_table <- function(tbl, max_rows = 50) {
  if (is.null(tbl) || base::nrow(tbl) == 0) {
    return(base::list())
  }

  tbl <- utils::head(tbl, max_rows)
  base::lapply(base::seq_len(base::nrow(tbl)), function(i) {
    base::as.list(tbl[i, , drop = FALSE])
  })
}
