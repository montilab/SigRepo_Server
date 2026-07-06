# Shared request/response/DB helpers used across every endpoint domain.
# Depends on the `serializers` list defined in api.R.

db_connect_local <- function() {
  DBI::dbConnect(
    drv = RMySQL::MySQL(),
    dbname = base::Sys.getenv("DB_NAME"),
    host = base::Sys.getenv("DB_LOCAL_HOST"),
    port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"),
    password = base::Sys.getenv("DB_PASSWORD")
  )
}

json_response <- function(res, status = 200, payload = NULL) {
  res$serializer <- serializers[["json"]]
  res$status <- status
  jsonlite::toJSON(payload, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null")
}

json_error <- function(res, status = 400, message) {
  json_response(
    res = res,
    status = status,
    payload = base::data.frame(MESSAGES = as.character(message), stringsAsFactors = FALSE)
  )
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
