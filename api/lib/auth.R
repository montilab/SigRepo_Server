# API key / admin key validation shared by signature, difexp, and admin endpoints.
# Depends on api/lib/common.R (json_error, json_scalar) and the `conn_handler`
# global defined in api.R.

require_admin_key <- function(res, admin_key) {
  if (base::missing(admin_key) || is.null(admin_key)) {
    return(json_error(res, 404, "Missing required parameter: admin_key"))
  }

  admin_key <- base::trimws(admin_key[1])

  if (identical(admin_key, "")) {
    return(json_error(res, 404, "admin_key cannot be empty."))
  }

  if (!identical(admin_key, base::Sys.getenv("ADMIN_KEY"))) {
    return(json_error(res, 404, "Invalid admin key."))
  }

  NULL
}

sigrepo_condition <- function(subclass, message, status = 500) {
  structure(
    class = c(subclass, "sigrepo_api_key_error", "error", "condition"),
    list(message = message, status = status, call = NULL)
  )
}

# Core api_key -> user lookup, shared by the Plumber-facing validate_api_key()
# and the MCP-facing require_api_key(). Throws a classed
# "sigrepo_api_key_error" condition (with a $status) on any failure instead of
# shaping a response itself, since callers need to render that failure very
# differently (a Plumber json_error() vs. an MCP tool error).
lookup_user_by_api_key <- function(api_key) {
  if (base::missing(api_key) || is.null(api_key)) {
    stop(sigrepo_condition("sigrepo_missing_api_key", "Missing required parameter: api_key", 404))
  }

  api_key <- json_scalar(api_key)

  if (identical(api_key, "")) {
    stop(sigrepo_condition("sigrepo_empty_api_key", "api_key cannot be empty.", 404))
  }

  conn <- NULL
  user_tbl <- base::tryCatch({
    conn <- SigRepo::conn_init(conn_handler = conn_handler)
    SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "users",
      return_var = c("user_name", "user_role", "api_key"),
      filter_coln_var = "api_key",
      filter_coln_val = base::list("api_key" = api_key),
      check_db_table = TRUE
    )
  }, error = function(err) {
    stop(sigrepo_condition(
      "sigrepo_api_key_lookup_error",
      base::sprintf("Could not validate api_key: %s", err$message),
      500
    ))
  }, finally = {
    if (!is.null(conn)) {
      base::suppressWarnings(DBI::dbDisconnect(conn))
    }
  })

  if (base::nrow(user_tbl) == 0) {
    stop(sigrepo_condition("sigrepo_invalid_api_key", "Invalid api key.", 404))
  }

  base::list(
    user_name = user_tbl$user_name[1],
    user_role = user_tbl$user_role[1],
    api_key = api_key
  )
}

# Plumber-facing: same behavior/response shape as before the refactor.
validate_api_key <- function(res, api_key) {
  base::tryCatch(
    lookup_user_by_api_key(api_key),
    sigrepo_api_key_error = function(err) json_error(res, err$status, err$message)
  )
}

# MCP-facing: lets the sigrepo_api_key_error condition propagate so
# mcptools/ellmer report it back to the calling agent as a tool error.
require_api_key <- function(api_key) {
  lookup_user_by_api_key(api_key)
}
