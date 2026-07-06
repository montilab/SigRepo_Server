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

validate_api_key <- function(res, api_key) {
  if (base::missing(api_key) || is.null(api_key)) {
    return(json_error(res, 404, "Missing required parameter: api_key"))
  }

  api_key <- json_scalar(api_key)

  if (identical(api_key, "")) {
    return(json_error(res, 404, "api_key cannot be empty."))
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
    structure(
      base::list(message = base::sprintf("Could not validate api_key: %s", err$message)),
      class = "sigrepo_api_key_error"
    )
  }, finally = {
    if (!is.null(conn)) {
      base::suppressWarnings(DBI::dbDisconnect(conn))
    }
  })

  if (base::inherits(user_tbl, "sigrepo_api_key_error")) {
    return(json_error(res, 500, user_tbl$message))
  }

  if (base::nrow(user_tbl) == 0) {
    return(json_error(res, 404, "Invalid api key."))
  }

  base::list(
    user_name = user_tbl$user_name[1],
    user_role = user_tbl$user_role[1],
    api_key = api_key
  )
}
