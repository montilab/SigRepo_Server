# Username/password login for the web UI. SigRepo stores no plaintext
# password -- the users table holds `user_password_hashkey`, which
# addUser() sets to md5(tolower(user_password)) via SigRepo::createHashKey().
# We recompute the same hash and compare, then hand back the account's
# api_key so the browser can authenticate subsequent requests the same way
# every other endpoint already expects (see validate_api_key).
# Depends on api/lib/common.R (db_connect_local).

hash_user_password <- function(password) {
  digest::digest(base::tolower(password), algo = "md5", serialize = FALSE)
}

# Returns list(user_name, user_role, api_key) on success, or NULL for an
# unknown user, wrong password, or inactive account. Callers must not leak
# which of those failed -- the route returns one generic 401.
authenticate_user <- function(user_name, password) {
  user_name <- base::trimws(base::as.character(user_name)[1])
  password <- base::as.character(password)[1]

  if (base::is.na(user_name) || !base::nzchar(user_name) ||
      base::is.na(password) || !base::nzchar(password)) {
    return(NULL)
  }

  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  user_row <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "users",
    return_var = c("user_name", "user_role", "api_key", "user_password_hashkey", "active"),
    filter_coln_var = "user_name",
    filter_coln_val = base::list("user_name" = user_name),
    check_db_table = TRUE
  )

  if (base::nrow(user_row) == 0) {
    return(NULL)
  }

  row <- user_row[1, , drop = FALSE]

  if (!base::identical(as.integer(row$active[1]), 1L)) {
    return(NULL)
  }

  if (!base::identical(hash_user_password(password), as.character(row$user_password_hashkey[1]))) {
    return(NULL)
  }

  base::list(
    user_name = as.character(row$user_name[1]),
    user_role = as.character(row$user_role[1]),
    api_key = as.character(row$api_key[1])
  )
}
