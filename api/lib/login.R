# Username/password login for the web UI.
#
# The password is proven against MySQL itself: SigRepo accounts ARE MySQL
# accounts, so we try to open a connection with the supplied credentials and
# treat a successful connect as proof of the password. This is what the Shiny
# app did (shiny/utils/validateUser.R -> newConnHandler + conn_init), and it is
# why every user needs their own database account rather than sharing one.
#
# It is deliberately NOT a hash comparison against users.user_password_hashkey.
# That column exists and addUser() populates it, but the Shiny portal never
# authenticated against it, so on a repository migrated from Shiny its contents
# do not correspond to anyone's actual password -- comparing against it rejects
# every login. Keeping MySQL as the source of truth means existing passwords
# keep working with no reset and no user contact.
#
# Depends on api/lib/common.R (db_connect_local).

# Opens a short-lived connection as `user_name`. Returns TRUE if MySQL accepted
# the credentials, FALSE otherwise.
#
# This must not go through db_pool(): the pool is the API's own service
# connection, already authenticated as DB_USER, so borrowing from it would
# accept any password at all. Every attempt is a fresh connect, closed
# immediately -- we only care whether the handshake succeeded.
verify_db_credentials <- function(user_name, password) {
  conn <- base::tryCatch(
    DBI::dbConnect(
      drv = RMySQL::MySQL(),
      dbname = base::Sys.getenv("DB_NAME"),
      host = base::Sys.getenv("DB_LOCAL_HOST"),
      port = base::as.integer(base::Sys.getenv("DB_PORT")),
      user = user_name,
      password = password
    ),
    error = function(err) NULL,
    warning = function(war) NULL
  )

  if (base::is.null(conn)) {
    return(FALSE)
  }

  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  base::isTRUE(DBI::dbIsValid(conn))
}

# Kept because addSignature()/addUser() still write this column via
# SigRepo::createHashKey(), and tests assert the two agree. It is no longer the
# login path -- see the note above.
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

  # Prove the password first, so an unknown account and a wrong password cost
  # the same work and neither is distinguishable from the other by timing.
  if (!verify_db_credentials(user_name, password)) {
    return(NULL)
  }

  # Credentials are good; read the account's role and api_key over the API's
  # own pooled connection rather than the user's, so this does not depend on
  # what the user's MySQL grants happen to allow them to SELECT.
  conn <- db_connect_local()
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  user_row <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "users",
    return_var = c("user_name", "user_role", "api_key", "active"),
    filter_coln_var = "user_name",
    filter_coln_val = base::list("user_name" = user_name),
    check_db_table = TRUE
  )

  # A valid MySQL account with no matching users row is not a SigRepo user --
  # this is what stops a bare database account (or root) logging into the portal.
  if (base::nrow(user_row) == 0) {
    return(NULL)
  }

  row <- user_row[1, , drop = FALSE]

  if (!base::identical(as.integer(row$active[1]), 1L)) {
    return(NULL)
  }

  base::list(
    user_name = as.character(row$user_name[1]),
    user_role = as.character(row$user_role[1]),
    api_key = as.character(row$api_key[1])
  )
}
