# Helpers behind the sign-in, sign-out and profile flows in
# modules/auth_module.R, kept out of the module so they can be tested without
# Shiny (tests/testthat/test-shiny-auth-helpers.R). Registration and password
# reset are the API's register_new_user() and request_password_reset() from
# api/lib/user.R, which bootstrap.R sources alongside this file.

# One trimmed string from a text input: "" when the input was never set.
trimmed <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) {
    return("")
  }
  trimws(as.character(x[1]))
}

# `message` when `value` is blank, otherwise NULL. The checks below all return
# a message or NULL so a handler can chain them with %||%: the first problem
# wins and nothing after it runs.
empty_problem <- function(value, message) {
  if (nzchar(value)) NULL else message
}

# Why `user_name` cannot sign in, given the users rows for that name. An
# unknown name gets the same answer as a wrong password; an inactive account
# is told so, since its owner can do something about it.
signin_user_problem <- function(users, user_name) {
  if (!is.data.frame(users) || nrow(users) == 0) {
    return("Invalid username or password!")
  }
  if (identical(as.integer(users$active[1]), 0L)) {
    return(sprintf(
      "User = '%s' is currently inactive in our database. Please contact our admin to activate it.",
      user_name
    ))
  }
  NULL
}

email_format_problem <- function(email) {
  if (valid_email_format(email)) NULL else "Invalid email format."
}

# Whether an account already has this email, ignoring case.
email_taken <- function(users, email) {
  is.data.frame(users) && "user_email" %in% names(users) &&
    tolower(email) %in% tolower(users$user_email)
}

email_taken_problem <- function(email, users) {
  if (!email_taken(users, email)) {
    return(NULL)
  }
  sprintf("Email = '%s' already existed in our database. Please choose a different email.", email)
}

# The profile dialog shows every field as text, with NA (and the strings "NA"
# and "NULL" the database has been known to hold) as blanks.
blank_for_na <- function(df) {
  df[] <- lapply(df, function(column) {
    column <- as.character(column)
    column[is.na(column) | column %in% c("NA", "NULL")] <- ""
    column
  })
  df
}

# The API address for a per-user connection handler, from API_HOST and
# API_PORT like the server's own handler in bootstrap.R. There is no fallback
# address: a deployment that leaves API_HOST unset gets the client's default,
# and bootstrap.R says so at startup.
api_connection <- function(env = Sys.getenv) {
  settings <- list(api_host = env("API_HOST"))
  port <- suppressWarnings(as.integer(env("API_PORT")))
  if (!is.na(port)) {
    settings$api_port <- port
  }
  settings
}

# Opens the user's own MySQL connection, checks it carries at least the editor
# role, and returns their users row. A wrong password fails inside
# conn_init(), which the caller catches.
validateUser <- function(conn_handler) {
  conn <- SigRepo::conn_init(conn_handler = conn_handler)

  conn_info <- SigRepo::checkPermissions(
    conn = conn,
    action_type = "SELECT",
    required_role = "editor"
  )

  user_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "users",
    return_var = "*",
    filter_coln_var = "user_name",
    filter_coln_val = base::list("user_name" = conn_info$user[1]),
    check_db_table = TRUE
  )

  base::suppressWarnings(DBI::dbDisconnect(conn))

  user_tbl
}
