# User lifecycle: activation, self-registration, and password reset.
#
# Email delivery is not done here. The Montilab server owns the mail templates
# and the SMTP credentials, and exposes them as
# https://montilab.bu.edu/SigRepo/send_notifications/<action>, authenticated
# with SENDMAIL_KEY. We call those endpoints exactly where the Shiny app did.
#
# Note send_tmp_password is what actually resets a password -- the Montilab
# side generates the temporary one and applies it. This server deliberately
# does not invent or store a password of its own, so the two halves cannot
# disagree about what the user's password now is.

SENDMAIL_BASE_URL <- "https://montilab.bu.edu/SigRepo/send_notifications"

sendmail_key <- function() {
  base::Sys.getenv("SENDMAIL_KEY")
}

# Every notification is the same shape: one action, a user_name, and the key.
send_notification <- function(action, user_name, api_key = sendmail_key()) {
  api_url <- base::sprintf(
    "%s/%s?user_name=%s&api_key=%s",
    SENDMAIL_BASE_URL,
    action,
    utils::URLencode(user_name, reserved = TRUE),
    utils::URLencode(api_key, reserved = TRUE)
  )

  base::tryCatch(
    httr::GET(url = api_url),
    error = function(err) base::list(status_code = 0L)
  )
}

mark_user_active <- function(conn_handler, user_name) {
  SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, active = TRUE)
}

send_user_activation_email <- function(user_name, api_key) {
  send_notification("activate_user", user_name, api_key)
}

valid_email_format <- function(email) {
  base::grepl(
    "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
    base::as.character(email),
    ignore.case = TRUE
  )
}

# Returns list(ok = TRUE) or list(ok = FALSE, reason = <message for the user>).
#
# The reasons here are deliberately specific: someone picking a username needs
# to be told it is taken, otherwise they cannot proceed. That is a different
# situation from password reset below, where naming a valid account would leak
# who has one.
register_new_user <- function(user_name, password, user_email,
                              user_first = "", user_last = "", user_affiliation = "") {
  user_name <- base::trimws(json_scalar(user_name))
  password <- base::as.character(password)[1]
  user_email <- base::trimws(json_scalar(user_email))

  if (!base::nzchar(user_name)) {
    return(base::list(ok = FALSE, reason = "Username cannot be empty."))
  }
  if (base::is.na(password) || !base::nzchar(password)) {
    return(base::list(ok = FALSE, reason = "Password cannot be empty."))
  }
  if (!base::nzchar(user_email)) {
    return(base::list(ok = FALSE, reason = "Email cannot be empty."))
  }
  if (!valid_email_format(user_email)) {
    return(base::list(ok = FALSE, reason = "Invalid email format."))
  }

  existing <- SigRepo::searchUser(conn_handler = conn_handler)

  if (base::tolower(user_name) %in% base::tolower(existing$user_name)) {
    already <- existing[base::tolower(existing$user_name) == base::tolower(user_name), , drop = FALSE]
    if (base::nrow(already) > 0 && base::identical(base::as.integer(already$active[1]), 0L)) {
      return(base::list(ok = FALSE, reason = base::sprintf(
        "User = '%s' already exists in our database and is currently inactive. If this is your account, please contact our admin to activate it.",
        user_name
      )))
    }
    return(base::list(ok = FALSE, reason = base::sprintf(
      "User = '%s' already exists in our database. Please choose a different name.", user_name
    )))
  }

  if (base::tolower(user_email) %in% base::tolower(existing$user_email)) {
    return(base::list(ok = FALSE, reason = base::sprintf(
      "Email = '%s' already exists in our database. Please choose a different email.", user_email
    )))
  }

  # addUser() does both halves of an account: the users row (active = 0, so an
  # admin still has to approve it) and the MySQL account plus role-scoped
  # grants. Both are required -- login authenticates against MySQL, so a users
  # row on its own would produce an account nobody can sign in to.
  user_tbl <- base::data.frame(
    user_name = user_name,
    user_password = password,
    user_email = user_email,
    user_first = json_scalar(user_first),
    user_last = json_scalar(user_last),
    user_affiliation = json_scalar(user_affiliation),
    user_role = "editor",
    active = 0,
    stringsAsFactors = FALSE
  )

  created <- base::tryCatch({
    SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)
    TRUE
  }, error = function(err) {
    base::print(err)
    FALSE
  })

  if (!created) {
    return(base::list(ok = FALSE, reason = "Could not create the account. Please contact admin for support."))
  }

  notify <- send_notification("register_user", user_name)
  if (!base::identical(base::as.integer(notify$status_code), 200L)) {
    # The account exists at this point; only the admin's heads-up failed. Say
    # so rather than implying nothing happened, or they may register twice.
    return(base::list(ok = TRUE, warning = TRUE, reason = base::sprintf(
      "Account '%s' was created, but the notification email to our administrator failed. Please contact admin to activate it.",
      user_name
    )))
  }

  base::list(ok = TRUE, reason = "Thank you for signing up! Our administrator will contact you once your account is activated.")
}

# Accepts either a username or an email address, matching the Shiny form.
#
# Unlike registration, this never reveals whether the account exists: the
# caller is not logged in, and answering truthfully would turn the form into a
# way to enumerate who has an account. The response is identical either way.
request_password_reset <- function(identifier) {
  identifier <- base::trimws(json_scalar(identifier))

  if (!base::nzchar(identifier)) {
    return(base::list(ok = FALSE, reason = "Enter your username or the email address on your account."))
  }

  generic <- base::list(ok = TRUE, reason = "If that account exists, a temporary password has been sent to its email address.")

  users <- base::tryCatch(
    SigRepo::searchUser(conn_handler = conn_handler),
    error = function(err) base::data.frame()
  )

  if (base::nrow(users) == 0) {
    return(generic)
  }

  match_row <- if (valid_email_format(identifier)) {
    users[base::tolower(users$user_email) == base::tolower(identifier), , drop = FALSE]
  } else {
    users[base::tolower(users$user_name) == base::tolower(identifier), , drop = FALSE]
  }

  if (base::nrow(match_row) == 0) {
    return(generic)
  }

  send_notification("send_tmp_password", base::as.character(match_row$user_name[1]))

  generic
}
