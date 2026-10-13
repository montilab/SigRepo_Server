# The sign-in, sign-out, register, forgot-password and profile flows
# (legacy_app/modules/auth_module.R) under testServer(), with every function
# that would touch the database, the API or the mail service injected.
#
# auth_server() is a plain server function (its ids are not namespaced, so
# app.js and scripts/smoke_test_ui.R can address them), so each test builds a
# server closure with its fakes and keeps the module's return value in `auth`.
for (pkg in c("shinyjs", "shiny", "dplyr")) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}
source(testthat::test_path("../../api/lib/user.R"), local = globalenv())
source(testthat::test_path("../../legacy_app/utils/utils.R"), local = globalenv())
source(testthat::test_path("../../legacy_app/utils/auth_utils.R"), local = globalenv())
source(testthat::test_path("../../legacy_app/modules/auth_module.R"), local = globalenv())

auth_known_users <- data.frame(
  user_name = c("ann", "bo"),
  user_email = c("ann@example.org", "bo@example.org"),
  user_role = "editor",
  api_key = c("key-ann", "key-bo"),
  user_first = c("Ann", NA),
  user_last = c("Lee", NA),
  user_affiliation = c("BU", NA),
  active = c(1L, 0L),
  stringsAsFactors = FALSE
)

# A fake that records every call and returns a canned value.
recorder <- function(value = NULL) {
  calls <- list()
  list(
    calls = function() calls,
    fn = function(...) {
      calls[[length(calls) + 1]] <<- list(...)
      if (is.function(value)) value(...) else value
    }
  )
}

search_by_name <- function(conn_handler, user_name = NULL, ...) {
  if (is.null(user_name)) return(auth_known_users)
  auth_known_users[auth_known_users$user_name == user_name, , drop = FALSE]
}

# Runs auth_server() with the given fakes. The expression sees `auth`, the
# module's return value, plus `input`, `output` and `session`.
run_auth <- function(expr,
                     search_user_fn = search_by_name,
                     validate_fn = function(conn_handler) auth_known_users[1, , drop = FALSE],
                     new_conn_fn = function(...) list(...),
                     register_fn = function(...) list(ok = TRUE, reason = "registered"),
                     reset_fn = function(identifier) list(ok = TRUE, reason = "sent"),
                     update_user_fn = function(...) invisible(NULL)) {
  # The server closure and the holder its return value lands in are spliced
  # into the quoted expression, so it does not matter which environment
  # testServer() evaluates it in.
  holder <- new.env()
  server <- function(input, output, session) {
    holder$auth <- auth_server(
      input, output, session,
      conn_handler = "root-conn",
      search_user_fn = search_user_fn,
      validate_fn = validate_fn,
      new_conn_fn = new_conn_fn,
      register_fn = register_fn,
      reset_fn = reset_fn,
      update_user_fn = update_user_fn
    )
  }
  later::with_temp_loop(eval(bquote(testServer(.(server), {
    auth <- .(holder)$auth
    .(substitute(expr))
  })), envir = parent.frame()))
}

sign_in_as <- function(session, user_name, password) {
  session$setInputs(username = user_name, password = password)
  session$setInputs(sign_in_btn = 1)
}

# ---- sign in ----------------------------------------------------------------

test_that("a blank username is refused before anything is looked up", {
  search <- recorder(auth_known_users[0, ])
  run_auth(search_user_fn = search$fn, {
    sign_in_as(session, "   ", "secret")
    expect_equal(auth$messages$login(), "'Username' cannot be empty")
    expect_null(auth$user_conn_handler())
    expect_length(search$calls(), 0)
  })
})

test_that("an unknown user and an inactive user get their messages", {
  run_auth({
    sign_in_as(session, "zed", "secret")
    expect_equal(auth$messages$login(), "Invalid username or password!")

    sign_in_as(session, "bo", "secret")
    expect_match(auth$messages$login(), "User = 'bo' is currently inactive")
    expect_null(auth$user_conn_handler())
  })
})

test_that("a known user with a blank password is refused", {
  run_auth({
    sign_in_as(session, "ann", "  ")
    expect_equal(auth$messages$login(), "'Password' cannot be empty")
    expect_null(auth$user_conn_handler())
  })
})

test_that("a failed validation keeps the user signed out", {
  run_auth(validate_fn = function(conn_handler) stop("Access denied"), {
    sign_in_as(session, "ann", "wrong")
    expect_equal(auth$messages$login(), "Invalid username or password!")
    expect_null(auth$user_conn_handler())
    expect_null(auth$user_login_info())
  })
})

test_that("a good sign-in builds the handler from the environment and keeps the user row", {
  old <- Sys.getenv(c("DB_NAME", "DB_HOST", "DB_PORT", "API_HOST", "API_PORT"), unset = NA)
  on.exit({
    for (name in names(old)) {
      if (is.na(old[[name]])) Sys.unsetenv(name) else do.call(Sys.setenv, as.list(old[name]))
    }
  })
  Sys.setenv(DB_NAME = "sigrepo", DB_HOST = "sigrepo-mysql", DB_PORT = "3306",
             API_HOST = "http://sigrepo-api:3838", API_PORT = "3838")

  new_conn <- recorder(function(...) c(list(...), kind = "handler"))
  run_auth(new_conn_fn = new_conn$fn, {
    sign_in_as(session, " ann ", " secret ")

    expect_null(auth$messages$login())
    handler <- auth$user_conn_handler()
    expect_equal(handler$kind, "handler")
    expect_equal(auth$user_login_info()$user_name, "ann")
    expect_equal(as.character(output$welcome_msg$html), "ann")

    args <- new_conn$calls()[[1]]
    expect_equal(args$user, "ann")
    expect_equal(args$password, "secret")
    expect_equal(args$dbname, "sigrepo")
    expect_equal(args$host, "sigrepo-mysql")
    expect_equal(args$port, 3306L)
    expect_equal(args$api_host, "http://sigrepo-api:3838")
    expect_equal(args$api_port, 3838L)
    expect_false(any(grepl("142.93.67.157", unlist(args), fixed = TRUE)))
  })
})

test_that("signing out clears the handler and the user row", {
  run_auth({
    sign_in_as(session, "ann", "secret")
    expect_false(is.null(auth$user_conn_handler()))

    session$setInputs(log_out_btn = 1)
    expect_null(auth$user_conn_handler())
    expect_null(auth$user_login_info())
  })
})

# ---- register ---------------------------------------------------------------

test_that("registering passes the six trimmed fields on and shows the outcome", {
  register <- recorder(list(ok = TRUE, reason = "Thank you for signing up!"))
  run_auth(register_fn = register$fn, {
    session$setInputs(register = 1)
    session$setInputs(
      register_username = " cy ", register_password = " pw ", register_email = " cy@example.org ",
      register_first_name = "Cy", register_last_name = "", register_affiliation = " BU "
    )
    session$setInputs(register_user = 1)

    args <- register$calls()[[1]]
    expect_equal(args$user_name, "cy")
    expect_equal(args$password, "pw")
    expect_equal(args$user_email, "cy@example.org")
    expect_equal(args$user_first, "Cy")
    expect_equal(args$user_last, "")
    expect_equal(args$user_affiliation, "BU")
    expect_equal(auth$messages$register(), "Thank you for signing up!")
  })
})

test_that("a refused registration shows the reason", {
  run_auth(register_fn = function(...) list(ok = FALSE, reason = "Username cannot be empty."), {
    session$setInputs(register = 1)
    session$setInputs(register_username = "", register_password = "pw", register_email = "cy@example.org")
    session$setInputs(register_user = 1)
    expect_equal(auth$messages$register(), "Username cannot be empty.")
  })
})

# ---- forgot password --------------------------------------------------------

test_that("a password reset is requested by username or by email, whichever is chosen", {
  reset <- recorder(list(ok = TRUE, reason = "If that account exists, a temporary password has been sent."))
  run_auth(reset_fn = reset$fn, {
    session$setInputs(forget_password = 1)
    session$setInputs(psw_lookup_option = "Username", psw_username = " ann ", psw_email = "ignored@example.org")
    session$setInputs(send_tmp_password = 1)
    expect_equal(reset$calls()[[1]]$identifier, "ann")
    expect_match(auth$messages$forgot(), "If that account exists")

    session$setInputs(psw_lookup_option = "Email", psw_email = " ann@example.org ")
    session$setInputs(send_tmp_password = 2)
    expect_equal(reset$calls()[[2]]$identifier, "ann@example.org")
  })
})

# ---- profile ----------------------------------------------------------------

test_that("changing the email validates it, updates the account and the user row", {
  update <- recorder()
  run_auth(update_user_fn = update$fn, {
    sign_in_as(session, "ann", "secret")
    session$setInputs(edit_profile = 1)

    session$setInputs(new_profile_email = "  ")
    session$setInputs(save_change_email = 1)
    expect_equal(auth$messages$profile(), "Email cannot be empty")

    session$setInputs(new_profile_email = "not-an-email")
    session$setInputs(save_change_email = 2)
    expect_equal(auth$messages$profile(), "Invalid email format.")

    session$setInputs(new_profile_email = "BO@example.org")
    session$setInputs(save_change_email = 3)
    expect_match(auth$messages$profile(), "Email = 'BO@example.org' already existed")
    expect_length(update$calls(), 0)

    session$setInputs(new_profile_email = " ann.lee@example.org ")
    session$setInputs(save_change_email = 4)
    expect_equal(auth$messages$profile(), "Your email has been changed and updated.")
    expect_equal(update$calls()[[1]]$user_name, "ann")
    expect_equal(update$calls()[[1]]$email, "ann.lee@example.org")
    expect_equal(auth$user_login_info()$user_email, "ann.lee@example.org")
  })
})

test_that("changing the password updates the account and the signed-in handler", {
  update <- recorder()
  run_auth(update_user_fn = update$fn, {
    sign_in_as(session, "ann", "secret")
    session$setInputs(edit_profile = 1)

    session$setInputs(new_profile_password = "")
    session$setInputs(save_change_password = 1)
    expect_equal(auth$messages$profile(), "Password cannot be empty")
    expect_length(update$calls(), 0)

    session$setInputs(new_profile_password = " newer ")
    session$setInputs(save_change_password = 2)
    expect_equal(auth$messages$profile(), "Your password has been changed and updated.")
    expect_equal(update$calls()[[1]]$user_name, "ann")
    expect_equal(update$calls()[[1]]$password, "newer")
    expect_equal(auth$user_conn_handler()$password, "newer")
  })
})

test_that("the sign-in card renders with the ids the scripts rely on", {
  html <- as.character(auth_signin_ui())
  for (id in c("username", "password", "sign_in_btn", "forget_password", "register", "login_error_message")) {
    expect_true(grepl(paste0('id="', id, '"'), html, fixed = TRUE) || grepl(paste0("id='", id, "'"), html, fixed = TRUE), info = id)
  }
})
