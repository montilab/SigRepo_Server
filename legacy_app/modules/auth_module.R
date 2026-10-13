# Signing in and out, registering, resetting a password, and the profile
# dialog.
#
# auth_server() is a plain server function rather than a moduleServer(): its
# inputs keep their bare ids (username, password, sign_in_btn, ...) because
# www/assets/js/app.js and scripts/smoke_test_ui.R address them by those ids.
# Everything that touches the database, the API or the mail service comes in
# as an argument so tests/testthat/test-shiny-auth-module.R can stand it in.
# The pure checks live in utils/auth_utils.R; registration and password reset
# are the API's own register_new_user() and request_password_reset().

# The sign-in card. Enter in either field clicks the button: login_keypress()
# in app.js reads the inline handler's `event`.
auth_signin_ui <- function() {
  shiny::div(
    class = "login-wrapper", id = "login-wrapper",

    shiny::div(
      class = "login-card",

      shiny::div(
        class = "login-brand",
        tags$img(src = "images/logo.png", alt = "SigRepo"),
        shiny::div(
          tags$h1(class = "login-title", "Sign in"),
          shiny::p(class = "login-sub", "SigRepo signature repository")
        )
      ),

      tags$form(
        class = "login-form",

        shiny::div(
          class = "login-field",
          tags$label(class = "login-label", `for` = "username", "Username"),
          shiny::HTML("<input class='login-input' type='text' id='username' onkeypress='login_keypress(event)' placeholder='Enter username'>")
        ),

        shiny::div(
          class = "login-field",
          tags$label(class = "login-label", `for` = "password", "Password"),
          shiny::div(
            class = "login-password",
            shiny::HTML("<input class='login-input' type='password' id='password' onkeypress='login_keypress(event)' placeholder='Enter password'>"),
            shiny::HTML("<span class='login-eye' onclick='toggle_password()'>👁️</span>")
          )
        ),

        shiny::div(
          class = "login-message",
          shiny::uiOutput(outputId = "login_error_message")
        ),

        shiny::div(
          class = "login-actions",
          shiny::actionButton(
            inputId = "sign_in_btn",
            class = "login-button",
            label = "Sign in"
          ),
          shiny::HTML("<a href='#' id='forget_password' class='action-button'>Forgot password?</a>")
        ),

        shiny::div(
          class = "login-links",
          "Don't have an account? ",
          shiny::HTML("<a href='#' id='register' class='action-button'>Register here</a>")
        )
      )
    )
  )
}

required_label <- function(text) {
  shiny::strong(shiny::span(style = "color: red;", "*"), text)
}

auth_register_modal <- function() {
  shiny::modalDialog(
    size = "l",
    title = shiny::span(shiny::icon("user-plus"), "Register"),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::textInput("register_username", required_label("Username"), value = "", placeholder = "Enter Username", width = "100%")
      ),
      shiny::column(
        width = 12,
        id = "register-password",
        required_label("Password"),
        shiny::div(
          class = "register-password",
          shiny::HTML("<input type='password' id='register_password' placeholder='Enter Password'>"),
          shiny::HTML("<span class='toggle-register-password' onclick='toggle_register_password()'>👁️</span>")
        )
      ),
      shiny::column(
        width = 12,
        shiny::textInput("register_email", required_label("Email"), value = "", placeholder = "Enter Email", width = "100%"),
        shiny::textInput("register_first_name", shiny::strong("First Name"), value = "", placeholder = "Enter First Name", width = "100%"),
        shiny::textInput("register_last_name", shiny::strong("Last Name"), value = "", placeholder = "Enter Last Name", width = "100%"),
        shiny::textInput("register_affiliation", shiny::strong("Affiliation"), value = "", placeholder = "Enter Affiliation", width = "100%"),
        shiny::uiOutput("register_message")
      )
    ),
    footer = shiny::tagList(
      shiny::actionButton("register_user", shiny::strong("Register"), class = "btn-primary"),
      shiny::actionButton("dismiss_register", shiny::strong("Cancel"))
    )
  )
}

auth_forgot_modal <- function() {
  shiny::modalDialog(
    size = "l",
    title = NULL,
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3("Forgot your password?", class = "text-center"),
        shiny::br(),
        shiny::p(shiny::strong("To access your account, please fill in the following information:")),
        shiny::br(),
        shiny::radioButtons("psw_lookup_option", label = NULL, choices = c("Username", "Email"), inline = TRUE),
        shiny::conditionalPanel(
          condition = 'input.psw_lookup_option == "Username"',
          shiny::textInput("psw_username", shiny::strong("Enter Your Username"), value = "", width = "100%")
        ),
        shiny::conditionalPanel(
          condition = 'input.psw_lookup_option == "Email"',
          shiny::textInput("psw_email", shiny::strong("Enter Your Email"), value = "", width = "100%")
        ),
        shiny::uiOutput("forgot_psw_message")
      )
    ),
    footer = shiny::tagList(
      shiny::actionButton("send_tmp_password", shiny::strong("Submit"), class = "btn-primary"),
      shiny::actionButton("dismiss_forgot_password", shiny::strong("Cancel"))
    )
  )
}

# `user_tbl` is the signed-in user's row with blanks for missing fields.
auth_profile_modal <- function(user_tbl) {
  shiny::modalDialog(
    size = "l",
    title = shiny::span(shiny::icon("user"), "Your profile"),
    shiny::fluidRow(
      shiny::column(
        width = 6,
        shiny::p(shiny::strong("Username: "), user_tbl$user_name[1]),
        shiny::p(
          shiny::strong("Email: "),
          shiny::HTML(sprintf("<input type='text' id='profile_email' value='%s' disabled='disabled'>", htmltools::htmlEscape(user_tbl$user_email[1], attribute = TRUE)))
        ),
        shiny::p(class = "profile-bullet", shiny::strong("Role: "), user_tbl$user_role[1]),
        shiny::p(class = "profile-bullet", shiny::strong("API Key: "), user_tbl$api_key[1])
      ),
      shiny::column(
        width = 6,
        shiny::p(class = "profile-bullet", shiny::strong("First Name: "), user_tbl$user_first[1]),
        shiny::p(class = "profile-bullet", shiny::strong("Last Name: "), user_tbl$user_last[1]),
        shiny::p(class = "profile-bullet", shiny::strong("Affiliation: "), user_tbl$user_affiliation[1])
      )
    ),
    shiny::fluidRow(
      shiny::column(
        width = 12,
        id = "change-profile-email",
        style = "display: none;",
        shiny::HTML("<span><b>Enter Your New Email</b></span>"),
        shiny::HTML("<input type='text' id='new_profile_email'>")
      ),
      shiny::column(
        width = 12,
        id = "change-profile-password",
        style = "display: none;",
        shiny::HTML("<span><b>Enter Your New Password</b></span>"),
        shiny::div(
          class = "change-profile-password",
          shiny::HTML("<input type='password' id='new_profile_password'>"),
          shiny::HTML("<span class='toggle-change-password' onclick='toggle_change_password()'>👁️</span>")
        )
      ),
      shiny::column(width = 12, shiny::uiOutput("change_profile_message"))
    ),
    footer = shiny::tagList(
      shinyjs::hidden(shiny::actionButton("save_change_email", shiny::strong("Save Email"), class = "btn-primary")),
      shinyjs::hidden(shiny::actionButton("save_change_password", shiny::strong("Save Password"), class = "btn-primary")),
      shiny::actionButton("change_email", shiny::strong("Change Email")),
      shiny::actionButton("change_password", shiny::strong("Change Password")),
      shiny::actionButton("dismiss_change_profile", shiny::strong("Cancel"))
    )
  )
}

# Returns the per-user connection handler and the signed-in user's row as
# reactives (NULL while signed out), plus the four dialog messages, which the
# tests read.
auth_server <- function(input, output, session, conn_handler,
                        search_user_fn = SigRepo::searchUser,
                        validate_fn = validateUser,
                        new_conn_fn = SigRepo::newConnHandler,
                        register_fn = register_new_user,
                        reset_fn = request_password_reset,
                        update_user_fn = SigRepo::updateUser) {
  user_conn_handler <- shiny::reactiveVal(NULL)
  user_login_info <- shiny::reactiveVal(NULL)

  login_message <- shiny::reactiveVal(NULL)
  register_message <- shiny::reactiveVal(NULL)
  forgot_message <- shiny::reactiveVal(NULL)
  profile_message <- shiny::reactiveVal(NULL)

  message_output <- function(message) {
    shiny::renderUI({
      shiny::req(message())
      shiny::p(class = "error-message", message())
    })
  }

  show_signed_in <- function(signed_in) {
    shinyjs::toggle(id = "login-wrapper", condition = !signed_in)
    shinyjs::toggle(id = "content-wrapper", condition = signed_in)
  }

  # ---- sign in and out ------------------------------------------------------

  shiny::observeEvent(input$sign_in_btn, {
    user_name <- trimmed(input$username)
    password <- trimmed(input$password)

    problem <- empty_problem(user_name, "'Username' cannot be empty") %||%
      signin_user_problem(search_user_fn(conn_handler = conn_handler, user_name = user_name), user_name) %||%
      empty_problem(password, "'Password' cannot be empty")
    if (!is.null(problem)) {
      login_message(problem)
      return()
    }

    # The handler every call for this user goes through. api_host and
    # api_port must be given: without them the client's own default address
    # is used, and getSignature() then fetches difexp tables from the wrong
    # deployment (which is how Annotate once matched 0% of every geneset).
    user_conn <- do.call(new_conn_fn, c(
      list(
        dbname = base::Sys.getenv("DB_NAME"),
        host = base::Sys.getenv("DB_HOST"),
        port = base::as.integer(base::Sys.getenv("DB_PORT")),
        user = user_name,
        password = password
      ),
      api_connection()
    ))

    user_tbl <- base::tryCatch(
      validate_fn(conn_handler = user_conn),
      error = function(e) {
        base::message("Sign-in for '", user_name, "' failed: ", conditionMessage(e))
        base::data.frame()
      }
    )

    if (!is.data.frame(user_tbl) || nrow(user_tbl) == 0) {
      login_message("Invalid username or password!")
      user_conn_handler(NULL)
      user_login_info(NULL)
      show_signed_in(FALSE)
      return()
    }

    login_message(NULL)
    user_conn_handler(user_conn)
    user_login_info(user_tbl)
    show_signed_in(TRUE)
  }, ignoreInit = TRUE)

  output$login_error_message <- shiny::renderUI({
    shiny::req(login_message())
    shiny::p(class = "error-message", id = "login-error-message", login_message())
  })

  output$welcome_msg <- shiny::renderUI({
    shiny::req(user_login_info())
    shiny::HTML(base::sprintf("%s", user_login_info()$user_name))
  })

  shiny::observeEvent(input$log_out_btn, {
    user_conn_handler(NULL)
    user_login_info(NULL)
    show_signed_in(FALSE)
  })

  # ---- register -------------------------------------------------------------

  shiny::observeEvent(input$register, {
    register_message(NULL)
    shiny::showModal(auth_register_modal())
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$dismiss_register, {
    shiny::removeModal()
    register_message(NULL)
  })

  shiny::observeEvent(input$register_user, {
    result <- register_fn(
      user_name = trimmed(input$register_username),
      password = trimmed(input$register_password),
      user_email = trimmed(input$register_email),
      user_first = trimmed(input$register_first_name),
      user_last = trimmed(input$register_last_name),
      user_affiliation = trimmed(input$register_affiliation)
    )
    register_message(result$reason)
    if (isTRUE(result$ok)) {
      base::message("Registered user '", trimmed(input$register_username), "' (inactive until an admin activates it)")
    }
  }, ignoreInit = TRUE)

  output$register_message <- message_output(register_message)

  # ---- forgot password ------------------------------------------------------

  shiny::observeEvent(input$forget_password, {
    forgot_message(NULL)
    shiny::showModal(auth_forgot_modal())
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$dismiss_forgot_password, {
    shiny::removeModal()
    forgot_message(NULL)
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$send_tmp_password, {
    identifier <- if (identical(input$psw_lookup_option, "Email")) {
      trimmed(input$psw_email)
    } else {
      trimmed(input$psw_username)
    }
    result <- reset_fn(identifier = identifier)
    forgot_message(result$reason)
  }, ignoreInit = TRUE)

  output$forgot_psw_message <- message_output(forgot_message)

  # ---- profile --------------------------------------------------------------

  shiny::observeEvent(input$edit_profile, {
    shiny::req(user_login_info())
    profile_message(NULL)
    shiny::showModal(auth_profile_modal(blank_for_na(user_login_info())))
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$change_email, {
    shinyjs::hide(id = "change_email")
    shinyjs::show(id = "save_change_email")
    shinyjs::show(id = "change-profile-email")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_change_email, {
    shiny::req(user_login_info())
    email <- trimmed(input$new_profile_email)

    problem <- empty_problem(email, "Email cannot be empty") %||%
      email_format_problem(email) %||%
      email_taken_problem(email, search_user_fn(conn_handler = conn_handler))
    if (!is.null(problem)) {
      profile_message(problem)
      return()
    }

    user_tbl <- user_login_info()
    update_user_fn(conn_handler = conn_handler, user_name = user_tbl$user_name[1], email = email)
    shiny::updateTextInput(session, "profile_email", value = email)
    user_tbl$user_email <- email
    user_login_info(user_tbl)
    profile_message("Your email has been changed and updated.")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$change_password, {
    shinyjs::hide(id = "change_password")
    shinyjs::show(id = "save_change_password")
    shinyjs::show(id = "change-profile-password")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_change_password, {
    shiny::req(user_conn_handler(), user_login_info())
    password <- trimmed(input$new_profile_password)

    problem <- empty_problem(password, "Password cannot be empty")
    if (!is.null(problem)) {
      profile_message(problem)
      return()
    }

    update_user_fn(conn_handler = conn_handler, user_name = user_login_info()$user_name[1], password = password)
    user_conn <- user_conn_handler()
    user_conn$password <- password
    user_conn_handler(user_conn)
    profile_message("Your password has been changed and updated.")
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$dismiss_change_profile, {
    shiny::removeModal()
    profile_message(NULL)
  }, ignoreInit = TRUE)

  output$change_profile_message <- message_output(profile_message)

  list(
    user_conn_handler = user_conn_handler,
    user_login_info = user_login_info,
    messages = list(
      login = login_message,
      register = register_message,
      forgot = forgot_message,
      profile = profile_message
    )
  )
}
