# app for modules

# loading in packages

#installing sigrepo_client and then validating it is the correct branch
# devtools::install_github("montilab/SigRepo@sigrepo_client")
# packageDescription("SigRepo")$GithubRef



# R packages for building shiny dashboard
library(shinyjs)
library(shiny)
library(DT)

# Package for knitting PDF
library(rmarkdown)

# Packages for API
library(httr)
library(jsonlite)

# Package for data cleaning, extraction and manipulation
library(dplyr)

# Package for plotting
library(ggplot2)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all(base::Sys.getenv("SIGREPO_DIR"))

# Loading OmicSignature package
devtools::load_all(base::Sys.getenv("OMICSIG_DIR"))

# Package for parallel processes
library(promises)
library(future)
future::plan(multisession)



# sourcing modules

source("modules/home_module.R")
source("modules/signature_module.R")
source("modules/collection_module.R")
source("modules/annotate_module.R")
source("modules/compare_module.R")
source("modules/reference_module.R")
source("modules/resource_module.R")

# sourcing modals 

source("modals/manage_users_modal.R")


# testing module
source("modules/test_module.R")
# utils
source("utils/utils.R")



# default connection handler for root, DONT USE IN MAIN APP

conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = as.integer(Sys.getenv("DB_PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

# UI scaffold

ui <- fluidPage(
  useShinyjs(),
  
  title = "SigRepo - Signature Repository",
  
  ### CSS and JS Tags
  shiny::tagList(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/app_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/sign_in_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/fontawesome-all.min.css"),
      tags$script(src = "assets/js/app.js", type = "text/javascript"),
      tags$style(
        HTML(
          "
    .navbar {
      background-color: #004080 !important;
      border-bottom: 3px solid #003366;
    }

    .navbar-default .navbar-nav > li > a {
      color: #ffffff !important;
      font-weight: 500;
      font-size: 15px;
    }

    .navbar-default .navbar-nav > li > a:hover,
    .navbar-default .navbar-nav > .active > a {
      background-color: #003366 !important;
      color: #ffffff !important;
    }

    .navbar-brand {
      display: flex;
      align-items: center;
    }

    .navbar-brand img {
      margin-right: 10px;
    }


    body {
  padding-top: 20px;
}





     .dataTables_wrapper {
    z-index: 0 !important;
  }

  #details_panel {
    z-index: 1050 !important;
  }

  .dataTables_filter,
  .dataTables_length,
  .dataTables_info,
  .dataTables_paginate {
    z-index: 0 !important;
  }

  .absolute-panel {
    position: fixed !important;
  }

  $(document).on('click', '.delete-btn', function() {
    var sigId = $(this).data('id');
    Shiny.setInputValue('delete_signature_id', sigId, {priority: 'event'});
  });

  "
        )
      )
    )
  ),
  
  ## JS TAGS ####
  
  tags$script(
    HTML(
      "
  $(document).on('click', '.sig-link', function(e) {
    e.preventDefault();
    const sig_id = $(this).data('sig_id');
    const sig_name = $(this).data('sig_name');
    Shiny.setInputValue('show_signature_info', {
      signature_id: sig_id,
      signature_name: sig_name,
      nonce: Math.random()  // force re-trigger even if same sig clicked
    });
  });
"
    )
  ),
  
  
  shiny::div(
    class = "login-wrapper",
    id = "login-wrapper",
    shiny::div(
      class = "login-container",
      
      shiny::div(class = "login-title", shiny::h2("Sign In")),
      
      tags$form(
        class = "login-form",
        
        shiny::div(
          class = "validate-input",
          shiny::HTML("<span class='login-label'><b>Username</b></span>"),
          shiny::div(
            class = "username-container",
            shiny::HTML(
              "<input class='login-input' type='text' id='username' onkeypress='login_keypress(e)' placeholder='Enter Username'>"
            )
          )
        ),
        
        shiny::div(
          class = "validate-input",
          shiny::HTML("<span class='login-label'><b>Password</b></span>"),
          shiny::div(
            class = "password-container",
            shiny::HTML(
              "<input class='login-input' type='password' id='password' onkeypress='login_keypress(e)' placeholder='Enter Password'>"
            ),
            shiny::HTML(
              "<span class='toggle-password' onclick='toggle_password()'>👁️</span>"
            )
          )
        ),
        
        shiny::div(class = "validate-message", shiny::uiOutput(outputId = "login_error_message")),
        
        shiny::div(
          class = "validate-button",
          shiny::actionButton(
            inputId = "sign_in_btn",
            class = "sign-in-button",
            label = "Login",
            onclick = "login_keypress(e)"
          ),
          shiny::div(
            class = "forgot_psw",
            shiny::HTML(
              "<a href='#' id='forget_password' class='action-button'>Forgot password?</a>"
            )
          ),
        ),
        
        shiny::div(class = "register", shiny::span(
          "Don't have an acount.",
          shiny::HTML(
            "<a href='#' id='register' class='action-button'>Register here!</a>"
          )
        ))
      )
    )
    
  ),
  shiny::div(
    id = "content-wrapper",
    style = "display:none;",
    
    navbarPage(
      title = div(
        style = "display: flex; align-items: center; width: 100%;",
        # Logo on the left
        tags$img(
          src = "images/logo.png",
          height = "50px",
          style = "margin-right: 10px;"
        ),
        # Spacer pushes user links to right
        
        # User controls on the right
        div(
          style = "display: flex; align-items: center; gap: 15px; color: white;",
          actionLink(
            inputId = "edit_profile",
            label = NULL,
            icon = icon("user-circle")
          ),
          uiOutput(outputId = "welcome_msg"),
          actionLink(
            inputId = "log_out_btn",
            class = "button-link",
            icon = tags$i(class = "fa fa-sign-out"),
            label = strong("Log out")
          )
        )
      ),
      id = "main_navbar",
      position = "fixed-top",
      
      tabPanel("Home", home_module_ui("home")),
      tabPanel("Signatures", signature_module_ui("signatures")),
      tabPanel("Collections", collection_module_ui("collections")),
      tabPanel("Annotate", annotate_module_ui("annotate")),
      tabPanel("Compare", compare_module_ui("compare")),
      tabPanel("References", reference_module_ui("references")),
      tabPanel("Resources", resource_module_ui("resources")),
      
      
      # custom logo for longevity consortium 
      
      
      header = tagList(
        tags$style(HTML("
                      .navbar-custom-logo {

                      position: absolute;
                      right: 20px;
                      top: 10px;
                      height: 40px;
                      }"
                        
        )),
      ),
      
      # inject image to navbar
      
      tags$img(
        src = "images/longevityconsortiumlogo.svg",
        class = "navbar-custom-logo"
      )
      
      
      
    )
  )
  
)



server <- function(input, output, session) {
  #### LOGIN AND CONNECTION HANDLER SERVER LOGIC ####
  
  
  login_error_message <- shiny::reactiveVal()
  
  # Create reactive values to store user login information
  user_conn_handler <- shiny::reactiveVal()
  user_login_info <- shiny::reactiveVal()
  
  shinyjs::hide("oms_download_wrapper")
  
  # reactive values for triggers to refresh tables
  signature_trigger <- reactiveVal(0)
  collection_trigger <- reactiveVal(0)
  
  
  # Print this when a session starts ####
  base::cat("\nSession started.\n")
  
  # Anything that calls autoInvalidate will automatically invalidate every 2 seconds.
  autoInvalidate <- shiny::reactiveTimer(2000)
  
  # Prevent Shiny from graying out ####
  shiny::observe({
    autoInvalidate()
    base::cat(".")
  })
  
  # Observe when session ends ####
  session$onSessionEnded(function(x) {
    base::cat("\nSession ended.\n")
    
  })
  
  # Observe when session stops ####
  shiny::onStop(function() {
    base::cat("\nSession stopped.\n")
    
  })
  
  
  
  # Observe when sign in button is clicked #####
  shiny::observeEvent({
    input$sign_in_btn
  }, {
    # Get user name and password
    user_name <- shiny::isolate({
      input$username
    }) %>% base::trimws()
    user_password <- shiny::isolate({
      input$password
    }) %>% base::trimws()
    
    # Check user name
    if (user_name %in% c(NA, "")) {
      login_error_message("'Username' cannot be empty")
      return(NULL)
      
    } else{
      # Check user table
      check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
      
      # If user exists, throw an error
      if (nrow(check_user_tbl) == 0) {
        login_error_message(base::sprintf("Invalid username or password!"))
        return(NULL)
      } else if (nrow(check_user_tbl) > 0 &&
                 check_user_tbl$active[1] == 0) {
        login_error_message(
          base::sprintf(
            "User = '%s' is currently inactive in our database. Please contact our admin to activate it.",
            user_name
          )
        )
        return(NULL)
      }
      
    }
    
    # Check user_password
    if (user_password %in% c(NA, "")) {
      login_error_message("'Password' cannot be empty")
      return(NULL)
    }
    
    # Create a user connection handler
    user_conn_handler(
      SigRepo::newConnHandler(
        dbname = Sys.getenv("DB_NAME"),
        host = Sys.getenv("DB_HOST"),
        port = as.integer(Sys.getenv("DB_PORT")),
        user = user_name,
        password = user_password
      )
    )
    
    # Validate user
    user_tbl <- base::tryCatch({
      SigRepo::searchUser(conn_handler = user_conn_handler(), user_name = user_name)
    }, error = function(e) {
      base::print(e, "\n")
      return(base::data.frame(NULL))
    })
    
    # Check if conn is a MySQLConnection class object
    if (nrow(user_tbl) == 0) {
      # Update message
      login_error_message(base::sprintf("Invalid username or password!"))
      user_conn_handler(NULL)
      user_login_info(NULL)
      user_signature_tbl(NULL)
      user_collection_tbl(NULL)
      shinyjs::show(id = "login-wrapper")
      shinyjs::hide(id = "content-wrapper")
      
    } else{
      # Update message ####
      login_error_message(NULL)
      
      # Update URL search string ####
      shiny::updateQueryString(
        session,
        queryString = sprintf(
          "#user_id=%s&token=%s",
          user_name,
          digest::digest(user_password, algo = "md5", serialize = TRUE)
        ),
        mode = "push"
      )
      
      # Get user connection info ####
      user_conn_handler(user_conn_handler())
      
      # Get user login info ####
      user_login_info(user_tbl)
      
      
      
      # Hide login wrapper
      shinyjs::hide(id = "login-wrapper")
      
      # Display app content
      shinyjs::show(id = "content-wrapper")
      
    }
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Output login_error_message ####
  output$login_error_message <- shiny::renderUI({
    req(login_error_message())
    
    shiny::p(class = "error-message", id = "login-error-message", login_error_message())
    
  })
  
  # Welcome message ####
  output$welcome_msg <- shiny::renderUI({
    req(user_login_info())
    
    # Get user connection info
    shiny::HTML(base::sprintf("%s", user_login_info()$user_name))
    
  })
  
  # Observe when sign out buttion is clicked
  shiny::observeEvent({
    input$log_out_btn
  }, {
    user_conn_handler(NULL)
    user_login_info(NULL)
    shinyjs::hide(id = "content-wrapper")
    shinyjs::show(id = "login-wrapper")
    shiny::updateQueryString(session, queryString = "#login", mode = "push")
    
  })
  
  # Create reactive to store messages
  register_message <- shiny::reactiveVal()
  forgot_psw_message <- shiny::reactiveVal()
  change_profile_message <- shiny::reactiveVal()
  
  # OBSERVE REGISTER BUTTON #####
  shiny::observeEvent({
    input$register
  }, {
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        title = shiny::span(
          shiny::icon(name = "user-plus", lib = "font-awesome"),
          "REGISTER USER"
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::textInput(
              inputId = "register_username",
              label = shiny::strong(shiny::span(style = "color: red;", "*"), "Username"),
              value = "",
              placeholder = "Enter Username",
              width = "100%"
            )
          ),
          shiny::column(
            width = 12,
            id = "register-password",
            shiny::strong(shiny::span(style = "color: red;", "*"), "Password"),
            #shiny::HTML("<b><span style='color: red;'>*<span> Password</b>"),
            shiny::div(
              class = "register-password",
              shiny::HTML(
                "<input type='password' id='register_password' placeholder='Enter Password'>"
              ),
              shiny::HTML(
                "<span class='toggle-register-password' onclick='toggle_register_password()'>👁️</span>"
              )
            )
          ),
          shiny::column(
            width = 12,
            shiny::textInput(
              inputId = "register_email",
              label = shiny::strong(shiny::span(style = "color: red;", "*"), "Email"),
              value = "",
              placeholder = "Enter Email",
              width = "100%"
            ),
            shiny::textInput(
              inputId = "register_first_name",
              label = shiny::strong("First Name"),
              value = "",
              placeholder = "Enter First Name",
              width = "100%"
            ),
            shiny::textInput(
              inputId = 'register_last_name',
              label = shiny::strong("Last Name"),
              value = "",
              placeholder = "Enter Last Name",
              width = "100%"
            ),
            shiny::textInput(
              inputId = 'register_affiliation',
              label = shiny::strong("Affiliation"),
              value = "",
              placeholder = "Enter Affiliation",
              width = "100%"
            ),
            shiny::uiOutput(outputId = "register_message")
          )
        ),
        footer = shiny::tagList(
          shiny::actionButton(
            inputId = "register_user",
            class = "primary-btn",
            label = shiny::strong("Register")
          ),
          shiny::actionButton(
            inputId = "dismiss_register",
            class = "primary-btn",
            label = shiny::strong("Cancel")
          )
        )
      )
    )
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## OBSERVE DISMISS REGISTER BUTTON #####
  shiny::observeEvent({
    input$dismiss_register
  }, {
    shiny::removeModal()
    register_message(NULL)
    
  })
  
  # OBSERVE REGISTER USER BUTTON #####
  shiny::observeEvent({
    input$register_user
  }, {
    # Get user name and password
    user_name <- shiny::isolate({
      input$register_username
    }) %>% base::trimws()
    user_password <- shiny::isolate({
      input$register_password
    }) %>% base::trimws()
    user_email <- shiny::isolate({
      input$register_email
    }) %>% base::trimws()
    user_first <- shiny::isolate({
      input$register_first_name
    }) %>% base::trimws()
    user_last <- shiny::isolate({
      input$register_last_name
    }) %>% base::trimws()
    user_affiliation <- shiny::isolate({
      input$register_affiliation
    }) %>% base::trimws()
    
    # Check user name
    if (user_name %in% c(NA, "")) {
      register_message("'Username' cannot be empty")
      return(NULL)
      
    } else{
      # Check user table
      check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
      
      # If user exists, throw an error
      if (nrow(check_user_tbl) > 0 &&
          check_user_tbl$active[1] == 0) {
        register_message(
          base::sprintf(
            "User = '%s' is already existed in our database and currently inactive. If this is your account, please contact our admin to activate it.",
            user_name
          )
        )
        return(NULL)
      } else if (nrow(check_user_tbl) > 0 &&
                 check_user_tbl$active[1] == 1) {
        register_message(
          base::sprintf(
            "User = '%s' is already existed in our database. Please choose a different name.",
            user_name
          )
        )
        return(NULL)
      }
      
    }
    
    # Check user name
    if (user_password %in% c(NA, "")) {
      register_message("'Password' cannot be empty")
      return(NULL)
    }
    
    # Check user name
    if (user_email %in% c(NA, "")) {
      register_message("'Email' cannot be empty")
      return(NULL)
      
    } else{
      # Check user emails ####
      check_email <- base::grepl(
        "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        base::as.character(user_email),
        ignore.case = TRUE
      )
      
      # If any emails do not have correct format, throw an error message
      if (check_email == FALSE) {
        register_message("Invalid email format.")
        return(NULL)
      }
      
      # Check user table
      check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
      
      # If user exists, throw an error
      if (base::tolower(user_email) %in% base::tolower(check_email_tbl$user_email)) {
        register_message(
          base::sprintf(
            "Email = '%s' already existed in our database. Please choose a different email.",
            user_email
          )
        )
        return(NULL)
      }
      
    }
    
    # Create a new user table to add to database
    user_tbl <- base::data.frame(
      user_name = user_name,
      user_password = user_password,
      user_email = user_email,
      user_first = user_first,
      user_last = user_last,
      user_affiliation = user_affiliation,
      user_role = "editor",
      active = 0,
      stringsAsFactors = FALSE
    ) %>%
      base::replace(. == "NA", "") %>%
      base::replace(. == "NULL", "") %>%
      base::replace(. == "", "") %>%
      base::replace(is.na(.), "") %>%
      base::replace(is.null(.), "")
    
    # Add user to database
    SigRepo::addUser(conn_handler = conn_handler, user_tbl = user_tbl)
    
    # Send email to admin
    api_url <- base::sprintf(
      "https://montilab.bu.edu/SigRepo/send_notifications/register_user?user_name=%s&api_key=%s",
      user_tbl$user_name[1],
      base::Sys.getenv("API_KEY")
    )
    
    # Send email to users through montilab server API
    res <- httr::GET(url = api_url)
    
    # Check status code
    if (res$status_code != 200) {
      register_message(
        base::sprintf(
          "Something went wrong with the API. Cannot register user. Please contact admin for support."
        )
      )
      return(NULL)
    } else{
      register_message(
        base::sprintf(
          "Thank you for signing up! Our administrator will contact you once your account is activated."
        )
      )
    }
    
    # Print message
    base::print(base::sprintf("Adding user = '%s' to database", user_tbl$user_name[1]))
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Output register_message ####
  output$register_message <- shiny::renderUI({
    req(register_message())
    
    shiny::p(class = "error-message", shiny::isolate({
      register_message()
    }))
    
  })
  
  # OBSERVE FORGOT PASSWORD BUTTON #####
  shiny::observeEvent({
    input$forget_password
  }, {
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        title = NULL,
        
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::h3("Forgot your password?", class = "text-center"),
            shiny::br(),
            shiny::p(
              shiny::strong(
                "To access your account, please fill in the following information:"
              )
            ),
            shiny::br(),
            shiny::radioButtons(
              inputId = "psw_lookup_option",
              label = NULL,
              choices = c("Username", "Email"),
              inline = TRUE
            ),
            shiny::conditionalPanel(
              condition = 'input.psw_lookup_option == "Username"',
              shiny::textInput(
                inputId = "psw_username",
                label = shiny::strong("Enter Your Username"),
                value = "",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              condition = 'input.psw_lookup_option == "Email"',
              shiny::textInput(
                inputId = "psw_email",
                label = shiny::strong("Enter Your Email"),
                value = "",
                width = "100%"
              )
            ),
            shiny::uiOutput("forgot_psw_message")
          )
        ),
        
        footer = shiny::tagList(
          shiny::actionButton(
            inputId = "send_tmp_password",
            class = "primary-btn",
            label = shiny::strong("Submit")
          ),
          shiny::actionButton(
            inputId = "dismiss_forgot_password",
            class = "primary-btn",
            label = shiny::strong("Cancel")
          )
        )
      )
    )
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # OBSERVE DISMISS FORGOT PASSWORD BUTTON #####
  shiny::observeEvent({
    input$dismiss_forgot_password
  }, {
    shiny::removeModal()
    forgot_psw_message(NULL)
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # OBSERVE SEND TMP PASSWORD BUTTON #####
  shiny::observeEvent({
    input$send_tmp_password
  }, {
    # Get user name and
    psw_lookup_option <- shiny::isolate({
      input$psw_lookup_option
    })
    
    if (psw_lookup_option == "Username") {
      user_name <- shiny::isolate({
        input$psw_username
      }) %>% base::trimws()
      
      # Make sure user_name is not empty
      if (user_name %in% c(NA, "")) {
        forgot_psw_message(base::sprintf("Username cannot be empty"))
        return(NULL)
      }
      
      # Check user table
      user_tbl <- check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
      
      # If user does not exists, throw an error
      if (nrow(check_user_tbl) == 0) {
        forgot_psw_message(
          base::sprintf(
            "User = '%s' does not exist in our database. Please choose a different name.",
            user_name
          )
        )
        return(NULL)
      }
      
    } else{
      user_email <- shiny::isolate({
        input$psw_email
      }) %>% base::trimws()
      
      # Make sure email is not empty
      if (user_email %in% c(NA, "")) {
        forgot_psw_message(base::sprintf("Email cannot be empty"))
        return(NULL)
      }
      
      # Check user emails ####
      check_email <- base::grepl(
        "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        base::as.character(user_email),
        ignore.case = TRUE
      )
      
      # If any emails do not have correct format, throw an error message
      if (check_email == FALSE) {
        forgot_psw_message("Invalid email format.")
        return(NULL)
      }
      
      # Check user table
      check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
      
      # If user exists, throw an error
      if (!base::tolower(user_email) %in% base::tolower(check_email_tbl$user_email)) {
        forgot_psw_message(
          base::sprintf(
            "Email = '%s' does not exist in our database. Please choose a different email.",
            user_email
          )
        )
        return(NULL)
      }
      
      # Return user_tbl
      user_tbl <- check_email_tbl %>% dplyr::filter(base::tolower(user_email) %in% base::tolower(!!user_email))
      
    }
    
    # Send email to users to notify their account are activated
    api_url <- base::sprintf(
      "https://montilab.bu.edu/SigRepo/send_notifications/send_tmp_password?user_name=%s&api_key=%s",
      user_tbl$user_name[1],
      base::Sys.getenv("API_KEY")
    )
    
    # Send email to users through montilab server API
    res <- httr::GET(url = api_url)
    
    # Check status code
    if (res$status_code != 200) {
      forgot_psw_message(
        base::sprintf(
          "Something went wrong with the API. Cannot send temporary password to user. Please contact admin for support."
        )
      )
      return(NULL)
    } else{
      forgot_psw_message(
        base::sprintf(
          "A temporary password has been sent to your email at %s",
          user_tbl$user_email[1]
        )
      )
    }
    
    # Print message
    base::print(base::sprintf(
      "Sending temporary password to user = '%s'",
      user_tbl$user_name[1]
    ))
    
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Output forgot_psw_message
  output$forgot_psw_message <- shiny::renderUI({
    req(forgot_psw_message())
    
    shiny::p(class = "error-message", shiny::isolate({
      forgot_psw_message()
    }))
    
  })
  
  ## OBSERVE EDIT PROFILE BUTTON #####
  shiny::observeEvent({
    input$edit_profile
  }, {
    req(user_login_info())
    
    # Get user table
    user_tbl <- shiny::isolate({
      user_login_info()
    }) %>%
      base::replace(. == "NA", "") %>%
      base::replace(. == "NULL", "") %>%
      base::replace(. == "", "") %>%
      base::replace(is.na(.), "") %>%
      base::replace(is.null(.), "")
    
    # Show the modal dialog
    shiny::showModal(
      shiny::modalDialog(
        size = "l",
        title = shiny::span(
          shiny::icon(name = "user", lib = "font-awesome"),
          "USER PROFILE"
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::p(shiny::strong("Username: "), user_tbl$user_name[1]),
            shiny::p(
              shiny::strong("Email: "),
              shiny::HTML(
                base::paste0(
                  "<input type='text' id='profile_email' value='",
                  user_tbl$user_email[1]
                ),
                "' disabled='disabled'>"
              )
            ),
            shiny::p(class = "profile-bullet", shiny::strong("Role: "), user_tbl$user_role[1]),
            shiny::p(class = "profile-bullet", shiny::strong("API Key: "), user_tbl$api_key[1])
          ),
          shiny::column(
            width = 6,
            shiny::p(
              class = "profile-bullet",
              shiny::strong("First Name: "),
              user_tbl$user_first[1]
            ),
            shiny::p(class = "profile-bullet", shiny::strong("Last Name: "), user_tbl$user_last[1]),
            shiny::p(
              class = "profile-bullet",
              shiny::strong("Affliliation: "),
              user_tbl$user_affiliation[1]
            )
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
              shiny::HTML(
                "<span class='toggle-change-password' onclick='toggle_change_password()'>👁️</span>"
              )
            )
          ),
          shiny::column(
            width = 12,
            shiny::uiOutput(outputId = "change_profile_message")
          )
        ),
        footer = shiny::tagList(
          shinyjs::hidden(
            shiny::actionButton(
              inputId = "save_change_email",
              class = "primary-btn",
              label = shiny::strong("Save Email")
            )
          ),
          shinyjs::hidden(
            shiny::actionButton(
              inputId = "save_change_password",
              class = "primary-btn",
              label = shiny::strong("Save Password")
            )
          ),
          shiny::actionButton(
            inputId = "change_email",
            class = "primary-btn",
            label = shiny::strong("Change Email")
          ),
          shiny::actionButton(
            inputId = "change_password",
            class = "primary-btn",
            label = shiny::strong("Change Password")
          ),
          shiny::actionButton(
            inputId = "dismiss_change_profile",
            class = "primary-btn",
            label = shiny::strong("Cancel")
          )
        )
      )
    )
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## OBSERVE CHANGE EMAIL BUTTON #####
  shiny::observeEvent({
    input$change_email
  }, {
    shinyjs::hide(id = "change_email")
    shinyjs::show(id = "save_change_email")
    shinyjs::show(id = "change-profile-email")
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## OBSERVE SAVE CHANGE EMAIL BUTTON #####
  shiny::observeEvent({
    input$save_change_email
  }, {
    req(user_login_info())
    
    # Get user table
    user_tbl <- shiny::isolate({
      user_login_info()
    })
    
    # Get user inputs
    user_email <- shiny::isolate({
      input$new_profile_email
    }) %>% base::trimws()
    
    # Make sure email is not empty
    if (user_email %in% c(NA, "")) {
      change_profile_message(base::sprintf("Email cannot be empty"))
      return(NULL)
    }
    
    # Check user emails ####
    check_email <- base::grepl(
      "\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
      base::as.character(user_email),
      ignore.case = TRUE
    )
    
    # If any emails do not have correct format, throw an error message
    if (check_email == FALSE) {
      change_profile_message("Invalid email format.")
      return(NULL)
    }
    
    # Check user table
    check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
    
    # If user exists, throw an error
    if (base::tolower(user_email) %in% base::tolower(check_email_tbl$user_email)) {
      change_profile_message(
        base::sprintf(
          "Email = '%s' already existed in our database. Please choose a different email.",
          user_email
        )
      )
      return(NULL)
    }
    
    # Update user email in the database
    SigRepo::updateUser(
      conn_handler = conn_handler,
      user_name = user_tbl$user_name[1],
      email = user_email
    )
    
    # Update user email in profile
    shiny::updateTextInput(session = session,
                           inputId = "profile_email",
                           value = user_email)
    
    # Update user information
    user_login_info(user_tbl %>% dplyr::mutate(user_email = !!user_email))
    
    # Update message
    change_profile_message(base::sprintf("Your email has been changed and updated."))
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## OBSERVE CHANGE PASSWORD BUTTON #####
  shiny::observeEvent({
    input$change_password
  }, {
    shinyjs::hide(id = "change_password")
    shinyjs::show(id = "save_change_password")
    shinyjs::show(id = "change-profile-password")
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## OBSERVE SAVE CHANGE PASSWORD BUTTON #####
  shiny::observeEvent({
    input$save_change_password
  }, {
    req(user_conn_handler(), user_login_info())
    
    # Get user table
    user_tbl <- shiny::isolate({
      user_login_info()
    })
    
    # Get user connection handler
    user_conn_handler <- shiny::isolate({
      user_conn_handler()
    })
    
    # Get user inputs
    user_password <- shiny::isolate({
      input$new_profile_password
    }) %>% base::trimws()
    
    # Make sure password is not empty
    if (user_password %in% c(NA, "")) {
      change_profile_message(base::sprintf("Password cannot be empty"))
      return(NULL)
    }
    
    # Update user password in the database
    SigRepo::updateUser(
      conn_handler = conn_handler,
      user_name = user_tbl$user_name[1],
      password = user_password
    )
    
    # Update handler
    user_conn_handler$password <- user_password
    
    # Update user information
    user_conn_handler(user_conn_handler)
    
    # Update message
    change_profile_message(base::sprintf("Your password has been changed and updated."))
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  ## OBSERVE DISMISS BUTTON #####
  shiny::observeEvent({
    input$dismiss_change_profile
  }, {
    shiny::removeModal()
    change_profile_message(NULL)
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Output change_profile_message ####
  output$change_profile_message <- shiny::renderUI({
    req(change_profile_message())
    
    shiny::p(class = "error-message", shiny::isolate({
      change_profile_message()
    }))
  })
  
  ### signature db ###
  
  signature_db <- reactive({
    # only runs when user_conn_handler is available
    req(user_conn_handler())
    signature_trigger()
   
    
    tryCatch({
      df <- SigRepo::searchSignature(conn_handler = user_conn_handler())
      
      if(nrow(df) == 0){
        showNotification("No Signatures Found.", type = "warning")
        return(data.frame())
      }
      df
      
    }, error = function(e) {
      showNotification(paste("Error fetching signatures:", e$message), type = "error")
      data.frame()
    })
  })
  
  
  
  #### collection db ###
  
  collection_db <- reactive({
    req(user_conn_handler())
    collection_trigger()
      # Avoid calling reactive multiple times
    
    tryCatch({
      df <- SigRepo::searchCollection(conn_handler = user_conn_handler())
      
      if (nrow(df) == 0) {
        showNotification("No collections found.", type = "warning")
        return(data.frame())  # Or return(NULL), depending on your downstream logic
      }
      
      df
    }, error = function(e) {
      showNotification(paste("Error fetching Collections:", e$message), type = "error")
      data.frame()
    })
  })
  
  
  
  ### SERVER MODULES ####
  
  # testing module
  test_module_server("test1", signature_db = signature_db)
  
  
  observe({
    
    req(user_conn_handler())
    
    # Now that user_conn_handler() is available, initialize the modules
    
    home_module_server(
      "home", 
      signature_db = signature_db)
    
    
    signature_module_server(
      "signatures",
      signature_db = signature_db,
      user_conn_handler = user_conn_handler,
      signature_trigger = signature_trigger
    )
    
    collection_module_server(
      "collections",
      collection_db = collection_db,
      user_conn_handler = user_conn_handler,
      collection_trigger = collection_trigger
    )
    annotate_module_server(
      "annotate",
      signature_db = signature_db,
      user_conn_handler = user_conn_handler)
    
   
    reference_module_server("references", user_conn_handler = user_conn_handler)
    
    
    # in progress modules
    #resourcesServer("resources")
    # compareServer("compare") 
  })
  
} # end bracket, dont touch !!!

shinyApp(ui, server)
