
# # R packages for building shiny dashboard
library(shinyjs)
library(shiny)
library(DT)

# For DB connection
library(RMySQL)
library(DBI)

# Package for data cleaning, extraction and manipulation
library(tidyverse)

# Package for loading and installing packages
library(devtools)
devtools::load_all()

# Package to sendmail
library(sendmailR)

# Create a default database handler
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"),
  host = Sys.getenv("HOST"),
  port = as.integer(Sys.getenv("PORT")),
  user = Sys.getenv("USER"),
  password = Sys.getenv("PASSWORD")
)

# Function to generate random password
randPassword <- function(n = 1) {
  a <- base::do.call(base::paste0, base::replicate(5, base::sample(LETTERS, n, TRUE), FALSE))
  base::paste0(a, base::sprintf("%04d", base::sample(9999, n, TRUE)), base::sample(LETTERS, n, TRUE))
}

# Function to send registered users to admin
registerUser <- function(
    from_sender = "montilab@bu.edu",
    user_name = "rchau88",
    user_password = "123456789",
    user_email = "rchau88@bu.edu",
    user_first = "Reina",
    user_last = "Chau",
    user_affiliation = "Boston University",
    api_key = "dienfkdingnkgggiidndkkdidingnn"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>REGISTER USER</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>SigRepo Admin,</strong></p>',
      '<br>',
      '<p>A user has registered to gain access to the <strong>SigRepo</strong> database.</p>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Password: <strong>', user_password, '</strong></p>',
      '<p>Email: <strong>', user_email, '</strong></p>',
      '<p>First: <strong>', user_first, '</strong></p>',
      '<p>Last: <strong>', user_last, '</strong></p>',
      '<p>Affliation: <strong>', user_affiliation, '</strong></p>',
      '<br>',
      '<p>To give access to this user, please click on this API link below.</p>',
      '<p><strong>https://sigrepo.org/api/activate_user/?api_key=', api_key, '&user_name=', user_name, '</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Register User Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

# Function to send email to registered users
notifyUser <- function(
    from_sender = "rchau88@bu.edu",
    user_name = "rchau88",
    user_password = "123456789",
    user_email = "rchau88@bu.edu",
    user_first = "Reina",
    user_last = "Chau",
    user_affiliation = "Boston University"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>SIGREPO</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', user_name, ',</strong></p>',
      '<br>',
      '<p>Thank you for signing up to use our <strong>SigRepo</strong> database! Our administrator will contact you as soon as they had reviewed and activated our account.</p>',
      '<br>',
      '<p>Here is the registration information you submitted:</p>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Password: <strong>', user_password, '</strong></p>',
      '<p>Email: <strong>', user_email, '</strong></p>',
      '<p>First: <strong>', user_first, '</strong></p>',
      '<p>Last: <strong>', user_last, '</strong></p>',
      '<p>Affliation: <strong>', user_affiliation, '</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Account Creation Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

# Function to send temporary password to user
sendPassword <- function(
    from_sender = "rchau88@bu.edu",
    user_name = "rchau88",
    temp_password = "123456789",
    user_email = "rchau88@bu.edu"
){
  
  msg <- sendmailR::mime_part(
    base::paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>SIGREPO</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', user_name, '</strong></p>',
      '<br>',
      '<p>Please find below a temporary password for accessing the <strong>SigRepo</strong> database.</p>',
      '<br>',
      '<p>Username: <strong>', user_name, '</strong></p>',
      '<p>Temporary password: <strong>', temp_password, '</strong></p>',
      '<br>',
      '<p>To log back in? Follow this link, <strong>https://sigrepo.org/</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>'
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- base::paste0("\"Montilab Team\"<", from_sender, ">")
  to <- base::paste0("<", user_email, ">")
  subject <- "SigRepo Temporary Password Do Not Reply"
  msg <- base::list(msg)
  sendmailR::sendmail(from = from, to = to, subject = subject, msg = msg, control = base::list(smtpServer = "smtp.bu.edu", smtpPort = "25"))
  
}

## Create a modal dialog for forgot password #####
forgotPasswordDialog <- function() {
  shiny::modalDialog(
    size = "l", title = NULL,
    shiny::fluidRow(
      shiny::column(
        width = 12,
        shiny::h3("Forgot your password?", class="text-center"),
        shiny::br(),
        shiny::p(strong("To access your account, please fill in the following information:")),
        shiny::br(),
        shiny::radioButtons(inputId = "psw_lookup_option", label = NULL, choices = c("Username", "Email"), inline = TRUE),
        shiny::conditionalPanel(
          condition = 'input.psw_lookup_option == "Username"',
          shiny::textInput(inputId = "psw_username", label = "Enter your username:", value = "", width = "100%")
        ),
        shiny::conditionalPanel(
          condition = 'input.psw_lookup_option == "Email"',
          shiny::textInput(inputId = "psw_email", label = "Enter your email:", value = "", width = "100%")
        ),
        shiny::uiOutput("forgot_psw_message")
      )
    ),
    footer = shiny::tagList(
      shiny::actionButton(inputId = "send_tmp_password", label = shiny::strong("Submit")),
      shiny::actionButton(inputId = "dismiss_password", label = shiny::strong("Cancel"))
    )
  )
}

## Define ui logic ####
ui <- shiny::bootstrapPage(
  
  title = "SigRepo - Sign up",

  ### CSS and JS ####
  shiny::tagList(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/main.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/sign_in_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/fontawesome-all.min.css"),
      tags$script(src = "assets/js/app.js", type = "text/javascript")
    )
  ),

  shinyjs::useShinyjs(),

  ### Login Page ####
  shiny::div(
    class = "login-wrapper", id = "login-wrapper",
    shiny::div(
      class = "login-container",

      shiny::div(class = "login-title", shiny::h2("Register Form")),

      tags$form(
        class = "login-form",

        shiny::div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Username</b></span>"),
          shiny::div(
            class = "username-container",
            HTML("<input class='login-input' type='text' id='username' onkeypress='login_keypress(e)' placeholder='Enter Username' required>")
          )
        ),

        shiny::div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Password</b></span>"),
          shiny::div(
            class = "password-container",
            HTML("<input class='login-input' type='password' id='password' onkeypress='login_keypress(e)' placeholder='Enter Password' required>"),
            HTML("<span class='toggle-password' onclick='toggle_password()'>👁️</span>")
          )
        ),

        shiny::div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Email</b></span>"),
          shiny::div(
            class = "username-container",
            HTML("<input class='login-input' type='text' id='email' onkeypress='login_keypress(e)' placeholder='Enter Email' required>")
          )
        ),

        shiny::div(
          class = "validate-input",
          HTML("<span class='login-label'><b>First Name</b></span>"),
          shiny::div(
            class = "username-container",
            HTML("<input class='login-input' type='text' id='first_name' onkeypress='login_keypress(e)' placeholder='Enter First Name' required>")
          )
        ),

        shiny::div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Last Name</b></span>"),
          shiny::div(
            class = "username-container",
            HTML("<input class='login-input' type='text' id='last_name' onkeypress='login_keypress(e)' placeholder='Enter Last Name' required>")
          )
        ),

        shiny::div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Affiliation</b></span>"),
          shiny::div(
            class = "username-container",
            HTML("<input class='login-input' type='text' id='affiliation' onkeypress='login_keypress(e)' placeholder='Enter Affiliation' required>")
          )
        ),

        shiny::div(
          class = "validate-message",
          shiny::uiOutput(outputId = "login_error_message")
        ),

        shiny::div(
          class = "validate-button",
          shiny::actionButton(inputId = "register_user", class = "sign-in-button", label = "Register", onclick = "login_keypress(e)"),
          shiny::div(class = "forgot_psw", HTML("<a href='#' id='forget_password' class='action-button'>Forgot password?</a>")),
        ),

        shiny::div(
          class = "register",
          shiny::p(HTML("<b>&copy; Monti Lab &diams; <script>document.write(new Date().getFullYear());</script> &diams; All Rights Reserved.</b>"))
        )
      )
    )
  )

)

## Define server logic ####
server <- function(input, output, session) { 
  
  # Create reactive to store messages
  login_error_message <- shiny::reactiveVal()
  forgot_psw_message <- shiny::reactiveVal()

  # Observe register_user ####
  shiny::observeEvent({
    input$register_user
  }, {

    # Get user name and password
    user_name <- shiny::isolate({ input$username }) %>% base::trimws()
    user_password <- shiny::isolate({ input$password }) %>% base::trimws()
    user_email <- shiny::isolate({ input$email }) %>% base::trimws()
    user_first <- shiny::isolate({ input$first_name }) %>% base::trimws()
    user_last <- shiny::isolate({ input$last_name }) %>% base::trimws()
    user_affiliation <- shiny::isolate({ input$affiliation }) %>% base::trimws()

    # Check user name
    if(user_name %in% c(NA, "")){

      login_error_message("'Username' cannot be empty")
      return(NULL)

    }else{

      # Check user table
      check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)

      # If user exists, throw an error
      if(nrow(check_user_tbl) > 0){
        login_error_message(base::sprintf("User = '%s' already existed in our database. Please choose a different name.", user_name))
        return(NULL)
      }

    }

    # Check user name
    if(user_password %in% c(NA, "")){
      login_error_message("'Password' cannot be empty")
      return(NULL)
    }

    # Check user name
    if(user_email %in% c(NA, "")){

      login_error_message("'Email' cannot be empty")
      return(NULL)

    }else{

      # Check user emails ####
      check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(user_email), ignore.case = TRUE)

      # If any emails do not have correct format, throw an error message
      if(check_email == FALSE){
        login_error_message("Invalid email format.")
        return(NULL)
      }

      # Check user table
      check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)

      # If user exists, throw an error
      if(user_email %in% check_email_tbl$user_email){
        login_error_message(base::sprintf("Email = '%s' already existed in our database. Please choose a different email.", user_email))
        return(NULL)
      }

    }

    # Create a new user table to add to database
    new_user_tbl <- base::data.frame(
      user_name = user_name,
      user_password = user_password,
      user_email = user_email,
      user_first = user_first,
      user_last = user_last,
      user_affiliation = user_affiliation,
      user_role = "editor",
      active = 0,
      stringsAsFactors = FALSE
    )

    # Add user to database
    SigRepo::addUser(conn_handler = conn_handler, user_tbl = new_user_tbl)

    # Send email to admin
    registerUser(
      user_name = "rchau88",
      user_password = "123456789",
      user_email = "rchau88@bu.edu",
      user_first = "Reina",
      user_last = "Chau",
      user_affiliation = "Boston University",
      api_key = Sys.getenv("API_KEY")
    )
    
    # Send email to registered user
    notifyUser(
      from_sender = "rchau88@bu.edu",
      user_name = "rchau88",
      user_password = "123456789",
      user_email = "rchau88@bu.edu",
      user_first = "Reina",
      user_last = "Chau",
      user_affiliation = "Boston University"
    )
    
    # Update message
    login_error_message(base::sprintf("Thank you for signing up! Our administrator will contact you on how to access our database.", user_email))

    # Print message
    base::print(base::sprintf("Adding user = '%s' to database", user_name))

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # output login_error_message
  output$login_error_message <- shiny::renderUI({

    req(login_error_message())

    shiny::p(class = "error-message", id = "login-error-message", shiny::isolate({ login_error_message() }))

  })

  # Observe forget_password
  shiny::observeEvent({
    input$forget_password
  }, {

    shiny::showModal(forgotPasswordDialog())

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Observe dismiss_password
  shiny::observeEvent({
    input$dismiss_password
  }, {

    shiny::removeModal()
    forgot_psw_message(NULL)

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Observe send_tmp_password
  shiny::observeEvent({
    input$send_tmp_password
  }, {

    # Get user name and
    psw_lookup_option <- shiny::isolate({ input$psw_lookup_option })
    
    if(psw_lookup_option == "Username"){

      user_name <- shiny::isolate({ input$psw_username }) %>% base::trimws()

      # Check user table
      user_tbl <- check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
      
      # If user exists, throw an error
      if(nrow(check_user_tbl) == 0){
        forgot_psw_message(base::sprintf("User = '%s' does not exist in our database. Please choose a different name.", user_name))
        return(NULL)
      }

    }else{

      user_email <- shiny::isolate({ input$psw_email }) %>% base::trimws()

      # Check user emails ####
      check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(user_email), ignore.case = TRUE)

      # If any emails do not have correct format, throw an error message
      if(check_email == FALSE){
        forgot_psw_message("Invalid email format.")
        return(NULL)
      }

      # Check user table
      check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
      
      # If user exists, throw an error
      if(!user_email %in% check_email_tbl$user_email){
        forgot_psw_message(base::sprintf("Email = '%s' does not exist in our database. Please choose a different email.", user_email))
        return(NULL)
      }

      # Return user_tbl
      user_tbl <- check_email_tbl %>% dplyr::filter(email %in% user_email)

    }

    # Create a temporary password
    tmp_pwd <- randPassword()
    
    # Update user with new password
    SigRepo::updateUser(conn_handler = conn_handler, user_name = user_name, password = tmp_pwd)

    # Send email to users
    sendPassword(
      user_name = user_tbl$user_name[1],
      user_email = user_tbl$user_email[1],
      temp_password = tmp_pwd
    )

    # Update message
    forgot_psw_message(base::sprintf("A temporary password has been sent to your email at %s.", user_tbl$user_email[1]))

    # Print message
    base::print(base::sprintf("Sending temporary password to user = '%s'", user_tbl$user_name[1]))

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Output forgot_psw_message
  output$forgot_psw_message <- shiny::renderUI({

    req(forgot_psw_message())

    shiny::p(class = "error-message", id = "login-error-message", shiny::isolate({ forgot_psw_message() }))

  })
  
}

## Start the app ####
shiny::shinyApp(ui = ui, server = server)




