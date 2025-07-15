
# Create reactive to store messages
register_message <- shiny::reactiveVal()
forgot_psw_message <- shiny::reactiveVal()

# OBSERVE REGISTER BUTTON #####
shiny::observeEvent({
  input$register
}, {
  
  shiny::showModal(  
    shiny::modalDialog(
      size = "l", title = "Register Form",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::textInput(inputId = "register_username", label = shiny::strong("Username"), placeholder = "Enter Username", width = "100%"),
          shiny::textInput(inputId = 'register_password', label = shiny::strong("Password"), placeholder = "Enter Password", width = "100%"),
          shiny::textInput(inputId = "register_email", label = shiny::strong("Email"), placeholder = "Enter Email", width = "100%"),
          shiny::textInput(inputId = "register_first_name", label = shiny::strong("First Name"), placeholder = "Enter First Name", width = "100%"),
          shiny::textInput(inputId = 'register_last_name', label = shiny::strong("Last Name"), placeholder = "Enter Last Name", width = "100%"),
          shiny::textInput(inputId = 'register_affiliation', label = shiny::strong("Affiliation"), placeholder = "Enter Affiliation", width = "100%"),
          shiny::uiOutput(outputId = "register_message")
        )
      ),
      footer = shiny::tagList(
        shiny::actionButton(inputId = "register_user", class = "primary-btn", label = shiny::strong("Register")),
        shiny::actionButton(inputId = "dismiss_register", class = "primary-btn", label = shiny::strong("Cancel"))
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

# Observe register_user ####
shiny::observeEvent({
  input$register_user
}, {

  # Get user name and password
  user_name <- shiny::isolate({ input$register_username }) %>% base::trimws()
  user_password <- shiny::isolate({ input$register_password }) %>% base::trimws()
  user_email <- shiny::isolate({ input$register_email }) %>% base::trimws()
  user_first <- shiny::isolate({ input$register_first_name }) %>% base::trimws()
  user_last <- shiny::isolate({ input$register_last_name }) %>% base::trimws()
  user_affiliation <- shiny::isolate({ input$register_affiliation }) %>% base::trimws()
  
  # Check user name
  if(user_name %in% c(NA, "")){
    
    register_message("'Username' cannot be empty")
    return(NULL)
    
  }else{
    
    # Check user table
    check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
    
    # If user exists, throw an error
    if(nrow(check_user_tbl) > 0){
      register_message(base::sprintf("User = '%s' already existed in our database. Please choose a different name.", user_name))
      return(NULL)
    }
    
  }
  
  # Check user name
  if(user_password %in% c(NA, "")){
    register_message("'Password' cannot be empty")
    return(NULL)
  }
  
  # Check user name
  if(user_email %in% c(NA, "")){
    
    register_message("'Email' cannot be empty")
    return(NULL)
    
  }else{
    
    # Check user emails ####
    check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(user_email), ignore.case = TRUE)
    
    # If any emails do not have correct format, throw an error message
    if(check_email == FALSE){
      register_message("Invalid email format.")
      return(NULL)
    }
    
    # Check user table
    check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
    
    # If user exists, throw an error
    if(user_email %in% check_email_tbl$user_email){
      register_message(base::sprintf("Email = '%s' already existed in our database. Please choose a different email.", user_email))
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
  api_url <- base::sprintf("https://montilab.bu.edu/SigRepo/register_user/?user_name=%s&api_key=%s", user_name, base::Sys.getenv("API_KEY"))
  
  # Send email to users through montilab server API
  res <- httr::GET(url = api_url)
  
  # Check status code
  if(res$status_code != 200){
    register_message(base::sprintf("\tSomething went wrong with the API. Cannot register user. Please contact admin for support.\n", user_name))
    return(NULL)
  }else{
    register_message(base::sprintf("Thank you for signing up! Our administrator will contact you on how to access our database.", user_email))
  }

  # Print message
  base::print(base::sprintf("Adding user = '%s' to database", user_name))
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Output register_message
output$register_message <- shiny::renderUI({
  
  req(register_message())
  
  shiny::p(class = "error-message", shiny::isolate({ register_message() }))
  
})

# OBSERVE THE FORGOT BUTTON #####
shiny::observeEvent({
  input$forget_password
}, {
  
  shiny::showModal(  
    shiny::modalDialog(
      size = "l", title = NULL,
      
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h3("Forgot your password?", class="text-center"),
          shiny::br(),
          shiny::p(shiny::strong("To access your account, please fill in the following information:")),
          shiny::br(),
          shiny::radioButtons(inputId = "psw_lookup_option", label = NULL, choices = c("Username", "Email"), inline = TRUE),
          shiny::conditionalPanel(
            condition = 'input.psw_lookup_option == "Username"',
            shiny::textInput(inputId = "psw_username", label = shiny::strong("Enter your username:"), value = "", width = "100%")
          ),
          shiny::conditionalPanel(
            condition = 'input.psw_lookup_option == "Email"',
            shiny::textInput(inputId = "psw_email", label = shiny::strong("Enter your email:"), value = "", width = "100%")
          ),
          shiny::uiOutput("forgot_psw_message")
        )
      ),
      
      footer = shiny::tagList(
        shiny::actionButton(inputId = "send_tmp_password", class = "primary-btn", label = shiny::strong("Submit")),
        shiny::actionButton(inputId = "dismiss_password", class = "primary-btn", label = shiny::strong("Cancel"))
      )
    )
  )
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)


##OBSERVE THE BACK BUTTON#####
shiny::observeEvent({
  input$dismiss_password
}, {
  
  shiny::removeModal()
  forgot_psw_message(NULL)
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)


##OBSERVE THE SUBMIT BUTTON#####
shiny::observeEvent({
  input$send_tmp_password
}, {
  
  # Get user name and
  psw_lookup_option <- shiny::isolate({ input$psw_lookup_option })
  
  if(psw_lookup_option == "Username"){
    
    user_name <- shiny::isolate({ input$psw_username }) %>% base::trimws()
    
    print(user_name)
    
    # Check user table
    user_tbl <- check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
    
    print(user_tbl)
    
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
  
  # Send email to users to notify their account are activated
  api_url <- base::sprintf("https://montilab.bu.edu/SigRepo/send_tmp_password/?user_name=%s&api_key=%s", user_name, base::Sys.getenv("API_KEY"))
  
  # Send email to users through montilab server API
  res <- httr::GET(url = api_url)
  
  # Check status code
  if(res$status_code != 200){
    forgot_psw_message(base::sprintf("\tSomething went wrong with the API. Cannot send temporary password to user. Please contact admin for support.\n"))
    return(NULL)
  }else{
    forgot_psw_message(base::sprintf("A temporary password has been sent to your email at %s.", user_tbl$user_email[1]))
  }
  
  # Print message
  base::print(base::sprintf("Sending temporary password to user = '%s'", user_tbl$user_name[1]))
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Output forgot_psw_message
output$forgot_psw_message <- shiny::renderUI({
  
  req(forgot_psw_message())
  
  shiny::p(class = "error-message", shiny::isolate({ forgot_psw_message() }))
  
})

## OBSERVE THE SUBMIT BUTTON #####
shiny::observeEvent({
  input$edit_profile
}, {

  req(user_login_info())

  # Get user table
  user_tbl <- shiny::isolate({ user_login_info() })

  # Show the modal dialog
  shiny::showModal(
    shiny::modalDialog(
      size = "l", title = "USER PROFILE", footer = NULL, style = "border: 1px solid  #08519c;; background: #08519c;",

      shiny::fluidRow(
        shiny::column(
          width = 12, style = "background: white; padding: 10px;",
          shiny::p("Username: ", user_tbl$user_name),
          shiny::p("Email: ", user_tbl$user_email),
          shiny::p("Affliliation: ", user_tbl$user_affiliation),
          shiny::p("Role: ", user_tbl$user_role),
          shiny::p("API Key: ", user_tbl$api_key),
          br(),
          shiny::uiOutput(outputId = "change_psw_message"),
          shiny::actionButton(inputId = "change_email", label = shiny::strong("Change Email")),
          shiny::actionButton(inputId = "change_password", label = shiny::strong("Change Password")),
          shiny::actionButton(inputId = "dismiss_change_profile", label = shiny::strong("Cancel"))
        )
      )
    )
  )

}, ignoreNULL = TRUE, ignoreInit = TRUE)


## OBSERVE THE BACK BUTTON #####
shiny::observeEvent({
  input$dismiss_change_profile
}, {

  shiny::removeModal()

}, ignoreNULL = TRUE, ignoreInit = TRUE)

  
  