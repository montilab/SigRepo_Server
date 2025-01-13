
sendpassword <<- function(from_sender="montilab@bu.edu", to_recipient="montilab@bu.edu", recipient_first="Montilab", recipient_last="Montilab", recipient_account="Montilab", tmp_pwd){
  
  recipient <- paste0(recipient_first, recipient_last)
  
  msg <- mime_part(
    paste0(
      '<!DOCTYPE>',
      '<html>',
      '<head>',
      '<meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0"/>',
      '<title>HTML MESSAGE</title>',
      '<style type="text/css">',
      '</style>',
      '</head>',
      '<body>',
      '<p>Hi <strong>', recipient_first, ',</strong></p>',
      '<p>The password for your SigRepo account has changed.</p>',
      '<p></p>',
      '<p>Username: <strong>', recipient_account, '</strong></p>',
      '<p>Temporary password: <strong>', tmp_pwd, '</strong></p>',
      '<br>',
      '<p>Log back in? Follow this link, <strong>https://montilab.bu.edu/SigRepo/?page=sign_in</strong></p>',
      '<br>',
      '<p>Best,</p>',
      '<p>Montilab Team</p>',
      '</body>',
      '</html>' 
    )
  )
  
  ## Override content type.
  msg[["headers"]][["Content-Type"]] <- "text/html"
  
  from <- paste0("\"Montilab Team\"<", from_sender, ">")
  to <- paste0("\"", recipient, "\"<", to_recipient, ">", collapse="")
  subject <- "Temporary password for Xposome"
  body <- list(msg)
  sendmail(from, to, subject, body, control=list(smtpServer="smtp.bu.edu", smtpPort="25"))
  
}
  	  
## Create a modal dialog for forgot password #####
forgotPasswordDialog <- function() {
  div(
    id = "Forgot_Password", 
    
    shiny::modalDialog(
      size = "l", title = NULL, footer = NULL, style="border: 1px solid  #08519c;; background: #08519c;",
      
      fluidRow(
        column(
          width=12, style="background: white; padding-top: 10px;",
          
          h3("Forgot your password?", class="text-center"),
          br(), 
          p(strong("To access your account, please fill in the following information:")),
          br(),
          shiny::radioButtons(inputId = "psw_lookup_options", label = NULL, choices = c("Username", "Email"), inline = TRUE),
          shiny::textInput(inputId = "psw_lookup_value", label = NULL, value = "", width = "100%"),
          shiny::uiOutput("forgot_psw_message"), 
          br(),
          shiny::actionButton(class="mybuttons", inputId="send_tmp_password", label=strong("Submit")),
          shiny::actionButton(class="mybuttons", inputId="dismiss_password", label=strong("Cancel")),
          br(), br()
        )
      )
    )
  )
}

## Create a modal dialog for forgot password #####
signInDialog <- function() {
  div(
    id = "Forgot_Password", 
    
    shiny::modalDialog(
      size = "l", title = NULL, footer = NULL, style="border: 1px solid  #08519c;; background: #08519c;",
      
      fluidRow(
        column(
          width=12, style="background: white; padding-top: 10px;",
          
          h3("Forgot your password?", class="text-center"),
          br(), 
          p(strong("To access your account, please fill in the following information:")),
          br(),
          shiny::radioButtons(inputId = "psw_lookup_options", label = NULL, choices = c("Username", "Email"), inline = TRUE),
          shiny::textInput(inputId = "psw_lookup_value", label = NULL, value = "", width = "100%"),
          shiny::uiOutput("forgot_psw_message"), 
          br(),
          shiny::actionButton(class="mybuttons", inputId="send_tmp_password", label=strong("Submit")),
          shiny::actionButton(class="mybuttons", inputId="dismiss_password", label=strong("Cancel")),
          br(), br()
        )
      )
    )
  )
}

# Create reative values to store messages
forgotpasswordwarningmsg <- reactiveVal()

##OBSERVE THE FORGOT BUTTON#####
observeEvent(input$forget_password, {
  
  #Show the modal dialog
  shiny::showModal(forgotPasswordDialog())
  
})

##OBSERVE THE BACK BUTTON#####
observeEvent(input$dismiss_password, {
  
  shiny::removeModal()
  
})

##OBSERVE THE SUBMIT BUTTON#####
shiny::observeEvent({
  input$send_tmp_password
}, {
  
  lookup_options <- shiny::isolate({ input$psw_lookup_options })
  lookup_value <- shiny::isolate({ input$psw_lookup_value })
  
  print(lookup_options); print(lookup_value);
  
  if(lookup_options == "" | lookup_value == "" ){
    
    forgotpasswordwarningmsg("Please fill in the required field.")
    
  }else{
    
    filter_coln_var <- ifelse(lookup_options == "Username", "user_name", "user_email")
    filter_coln_val <- lookup_value
    names(filter_coln_val) <- filter_coln_var
    
    ## Establish database connection
    conn <- SigRepo::newConnHandler(
      driver = RMySQL::MySQL(),
      dbname = Sys.getenv("DBNAME"), 
      host = Sys.getenv("HOST"), 
      port = as.integer(Sys.getenv("PORT")), 
      user = Sys.getenv("USER"), 
      password = Sys.getenv("PASSWORD")
    )
    
    # Look up user 
    user_tbl <- SigRepo::lookup_table_sql(
      conn = conn, 
      db_table_name = "users", 
      return_var = "*",
      filter_coln_var = filter_coln_var, 
      filter_coln_val = filter_coln_val,
      check_db_table = TRUE
    )
    
    print(user_tbl)
    
    if(nrow(user_tbl) > 0){
      
      tmp_pwd <- stringi::stri_rand_strings(n=1, length=10, pattern="[A-Za-z0-9]")
      user_tbl$user_password_hashkey[1] <- sodium::password_store(as.character(tmp_pwd))
      
      # sendpassword(
      #   from_sender="rchau88@bu.edu",
      #   to_recipient = login_dat$Email[row[1]], 
      #   recipient_first = Firstname, 
      #   recipient_last = Lastname, 
      #   recipient_account = Username, 
      #   tmp_pwd = tmp_pwd
      # )
      
      # Need an update function to update a user's password
      
      # Update the message
      forgotpasswordwarningmsg(sprintf("A temporary password has been sent to your email at %s.", user_tbl$user_email[1]))
      
    }else{
      
      forgotpasswordwarningmsg(sprintf("%s = '%s' does not exist in our database.", filter_coln_var, filter_coln_val))
      
    }
    
  }
  
})

# output message
output$forgot_psw_message <- renderUI({
  
  req(forgotpasswordwarningmsg)
  
  p(class = "error-message", forgotpasswordwarningmsg())
  
})


# edit user profile
## Create a modal dialog for forgot password #####
userProfileDialog <- function(user_tbl){
  div(
    id = "user_profile", 
    
    shiny::modalDialog(
      size = "l", title = "USER PROFILE", footer = NULL, style="border: 1px solid  #08519c;; background: #08519c;",
      
      fluidRow(
        column(
          width=12, style="background: white; padding: 10px;",
          shiny::p("Username: ", user_tbl$user_name),
          shiny::p("Email: ", user_tbl$user_email),
          shiny::p("Affliliation: ", user_tbl$user_affiliation),
          shiny::p("Role: ", user_tbl$user_role),
          shiny::p("API Key: ", user_tbl$api_key),
          br(),
          shiny::uiOutput("change_psw_message"), 
          shiny::actionButton(inputId="change_email", label=strong("Change Email")),
          shiny::actionButton(inputId="change_password", label=strong("Change Password")),
          shiny::actionButton(inputId="dismiss_change_profile", label=strong("Cancel"))
        )
      )
    )
  )
}

## OBSERVE THE SUBMIT BUTTON #####
shiny::observeEvent({
  input$edit_profile
}, {

  req(user_login_info())
  
  #Show the modal dialog
  shiny::showModal(userProfileDialog(user_tbl = user_login_info()))
  
}) 
  
  
## OBSERVE THE BACK BUTTON #####
observeEvent(input$dismiss_change_profile, {
  
  shiny::removeModal()
  
})  
  
  