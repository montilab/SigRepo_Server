
login <<- function(username="", password="") {
  
  credentials <- list(user_auth = FALSE, username="", password="")
  
  users <- "Username"; pwds <- "Password";
  
  ## Establish database connection
  conn <- newConnHandler(
    driver = RMySQL::MySQL(),
    dbname = Sys.getenv("DBNAME"), 
    host = Sys.getenv("HOST"), 
    port = as.integer(Sys.getenv("PORT")), 
    user = Sys.getenv("USER"), 
    password = Sys.getenv("PASSWORD")
  )
  
  password <- sodium
  
  ##Read in the data
  data <- lookupUser(conn, username, password, api_key = TRUE)
  
  # ensure all text columns are character class
  data <- dplyr::mutate_if(data, is.factor, as.character)
  
  # check for match of input username to username column in data
  row_username <- which(dplyr::pull(data, !!users) == trimws(username))
  
  if (length(row_username) > 0) {
    row_password <- dplyr::filter(data, dplyr::row_number() == row_username[1])
    row_password <- dplyr::pull(row_password, !!pwds)
    # create a sodium hash for password
    password_match <- sodium::password_verify(row_password, password)
  } else {
    password_match <- FALSE
  }
  
  # if user name row and password name row are same, credentials are valid
  if (password_match) {
    shinyjs::hide(id = "error")
    shinyjs::hide(id = "uiSignIn")
    shinyjs::show(id = "uiModeratorPage")
    credentials$user_auth <- TRUE
    credentials$username <- username
    credentials$password <- password
  } else { # if not valid temporarily show error message to user
    shinyjs::show(id = "error")
    shinyjs::show(id = "uiSignIn")
    shinyjs::hide(id = "uiModeratorPage")
    credentials$user_auth <- FALSE
    credentials$username <- ""
    credentials$password <- ""
  }
  
  return(credentials)
  
}

sendpassword <<- function(from_sender="montilab@bu.edu", to_recipient="montilab@bu.edu", recipient_first="Montilab", recipient_last="Montilab", recipient_account="Montilab", tmp_pwd){
  
  recipient=paste(recipient_first, recipient_last)
  
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
      '<p>The password for your Xposome account has changed.</p>',
      '<p></p>',
      '<p>Username: <strong>', recipient_account, '</strong></p>',
      '<p>Temporary password: <strong>', tmp_pwd, '</strong></p>',
      '<br>',
      '<p>Log back in? Follow this link, <strong>https://montilab.bu.edu/Xposome/?page=sign_in</strong></p>',
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

# reactive values to store login credentials
credentials <- reactiveVal(list(user_auth = FALSE, username="", password=""))

##Observe when sign in button is clicked####
observeEvent(input$sign_in_btn, {
  
  login(username=input$username, password=input$password) %>% credentials()
  
})
  	  
## Create a modal dialog for forgot password #####
forgotPasswordDialog <- function() {
  div(
    id = "Forgot_Password", 
    
    modalDialog(
      size = "m", title = NULL, footer = NULL, style="border: 1px solid  #08519c;; background: #08519c;",
      
      fluidRow(
        column(
          width=12, style="background: white; padding-top: 10px;",
          
          h3("Forgot your password?", class="text-center"),
          br(), 
          p(strong("To access your account, please fill in the following information:")),
          br(),
          textInput(inputId="FG_Firstname", label=strong(span(style="color:red;", "*"), "First name"), value="", width="100%"),
          textInput(inputId="FG_Lastname", label=strong(span(style="color:red;", "*"), "Last name"), value="", width="100%"),
          textInput(inputId="FG_Username", label=strong(span(style="color:red;", "*"), "Username"), value="", width="100%"),
          uiOutput("FG_Message"), 
          br(),
          actionButton(class="mybuttons", inputId="save_forget_password", label=strong("Submit")),
          actionButton(class="mybuttons", inputId="dismiss_forget_password", label=strong("Cancel")),
          br(), br()
        )
      )
    )
  )
}
	    
##OBSERVE THE FORGOT BUTTON#####
observeEvent(input$forget_password, {
  
  #Show the modal dialog
  showModal(forgotPasswordDialog())
  
})


##OBSERVE THE BACK BUTTON#####
observeEvent(input$dismiss_forget_password, {
  
  removeModal()
  
})


##OBSERVE THE SUBMIT BUTTON#####
observeEvent(input$FG_Button, {
  
  Firstname=trimws(input$FG_Firstname);
  Lastname=trimws(input$FG_Lastname);
  Username=trimws(input$FG_Username);
  
  login_dat <- read_csv(paste0("www/data/User_Login_List.csv"))
  
  if(Firstname=="" | Lastname=="" | Username==""){
    
    forgotpasswordwarningmsg("Please fill in the required (*) fields.")
    
  }else{
    
    row <- which(login_dat$Username == Username)
    
    if(length(row) > 0){
      
      tmp_pwd <- password(n = 10, numbers = TRUE, case = TRUE, special = c("?", "!", "&", "%", "$"))
      login_dat$Password[row[1]] <- sodium::password_store(as.character(tmp_pwd))
      
      sendpassword(
        from_sender="rchau88@bu.edu",
        to_recipient=login_dat$Email[row[1]], 
        recipient_first=Firstname, 
        recipient_last=Lastname, 
        recipient_account=Username, 
        tmp_pwd=tmp_pwd
      )
      
      write.csv(login_dat, paste0("www/data/User_Login_List.csv"), row.names = FALSE)
      forgotpasswordwarningmsg("Thank you for your submission! A temporary password has been sent to your email.")
      
    }else{
      
      forgotpasswordwarningmsg("This username does not exist in our database. Please enter another username.")
      
    }
    
  }
})






