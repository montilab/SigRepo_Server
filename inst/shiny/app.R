
# R packages for building shiny dashboard
library(htmltools)
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
load_all(".")

#library(OmicSignature)

# Package for creating hash keys
library(sodium)

# Package for parallel processes
library(promises)
library(future)
future::plan(multisession)

## Define ui logic ####
ui <- shiny::bootstrapPage(
  
  title = "SigRepo - Signature Repository",
  
  ### CSS and JS ####
  shiny::tagList(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/main.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/app_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/home_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/sign_in_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/search_signature.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/search_collection.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/upload_signature.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/upload_collection.css"),
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
      
      shiny::div(class = "login-title", shiny::h2("Sign In")),
      
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
          class = "validate-message", 
          shinyjs::hidden(p(class = "error-message", id = "login-error-message", "Invalid username or password!"))
        ),
        
        shiny::div(
          class = "validate-button",
          shiny::actionButton(inputId = "sign_in_btn", class = "sign-in-button", label = "Login", onclick = "login_keypress(e)"),
          shiny::div(class = "forgot_psw", HTML("<a href='#' id='forget_password' class='action-button'>Forgot password?</a>")),
        ),
        
        shiny::div(
          class = "register", 
          shiny::p(HTML("Don't have an acount. <a href='#' id='register' class='action-button'>Register here.</a>"))
        )
      )
    )
  ),
  
  ### Main App ####
  shiny::div(
    class = "content-wrapper", id = "content-wrapper", style = "display: none;",
    
    ### Header #####
    shiny::div(
      class = "banner-wrapper",
      shiny::div(
        class = "container",
        shiny::fluidRow(
          class = "banner-info",
          shiny::column(
            width = 12, 
            class = "login-banner",
            shiny::actionLink(inputId = "edit_profile", label = NULL, icon = shiny::icon("user-circle")),
            shiny::uiOutput(outputId = "welcome_msg"),
            shiny::actionLink(inputId = "log_out_btn", class="button-link", icon = tags$i(class="fa fa-sign-out"), label = strong("Log out"))
          )
        )
      )
    ),
    
    # Include the navbar tabs
    htmltools::htmlTemplate("www/nav.html"),
    
    tags$script(src = "assets/js/jquery.dropotron.min.js", type = "text/javascript"),
    tags$script(src = "assets/js/browser.min.js", type = "text/javascript"),
    tags$script(src = "assets/js/breakpoints.min.js", type= "text/javascript"),
    tags$script(src = "assets/js/util.js", type = "text/javascript"),
    tags$script(src = "assets/js/main.js", type = "text/javascript"),
    
    ### Content #####
    shiny::uiOutput(outputId = "tab_content"),
    
    ### Footer ####
    shiny::div(
      id="footer-wrapper",
      
      tags$footer(
        class="container",
        
        shiny::div(
          class="row footer",
          
          ### Copyright ####
          shiny::div(
            class="col-12 col-12-medium box copyright",
            p(HTML('&copy; Montilab | Boston University | ', format(Sys.Date(), format = "%Y"), ' | All rights reserved | Design by <a href="http://html5up.net">HTML5 UP</a>'))
          )
        )
      )
    )
  )
)

## Define server logic ####
server <- function(input, output, session) {
  
  # Create reactive values to store user login information
  user_conn_handler <- shiny::reactiveVal()
  user_login_info <- shiny::reactiveVal()
  
  # Create reactive values to store user signature and collection ####
  user_signature_tbl <- shiny::reactiveVal()
  user_collection_tbl <- shiny::reactiveVal()
  
  # Print this when a session starts ####
  cat("\nSession started.\n")
  
  # Anything that calls autoInvalidate will automatically invalidate every 2 seconds.
  autoInvalidate <- shiny::reactiveTimer(2000)
  
  # Prevent Shiny from graying out ####
  shiny::observe({
    autoInvalidate()
    cat(".")
  })
  
  # Observe when session ends ####
  session$onSessionEnded(function(x){
    
    cat("\nSession ended.\n")
    
  })
  
  # Observe when session stops ####
  shiny::onStop(function(){
    
    cat("\nSession stopped.\n")
    
  })
  
  # # Get the search parameters
  # shiny::observe({
  #   
  #   req(user_conn_handler())
  #   
  #   # Extract user connection
  #   conn_handler <- shiny::isolate({ user_conn_handler() })
  #   
  #   # Get URL and query string
  #   url  <- shiny::reactiveValuesToList(session$clientData)
  #   query  <- shiny::parseQueryString(session$clientData$url_hash)
  # 
  #   if(length(query$`#user_id`) > 0 && length(query$token) > 0){
  # 
  #     # Get user name and password
  #     user_name <- query$`#user_id`[1];
  #     user_password_token <- query$`token`[1];
  # 
  #     # Validate user
  #     validate_user_tbl <- SigRepo::validateUser(conn_handler = conn_handler)
  # 
  #     if(nrow(validate_user_tbl) > 0){
  # 
  #       user_login_info(validate_user_tbl)
  #       shinyjs::hide(id = "login-wrapper")
  #       shinyjs::show(id = "content-wrapper")
  # 
  #     }else{
  # 
  #       user_login_info(NULL)
  #       shinyjs::hide(id = "content-wrapper")
  #       shinyjs::show(id = "login-wrapper")
  # 
  #     }
  # 
  #   }else{
  # 
  #     user_login_info(NULL)
  #     shinyjs::hide(id = "content-wrapper")
  #     shinyjs::show(id = "login-wrapper")
  # 
  #   }
  # 
  # })

  # Observe when sign in button is clicked #####
  shiny::observeEvent({
    input$sign_in_btn
  }, {

    # Get user name and password
    user_name <- shiny::isolate({ input$username })
    user_password <- shiny::isolate({ input$password })

    # Create a user connection handler
    conn_handler <- base::tryCatch({
      SigRepo::newConnHandler(
        dbname = Sys.getenv("DBNAME"),
        host = Sys.getenv("HOST"),
        port = as.integer(Sys.getenv("PORT")),
        user = user_name,
        password = user_password
      )
    }, error = function(e){
      shinyjs::show(id = "login-error-message")
      print(e, "\n")
      return(NULL)
    }, warning = function(w){
      print(w, "\n")
    })

    # If conn_handler is not valid, escape the function
    if(is.null(conn_handler)) return(NULL)

    # Validate user
    user_tbl <- base::tryCatch({
      SigRepo::validateUser(conn_handler = conn_handler)
    }, error = function(e){
      shinyjs::show(id = "login-error-message")
      print(e, "\n")
      return(base::data.frame(NULL))
    }, warning = function(w){
      print(w, "\n")
    })

    # Check if conn is a MySQLConnection class object
    if(nrow(user_tbl) == 0){

      user_conn_handler(NULL)
      user_login_info(NULL)
      user_signature_tbl(NULL)
      user_collection_tbl(NULL)
      shinyjs::show(id = "login-wrapper")
      shinyjs::show(id = "login-error-message")
      shinyjs::hide(id = "content-wrapper")

    }else{

      # Update URL search string ####
      shiny::updateQueryString(session, queryString = sprintf("#user_id=%s&token=%s", user_name, digest::digest(user_password, algo = "md5", serialize = TRUE)), mode = "push")

      # Get user connection info ####
      user_conn_handler(conn_handler)

      # Get user login info ####
      user_login_info(user_tbl)

      # # Get user signature table #####
      # promises::future_promise({
      #   SigRepo::searchSignature(conn_handler = conn_handler, user_name = user_name)
      # }, package = "tidyverse") %...>% user_signature_tbl()
      # 
      # # Get user collection table ####
      # #promises::future_promise({
      # SigRepo::searchCollection(conn_handler = conn_handler) %>% user_collection_tbl()
      # #}, package = "tidyverse") %...>% user_collection_tbl()

      # Hide message and display app ####
      shinyjs::hide(id = "login-error-message")
      shinyjs::hide(id = "login-wrapper")
      shinyjs::show(id = "content-wrapper")

    }

  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # Welcome message ####
  output$welcome_msg <- shiny::renderUI({

    req(user_login_info())

    # Get user connection info
    shiny::HTML(sprintf("Welcome %s!", user_login_info()$user_name))

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

  # Create reactive values
  tab_selected <- shiny::reactiveVal("home")

  # Observe the selected page by users
  shiny::observeEvent({
    input$selected_tab
  }, {

    shiny::isolate({ input$selected_tab }) %>% tab_selected()

  })

  # Output the content page
  output$tab_content <- shiny::renderUI({

    req(tab_selected())

    if(tab_selected() == "home"){
      htmltools::includeHTML("www/home_content.html")
    }else if(tab_selected() == "search_signature"){
      base::source("ui/search_signature_ui.R")$value
    }else if(tab_selected() == "search_collection"){
      base::source("ui/search_collection_ui.R")$value
    }else if(tab_selected() == "upload_signature"){
      base::source("ui/upload_signature_ui.R")$value
    }else if(tab_selected() == "upload_collection"){
      base::source("ui/upload_collection_ui.R")$value
    }else if(tab_selected() == "compare"){
    }else if(tab_selected() == "analysis"){
    }else if(tab_selected() == "resources"){
    }

  })

  # Import all source files
  source("server/search_signature_server.R", local = TRUE)
  source("server/search_collection_server.R", local = TRUE)
  source("server/upload_signature_server.R", local = TRUE)
  source("server/upload_collection_server.R", local = TRUE)
  source("server/sign_in_server.R", local = TRUE)

}

## Start the app ####
shiny::shinyApp(ui=ui, server=server)




