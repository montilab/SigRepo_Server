
# R packages for building shiny dashboard
library(shinyjs)
library(shiny)
library(DT)
library(knitr)

# Package for data cleaning, extraction and manipulation
library(tidyverse)

# Package for loading and installing packages
library(devtools)

devtools::load_all()




# Loading omic signature package
library(OmicSignature)



# Package for parallel processes
library(promises)
library(future)
future::plan(multisession)





# Create a default database handler 
# FOR ROOT ONLY, DONT USE IN ACTUAL R SHINY
conn_handler <- SigRepo::newConnHandler(
  dbname = Sys.getenv("DBNAME"),
  host = Sys.getenv("HOST"),
  port = as.integer(Sys.getenv("PORT")),
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("PASSWORD")
)

## Define ui logic ####
ui <- fluidPage(
  
  title = "SigRepo - Signature Repository",
  
  ### CSS and JS ####
  shiny::tagList(
    tags$head(
      #tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/main.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/app_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/home_style.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/sign_in_style.css"),
      #tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/search_signature.css"),
      #tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/search_collection.css"),
      #tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/upload_signature.css"),
      #tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/upload_collection.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/fontawesome-all.min.css"),
      tags$script(src = "assets/js/app.js", type = "text/javascript"),
      tags$style(HTML("
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
    
    body{
    padding-top: 70px;
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
    
  "))
    )
  ),
  
  
  
  # JS TAGS ####
  tags$script(HTML("
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
")),
  
  
  
  
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
          shiny::HTML("<span class='login-label'><b>Username</b></span>"),
          shiny::div(
            class = "username-container",
            shiny::HTML("<input class='login-input' type='text' id='username' onkeypress='login_keypress(e)' placeholder='Enter Username'>")
          )
        ),
        
        shiny::div(
          class = "validate-input",
          shiny::HTML("<span class='login-label'><b>Password</b></span>"),
          shiny::div(
            class = "password-container",
            shiny::HTML("<input class='login-input' type='password' id='password' onkeypress='login_keypress(e)' placeholder='Enter Password'>"),
            shiny::HTML("<span class='toggle-password' onclick='toggle_password()'>👁️</span>")
          )
        ),
        
        shiny::div(
          class = "validate-message", 
          shiny::uiOutput(outputId = "login_error_message")
        ),
        
        shiny::div(
          class = "validate-button",
          shiny::actionButton(inputId = "sign_in_btn", class = "sign-in-button", label = "Login", onclick = "login_keypress(e)"),
          shiny::div(class = "forgot_psw", shiny::HTML("<a href='#' id='forget_password' class='action-button'>Forgot password?</a>")),
        ),
        
        shiny::div(
          class = "register", 
          shiny::span("Don't have an acount.", shiny::HTML("<a href='#' id='register' class='action-button'>Register here!</a>"))
        )
      )
    )
  ),
  
  ### Main App ####
  shiny::div(
    class = "content-wrapper", id = "content-wrapper", style = "display: none;",
    
    
    
    navbarPage(
      title = div(
        
        style = "width: 100%; display: flex; justify-content: space-between; align-items: center;",
        
        div(
          tags$img(src ="images/logo.png", height = "50px", style = "margin-right:10px;")
          # span("SigRepo", style = "font-weight: bold; font-size: 24px; vertical-align: middle;")
        ),
        
        # right side for log out and banner
        
        div(
          style = "display: flex; align-items: center; gap: 10px;",
          shiny::actionLink(inputId = "edit_profile", label = NULL, icon = shiny::icon("user-circle")),
          shiny::uiOutput(outputId = "welcoome_msg"),
          shiny::actionLink(inputId = "log_out_btn", class = "button-link", icon = tags$i(class = " fa fa-sign-out"), label = strong("Log out"))
        )
      ), 
      id = "main_navbar",
      position = "fixed-top",
      
      
      tabPanel("Home", value = "home",
               tags$head(
                 tags$style(HTML("
        .homepage-title {
          text-align: center;
          font-size: 36px;
          font-weight: bold;
          margin-top: 30px;
        }
        .homepage-subtitle {
          text-align: center;
          font-size: 20px;
          color: #666;
          margin-bottom: 30px;
        }
        .homepage-section {
          background-color: #f8f9fa;
          padding: 20px;
          border-radius: 10px;
          margin-bottom: 20px;
        }
        .homepage-icon {
          font-size: 40px;
          color: #007bff;
          margin-bottom: 10px;
        }
      "))
               ),
               
               #div(class = "homepage-title", "Welcome to SigRepo"),
               #div(class = "homepage-subtitle", "A platform for managing and exploring genomic signatures and collections"),
               
               
               fluidRow(
                 column(
                   width = 4,
                   div(class = "homepage-section text-intro",
                       
                       
                       HTML("
    <div style='padding: 10px; line-height: 1.6; font-size: 16px; height: 450px;'>
      <p>
        <strong>Welcome to the Signature Repository (SigRepo)!</strong>
      </p>
      <p>
        The Signature Repository is a collaborative platform designed for storing and managing biological signatures and their associated data.
      </p>
      <p>
        This R Shiny application provides a user-friendly interface to:
      </p>
      <ul>
        <li>Browse, upload, and search for signatures</li>
        <li>Manage access and permissions</li>
        <li>Perform gene set enrichment and annotation</li>
      </ul>
      <p>
        You can explore both public signatures and the ones you've contributed.
      </p>
    </div>
  ")
                       
                       
                       
                       
                       
                   )
                 ), 
                 column(width = 8,
                        div(class = "homepage-section",
                            h4("Signature Overview"),
                            tabsetPanel(
                              tabPanel("By Organism", plotOutput("organism_plot", height = "450px")),
                              tabPanel("By Assay", plotOutput("assay_plot", height = "450px")),
                              tabPanel("Top Users", plotOutput("top_users_plot", height = "450px"))
                            )
                        )
                 )
                 
               ),
               fluidRow(
                 
                 column(
                   width = 4,
                   div(class = "homepage-section text-center",
                       span(class = "homepage-icon", icon("dna")),
                       h4("Signatures"),
                       p("Browse, filter, and manage gene expression signatures."),
                       actionButton("go_signatures", "Go to Signatures", class = "btn-primary")
                   )
                 ),
                 column(
                   width = 4,
                   div(class = "homepage-section text-center",
                       span(class = "homepage-icon", icon("layer-group")),
                       h4("Collections"),
                       p("Explore curated collections of signatures."),
                       actionButton("go_collections", "Go to Collections", class = "btn-primary")
                   )
                 ),
                 column(
                   width = 4,
                   div(class = "homepage-section text-center",
                       span(class = "homepage-icon", icon("upload")),
                       h4("R-Client"),
                       p("View our R-Client Documentation to use SigRepo in R."),
                       actionButton("go_upload", "Github Docs", class = "btn-success")
                   )
                 )
               ),
               
               br(),
               fluidRow(
                 column(12,
                        div(class = "text-center text-muted",
                            p("Created by SigRepo Team  · Version 1.0 · © 2025")
                        )
                 )
               )
      ),
      
      
      tabPanel("Signatures", value = "signatures",
               
               fluidPage(
                 
                 actionButton("open_upload_modal", "Upload Signature", icon = icon("upload"), class = "btn-primary"),
                 
                 
                 # Use fluidRow to create left and right layout
                 fluidRow(
                   column(
                     width = 8 # Wider column for the tabset
                     # tabsetPanel(
                     #   id = "main_tabs",
                     #   type = "tabs",
                     #   
                     #   tabPanel("Upload",
                     #            h4("Upload Signature"),
                     #            fileInput("upload_file_signature", "Choose a file to upload"),
                     #            actionButton("upload_btn_signature", "Upload"),
                     #            uiOutput("upload_sig_error_msg"),
                     #            verbatimTextOutput("upload_output")
                     #   ),
                     #   
                     #   tabPanel("Update",
                     #            h4("Update a Signature"),
                     #            selectInput("update_sig", "Select Signature to Update", choices = "", multiple = FALSE),
                     #            fileInput("update_sig_file", "Choose a file to update"),
                     #            actionButton("update_btn", "Update"),
                     #            uiOutput("update_sig_error_msg")
                     #   ),
                     #   
                     #   tabPanel("Delete",
                     #            h4("Delete Signature"),
                     #            selectInput("delete_sig", "Select Signature to Delete", choices = NULL, multiple = FALSE),
                     #            actionButton("delete_btn_sig", "Delete")
                     #   )
                     # )
                   )
                   
                  
                 ),
                 
                 br(),
                 
                 # Signature table
                 uiOutput("action_buttons"),
                 DTOutput("signature_tbl")
                 
               ),
               
               # Absolute Panel (unchanged)
               absolutePanel(
                 id = "details_panel",
                 top = 100, right = 100, width = 800, draggable = TRUE,
                 style = "z-index: 10; background-color: #fff; padding: 20px; border: 1px solid #ccc; border-radius: 8px; box-shadow: 0px 4px 10px rgba(0,0,0,0.1); display: none;",

                 # Close button
                 tags$div(
                   style = "position: absolute; top: 10px; right: 10px; cursor: pointer; font-size: 20px;",
                   actionLink("close_panel", label = HTML("&times;"))
                 ),

                 conditionalPanel(
                   condition = "output.view === 'true'",
                   tabsetPanel(
                     tabPanel("Summary",
                              div(id = "oms_download_wrapper",
                                  downloadButton("download_oms_handler", "Download OmicSignature", class = "submit-button", onclick = "sig_tbl_select_rows();")
                              ),
                              uiOutput("signature_title"),
                              uiOutput("signature_description"),
                              br(), br(),
                              fluidRow(
                                column(
                                  width = 4,
                                  div(class = "signature-info",
                                      h4("🧬 Signature Info"),
                                      tags$hr(),
                                      uiOutput("signature_metadata")
                                  )
                                ),
                                column(
                                  width = 8,
                                  tabsetPanel(
                                    tabPanel("Top Features", plotOutput("top_features"))
                                  )
                                )
                              )
                     ),

                     tabPanel("Signature",
                              DTOutput("signature_file_table")
                     ),

                     tabPanel("Difexp",
                              DTOutput("difexp_file_table")
                     )
                   )
                 )
               )
      ),
      
      
      
      
      
      tabPanel("Collections", value = "collections",
               
               #div(class = "homepage-title", "Collection Management"),
               #div(class = "homepage-subtitle", "Add Signature Collections or browse the curated list of Signature Collections"),
               
               
               sidebarLayout(
                 sidebarPanel( width = 4,
                               tabsetPanel(
                                 id = "main_tabs",
                                 type = "tabs",
                                 
                                 #upload tab for collections
                                 
                                 tabPanel("Upload",
                                          h4("Upload Collection"),
                                          fileInput("upload_file_collection", "Choose a file to upload"),
                                          actionButton("upload_btn_collection","Upload"),
                                          uiOutput("upload_sig_error_msg")
                                 ),
                                 tabPanel("Update",
                                          h4("Update a Collection"),
                                          selectInput("update_collection","Select a Collection to Update", choices = "", multiple = FALSE),
                                          fileInput("update_collection_file", "Choose a collection file"),
                                          actionButton("update_btn_collection", "Update"),
                                          uiOutput("update_collection_error_msg")
                                 ),
                                 tabPanel("Delete",
                                          h4("Delete Collection"),
                                          selectInput("delete_collection", "Select a Collection to delete", choices = NULL, multiple = FALSE),
                                          actionButton("delete_btn_collection", "Delete")
                                 )
                               )
                 ), mainPanel(width = 8,
                              DTOutput("collection_tbl"),
                              uiOutput("collection_tbl_error_msg"),
                              br(),
                              downloadButton("download_collection","Download Collection", class = "submit-button",
                                             onclick = "collection_tbl_select_rows();")
                 )
               )
               
      ),
      
      tabPanel("Annotate", value = "annotate",
               
               #div(class = "homepage-title", "Annotate"),
               #div(class = "homepage-subtitle", "Conduct hypeR on signatures"),
               
               
               sidebarLayout(
                 sidebarPanel(
                   width = 4,
                   tabsetPanel(id = "sidebar_tabs",
                               tabPanel("[1] Signature",
                                        textInput("experiment_label", tags$b("Experiment Label"), placeholder = "E.g. Knockout Experiment"),
                                        textInput("signature_label", tags$b("Signature Label"), placeholder = "E.g. Downregulated Genes"),
                                        selectInput("signature_hypeR", "Select a signature", choices = "", multiple = FALSE),
                                        actionButton("signature_add", "Add Signature")
                               ),
                               tabPanel("[2] Genesets",
                                        fluidRow(
                                          column(4,
                                                 # hypeR::genesets_UI("genesets")
                                          ),
                                          column(8,
                                                 # uiOutput("geneset_table"),
                                          )
                                        )
                                        
                                        
                               ),
                               tabPanel("[3] Enrichment",
                                        
                                        numericInput("enrichment_thresh", "Threshold", 0.05),
                                        numericInput("enrichment_bg", "Background", 36000),
                                        actionButton("enrichment_do", "Do Enrichment")
                               )
                   )
                 ),
                 
                 mainPanel(
                   width = 8,
                   conditionalPanel(
                     condition = "input.sidebar_tabs === '[1] Signature'",
                     htmlOutput("data_preview")
                   ),
                   conditionalPanel(
                     condition = "input.sidebar_tabs === '[2] Genesets'",
                     uiOutput("geneset_table")
                   ),
                   conditionalPanel(
                     condition = "input.sidebar_tabs === '[3] Enrichment'",
                     uiOutput("enrichment")
                   )
                 )
               )
      ),
      
      tabPanel("Compare", value = "compare",
               div(class = "container", h3("Compare tab"))
      ),
      tabPanel("References", value = "references",
               
               # div(class = "homepage-title", "References"),
               #div(class = "homepage-subtitle", "Browse our reference feature dictionaries."),
               
               
               
               
               sidebarLayout(
                 sidebarPanel(
                   width = 4,
                   id = "main_tabs",
                   type = "tabs",
                   
                   tabPanel("References",
                            
                            selectInput("ref_organism", "select an organism", choices = c("Homo sapiens", "Mus musculus")),
                            selectInput("ref_assay", "select an assay type", choice = c("transcriptomics", "proteomics")),
                            actionButton("search_ref_btn", "Search")
                   )
                 ), 
                 mainPanel(width = 8,
                           DTOutput("ref_feature_tbl"))
               )),
      
      tabPanel("Resources", value = "resources",
               tags$head(
                 tags$style(HTML("
      .card-header {
        background-color: #f8f9fa;
        cursor: pointer;
      }
      .card {
        margin-bottom: 10px;
      }
    "))
               ),
               
               tabPanel("Resources", value = "resources",
                        tags$head(
                          tags$style(HTML("
             .resource-menu {
               background-color: #f8f9fa;
               padding: 20px 20px 20px 0px;
               border-right: 1px solid #ddd;
               margin-left: 0px; 
               height: 100%;
             }
             .resource-content {
               padding: 20px;
             }
             .resource-link {
               display: block;
               margin-bottom: 10px;
               font-size: 16px;
               color: #007bff;
               cursor: pointer;
             }
             .resource-link:hover {
               text-decoration: underline;
             }
           "))
                        ),
                        
                        div(class = "container",
                            h3("Resources"),
                            
                            fluidRow(
                              column(
                                width = 3,
                                div(class = "resource-menu",
                                    h4("Topics"),
                                    actionLink("res_getting_started", "📘 Getting Started", class = "resource-link"),
                                    actionLink("res_file_formats", "📂 File Formats", class = "resource-link"),
                                    actionLink("res_permissions", "🔒 Access & Permissions", class = "resource-link"),
                                    actionLink("res_rclient", "📦 R-Client Tutorial", class = "resource-link")
                                )
                              ),
                              
                              column(
                                width = 9,
                                div(class = "resource-content",
                                    uiOutput("resource_content")
                                )
                              )
                            )
                        )
               )
               
      )
      
      
      
      
      
    )
    
  )
)

# ### Tab Content #####
# shiny::uiOutput(outputId = "tab_content"),

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
        shiny::p(shiny::HTML('&copy; Montilab | Boston University | ', base::format(base::Sys.Date(), format = "%Y"), ' | All rights reserved | Design by <a href="http://html5up.net">HTML5 UP</a>'))
      )
    )
  )
)



## Define server logic ####
server <- function(input, output, session) {
  
  # Create reactive to store error messages
  login_error_message <- shiny::reactiveVal()  
  
  # Create reactive values to store user login information
  user_conn_handler <- shiny::reactiveVal()
  user_login_info <- shiny::reactiveVal()
  
  shinyjs::hide("oms_download_wrapper")
  
  
  
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
  session$onSessionEnded(function(x){
    
    base::cat("\nSession ended.\n")
    
  })
  
  # Observe when session stops ####
  shiny::onStop(function(){
    
    base::cat("\nSession stopped.\n")
    
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
    user_name <- shiny::isolate({ input$username }) %>% base::trimws()
    user_password <- shiny::isolate({ input$password }) %>% base::trimws()
    
    # Check user name
    if(user_name %in% c(NA, "")){
      
      login_error_message("'Username' cannot be empty")
      return(NULL)
      
    }else{
      
      # Check user table
      check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
      
      # If user exists, throw an error
      if(nrow(check_user_tbl) == 0){
        login_error_message(base::sprintf("Invalid username or password!"))
        return(NULL)        
      }else if(nrow(check_user_tbl) > 0 && check_user_tbl$active[1] == 0){
        login_error_message(base::sprintf("User = '%s' is currently inactive in our database. Please contact our admin to activate it.", user_name))
        return(NULL)
      }
      
    }
    
    # Check user_password
    if(user_password %in% c(NA, "")){
      login_error_message("'Password' cannot be empty")
      return(NULL)
    }
    
    # Create a user connection handler
    user_conn_handler <- SigRepo::newConnHandler(
      dbname = Sys.getenv("DBNAME"),
      host = Sys.getenv("HOST"),
      port = as.integer(Sys.getenv("PORT")),
      user = user_name,
      password = user_password
    )
    
    # Validate user
    user_tbl <- base::tryCatch({
      SigRepo::validateUser(conn_handler = user_conn_handler)
    }, error = function(e){
      base::print(e, "\n")
      return(base::data.frame(NULL))
    })
    
    # Check if conn is a MySQLConnection class object
    if(nrow(user_tbl) == 0){
      
      # Update message
      login_error_message(base::sprintf("Invalid username or password!"))
      user_conn_handler(NULL)
      user_login_info(NULL)
      user_signature_tbl(NULL)
      user_collection_tbl(NULL)
      shinyjs::show(id = "login-wrapper")
      shinyjs::hide(id = "content-wrapper")
      
    }else{
      
      # Update message ####
      login_error_message(NULL)     
      
      # Update URL search string ####
      shiny::updateQueryString(session, queryString = sprintf("#user_id=%s&token=%s", user_name, digest::digest(user_password, algo = "md5", serialize = TRUE)), mode = "push")
      
      # Get user connection info ####
      user_conn_handler(user_conn_handler)
      
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
        size = "l", title = shiny::span(shiny::icon(name = "user-plus", lib = "font-awesome"), "REGISTER USER"),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::textInput(inputId = "register_username", label = shiny::strong(shiny::span(style = "color: red;", "*"), "Username"), value = "", placeholder = "Enter Username", width = "100%")
          ),
          shiny::column(
            width = 12, id = "register-password", 
            shiny::strong(shiny::span(style = "color: red;", "*"), "Password"),
            #shiny::HTML("<b><span style='color: red;'>*<span> Password</b>"),
            shiny::div(
              class = "register-password",
              shiny::HTML("<input type='password' id='register_password' placeholder='Enter Password'>"),
              shiny::HTML("<span class='toggle-register-password' onclick='toggle_register_password()'>👁️</span>")
            )
          ),
          shiny::column(
            width = 12,
            shiny::textInput(inputId = "register_email", label = shiny::strong(shiny::span(style = "color: red;", "*"), "Email"), value = "", placeholder = "Enter Email", width = "100%"),
            shiny::textInput(inputId = "register_first_name", label = shiny::strong("First Name"), value = "", placeholder = "Enter First Name", width = "100%"),
            shiny::textInput(inputId = 'register_last_name', label = shiny::strong("Last Name"), value = "", placeholder = "Enter Last Name", width = "100%"),
            shiny::textInput(inputId = 'register_affiliation', label = shiny::strong("Affiliation"), value = "", placeholder = "Enter Affiliation", width = "100%"),
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
  
  # OBSERVE REGISTER USER BUTTON #####
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
      if(nrow(check_user_tbl) > 0 && check_user_tbl$active[1] == 0){
        register_message(base::sprintf("User = '%s' is already existed in our database and currently inactive. If this is your account, please contact our admin to activate it.", user_name))
        return(NULL)
      }else if(nrow(check_user_tbl) > 0 && check_user_tbl$active[1] == 1){
        register_message(base::sprintf("User = '%s' is already existed in our database. Please choose a different name.", user_name))
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
      check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", base::as.character(user_email), ignore.case = TRUE)
      
      # If any emails do not have correct format, throw an error message
      if(check_email == FALSE){
        register_message("Invalid email format.")
        return(NULL)
      }
      
      # Check user table
      check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
      
      # If user exists, throw an error
      if(base::tolower(user_email) %in% base::tolower(check_email_tbl$user_email)){
        register_message(base::sprintf("Email = '%s' already existed in our database. Please choose a different email.", user_email))
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
    api_url <- base::sprintf("https://montilab.bu.edu/SigRepo/send_notifications/register_user?user_name=%s&api_key=%s", user_tbl$user_name[1], base::Sys.getenv("API_KEY"))
    
    # Send email to users through montilab server API
    res <- httr::GET(url = api_url)
    
    # Check status code
    if(res$status_code != 200){
      register_message(base::sprintf("Something went wrong with the API. Cannot register user. Please contact admin for support."))
      return(NULL)
    }else{
      register_message(base::sprintf("Thank you for signing up! Our administrator will contact you once your account is activated."))
    }
    
    # Print message
    base::print(base::sprintf("Adding user = '%s' to database", user_tbl$user_name[1]))
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Output register_message ####
  output$register_message <- shiny::renderUI({
    
    req(register_message())
    
    shiny::p(class = "error-message", shiny::isolate({ register_message() }))
    
  })
  
  # OBSERVE FORGOT PASSWORD BUTTON #####
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
              shiny::textInput(inputId = "psw_username", label = shiny::strong("Enter Your Username"), value = "", width = "100%")
            ),
            shiny::conditionalPanel(
              condition = 'input.psw_lookup_option == "Email"',
              shiny::textInput(inputId = "psw_email", label = shiny::strong("Enter Your Email"), value = "", width = "100%")
            ),
            shiny::uiOutput("forgot_psw_message")
          )
        ),
        
        footer = shiny::tagList(
          shiny::actionButton(inputId = "send_tmp_password", class = "primary-btn", label = shiny::strong("Submit")),
          shiny::actionButton(inputId = "dismiss_forgot_password", class = "primary-btn", label = shiny::strong("Cancel"))
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
    psw_lookup_option <- shiny::isolate({ input$psw_lookup_option })
    
    if(psw_lookup_option == "Username"){
      
      user_name <- shiny::isolate({ input$psw_username }) %>% base::trimws()
      
      # Make sure user_name is not empty
      if(user_name %in% c(NA, "")){
        forgot_psw_message(base::sprintf("Username cannot be empty"))
        return(NULL)
      }
      
      # Check user table
      user_tbl <- check_user_tbl <- SigRepo::searchUser(conn_handler = conn_handler, user_name = user_name)
      
      # If user does not exists, throw an error
      if(nrow(check_user_tbl) == 0){
        forgot_psw_message(base::sprintf("User = '%s' does not exist in our database. Please choose a different name.", user_name))
        return(NULL)        
      }
      
    }else{
      
      user_email <- shiny::isolate({ input$psw_email }) %>% base::trimws()
      
      # Make sure email is not empty
      if(user_email %in% c(NA, "")){
        forgot_psw_message(base::sprintf("Email cannot be empty"))
        return(NULL)
      }
      
      # Check user emails ####
      check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", base::as.character(user_email), ignore.case = TRUE)
      
      # If any emails do not have correct format, throw an error message
      if(check_email == FALSE){
        forgot_psw_message("Invalid email format.")
        return(NULL)
      }
      
      # Check user table
      check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
      
      # If user exists, throw an error
      if(!base::tolower(user_email) %in% base::tolower(check_email_tbl$user_email)){
        forgot_psw_message(base::sprintf("Email = '%s' does not exist in our database. Please choose a different email.", user_email))
        return(NULL)
      }
      
      # Return user_tbl
      user_tbl <- check_email_tbl %>% dplyr::filter(base::tolower(user_email) %in% base::tolower(!!user_email))
      
    }
    
    # Send email to users to notify their account are activated
    api_url <- base::sprintf("https://montilab.bu.edu/SigRepo/send_notifications/send_tmp_password?user_name=%s&api_key=%s", user_tbl$user_name[1], base::Sys.getenv("API_KEY"))
    
    # Send email to users through montilab server API
    res <- httr::GET(url = api_url)
    
    # Check status code
    if(res$status_code != 200){
      forgot_psw_message(base::sprintf("Something went wrong with the API. Cannot send temporary password to user. Please contact admin for support."))
      return(NULL)
    }else{
      forgot_psw_message(base::sprintf("A temporary password has been sent to your email at %s", user_tbl$user_email[1]))
    }
    
    # Print message
    base::print(base::sprintf("Sending temporary password to user = '%s'", user_tbl$user_name[1]))
    
    
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Output forgot_psw_message
  output$forgot_psw_message <- shiny::renderUI({
    
    req(forgot_psw_message())
    
    shiny::p(class = "error-message", shiny::isolate({ forgot_psw_message() }))
    
  })
  
  ## OBSERVE EDIT PROFILE BUTTON #####
  shiny::observeEvent({
    input$edit_profile
  }, {
    
    req(user_login_info())
    
    # Get user table
    user_tbl <- shiny::isolate({ user_login_info() }) %>% 
      base::replace(. == "NA", "") %>% 
      base::replace(. == "NULL", "") %>% 
      base::replace(. == "", "") %>% 
      base::replace(is.na(.), "") %>% 
      base::replace(is.null(.), "")
    
    # Show the modal dialog
    shiny::showModal(
      shiny::modalDialog(
        size = "l", title = shiny::span(shiny::icon(name = "user", lib = "font-awesome"), "USER PROFILE"), 
        shiny::fluidRow(
          shiny::column(
            width = 6, 
            shiny::p(shiny::strong("Username: "), user_tbl$user_name[1]),
            shiny::p(shiny::strong("Email: "), shiny::HTML(base::paste0("<input type='text' id='profile_email' value='", user_tbl$user_email[1]), "' disabled='disabled'>")),
            shiny::p(class = "profile-bullet", shiny::strong("Role: "), user_tbl$user_role[1]),
            shiny::p(class = "profile-bullet", shiny::strong("API Key: "), user_tbl$api_key[1])
          ),
          shiny::column(
            width = 6, 
            shiny::p(class = "profile-bullet", shiny::strong("First Name: "), user_tbl$user_first[1]),
            shiny::p(class = "profile-bullet", shiny::strong("Last Name: "), user_tbl$user_last[1]),
            shiny::p(class = "profile-bullet", shiny::strong("Affliliation: "), user_tbl$user_affiliation[1])
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12, id = "change-profile-email", style = "display: none;",
            shiny::HTML("<span><b>Enter Your New Email</b></span>"),
            shiny::HTML("<input type='text' id='new_profile_email'>")
          ),
          shiny::column(
            width = 12, id = "change-profile-password", style = "display: none;",
            shiny::HTML("<span><b>Enter Your New Password</b></span>"),
            shiny::div(
              class = "change-profile-password",
              shiny::HTML("<input type='password' id='new_profile_password'>"),
              shiny::HTML("<span class='toggle-change-password' onclick='toggle_change_password()'>👁️</span>")
            )
          ),
          shiny::column(
            width = 12,
            shiny::uiOutput(outputId = "change_profile_message")
          )
        ),
        footer = shiny::tagList(
          shinyjs::hidden(shiny::actionButton(inputId = "save_change_email", class = "primary-btn", label = shiny::strong("Save Email"))),
          shinyjs::hidden(shiny::actionButton(inputId = "save_change_password", class = "primary-btn", label = shiny::strong("Save Password"))),
          shiny::actionButton(inputId = "change_email", class = "primary-btn", label = shiny::strong("Change Email")),
          shiny::actionButton(inputId = "change_password", class = "primary-btn", label = shiny::strong("Change Password")),
          shiny::actionButton(inputId = "dismiss_change_profile", class = "primary-btn", label = shiny::strong("Cancel"))
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
    user_tbl <- shiny::isolate({ user_login_info() })
    
    # Get user inputs
    user_email <- shiny::isolate({ input$new_profile_email }) %>% base::trimws()
    
    # Make sure email is not empty
    if(user_email %in% c(NA, "")){
      change_profile_message(base::sprintf("Email cannot be empty"))
      return(NULL)
    }
    
    # Check user emails ####
    check_email <- base::grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", base::as.character(user_email), ignore.case = TRUE)
    
    # If any emails do not have correct format, throw an error message
    if(check_email == FALSE){
      change_profile_message("Invalid email format.")
      return(NULL)
    }
    
    # Check user table
    check_email_tbl <- SigRepo::searchUser(conn_handler = conn_handler)
    
    # If user exists, throw an error
    if(base::tolower(user_email) %in% base::tolower(check_email_tbl$user_email)){
      change_profile_message(base::sprintf("Email = '%s' already existed in our database. Please choose a different email.", user_email))
      return(NULL)
    }    
    
    # Update user email in the database
    SigRepo::updateUser(conn_handler = conn_handler, user_name = user_tbl$user_name[1], email = user_email)
    
    # Update user email in profile
    shiny::updateTextInput(session = session, inputId = "profile_email", value = user_email)
    
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
    user_tbl <- shiny::isolate({ user_login_info() })
    
    # Get user connection handler
    user_conn_handler <- shiny::isolate({ user_conn_handler() })
    
    # Get user inputs
    user_password <- shiny::isolate({ input$new_profile_password }) %>% base::trimws()
    
    # Make sure password is not empty
    if(user_password %in% c(NA, "")){
      change_profile_message(base::sprintf("Password cannot be empty"))
      return(NULL)
    }
    
    # Update user password in the database
    SigRepo::updateUser(conn_handler = conn_handler, user_name = user_tbl$user_name[1], password = user_password)
    
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
    
    shiny::p(class = "error-message", shiny::isolate({ change_profile_message() }))
  })
  
  
  ### HOME PAGE SERVER LOGIC #### 
  
  # plots for homepage 
  # on home page load, use getSignature to get the plot data, then output the plots
  
  
  observe({
    req(input$main_navbar == "home")
    signature_data <- tryCatch({
      SigRepo::searchSignature(conn_handler = user_conn_handler())
    }, error = function(e) {
      showNotification(paste("Error loading signature_data:", e$message), type = "error")
      return(NULL)
    })
    
    # cache the signatures 
    
    homepage_signatures(signature_data)
  })
  
  # reactive for storing
  
  homepage_signatures <- reactiveVal(data.frame())
  
  # homepage plot outputs #####
  
  
  # top ten users
  
  output$top_users_plot <- renderPlot({
    df <- homepage_signatures()
    
    user_counts <- df %>%
      dplyr::count(user_name, name = "num_signatures") %>%
      dplyr::arrange(desc(num_signatures)) %>%
      dplyr::slice_head(n = 10)
    
    ggplot(user_counts, aes( x = reorder(user_name, num_signatures), y = num_signatures)) +
      geom_bar(stat = "identity", fill = "#2c7fb8") +
      coord_flip() +
      labs(
        title = "Top 10 Most Active Users",
        x = "User",
        y = "Number of Signtures"
      ) +
      theme_minimal()
  })
  
  # organism plot
  
  output$organism_plot <- renderPlot({
    df <- homepage_signatures()
    req(nrow(df) > 0, "organism" %in% names(df))
    
    ggplot(df, aes(x = organism, fill = organism)) +
      geom_bar() +
      labs(x = "Organism", y = "Signature Count")+
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  
  # assay type plot
  
  output$assay_plot <- renderPlot({
    df <- homepage_signatures()
    req(nrow(df) >0, "assay_type" %in% names(df))
    
    ggplot(df, aes(x = assay_type, fill = assay_type)) + 
      geom_bar() +
      labs("x = Assay Type", y = "Signature Count") +
      theme_minimal() +
      theme(legend.position = "right")
  })
  
  
  
  # redirect button logic for home page
  
  observeEvent(input$go_signatures, {
    updateTabsetPanel(session, "main_navbar", selected = "signatures")
  })
  
  observeEvent(input$go_collections, {
    updateTabsetPanel(session, "main_navbar", selected = "collections")
  })
  
  # observe event for github docs when it is done.
  
  
  
  search_sig_error_msg <- reactiveVal("")
  # Reactive trigger for refreshing signature table
  signature_update_trigger <- reactiveVal(0)
  
  
  
  # Signature DB reactive — scoped globally so it exists across your server
  signature_db <- reactive({
    signature_update_trigger()  # trigger dependency
    
    if (input$main_navbar != "signatures") return(data.frame())  # only run on the tab
    
    tryCatch({
      df <- SigRepo::searchSignature(conn_handler = user_conn_handler())
      validate(need(nrow(df) > 0, "No Signatures found."))
      df
    }, error = function(e) {
      showNotification(paste("Error fetching signatures:", e$message), type = "error")
      data.frame()
    })
  })
  
  # observe({
  #   req(input$main_navbar == "signatures")
  #   filtered_signatures(signature_db())
  # })
  ######
  
  
  
  
  
  
  #### UPDATE SERVER LOGIC #### 
  
  # Populate the update dropdown (when filtered_signatures or signature_db changes)
  observe({
    sigs <- signature_db()
    all_sigs <- signature_db()
    sigs_to_show <- if (!is.null(sigs) && nrow(sigs) > 0) sigs else all_sigs
    
    if (is.null(sigs_to_show) || nrow(sigs_to_show) == 0) {
      updateSelectInput(session, "update_sig", choices = c("No signatures available" = ""), selected = NULL)
      return()
    }
    
    update_choices <- setNames(
      as.character(sigs_to_show$signature_id),
      paste0(sigs_to_show$signature_name, " (ID: ", sigs_to_show$signature_id, ")")
    )
    
    updateSelectInput(session, "update_sig", choices = update_choices, selected = NULL)
    
  })
  
  
  # Perform update when button clicked
  observeEvent(input$update_btn, {
    req(input$update_sig)
    req(input$update_sig_file)
    
    signature_id_to_be_updated <- input$update_sig
    file <- input$update_sig_file
    
    # Try to read the RDS file
    omic_signature <- tryCatch({
      readRDS(file$datapath)
    }, error = function(e) {
      showNotification(paste("Error reading RDS file:", e$message), type = "error")
      return(NULL)
    })
    
    req(!is.null(omic_signature))  # Stop if file couldn't be read
    
    # Attempt to update
    success <- tryCatch({
      SigRepo::updateSignature(
        conn_handler = user_conn_handler(),
        signature_id = signature_id_to_be_updated,
        omic_signature = omic_signature
      )
      TRUE
    }, error = function(e) {
      showNotification(paste("Update failed:", e$message), type = "error")
      FALSE
    })
    
    if (success) {
      showNotification("Signature updated successfully!", type = "message")
      signature_update_trigger(isolate(signature_update_trigger()) + 1)  # Refresh table
    }
  })
  
  
  
  #### DELETE SERVER LOGIC ####
  
  # # Update delete dropdown whenever filtered_signatures or signature_db changes
  # observe({
  #   sigs <- signature_db()
  #   all_sigs <- signature_db()
  #   
  #   sigs_to_show <- if (!is.null(sigs) && nrow(sigs) > 0) sigs else all_sigs
  #   
  #   if (is.null(sigs_to_show) || nrow(sigs_to_show) == 0) {
  #     updateSelectInput(session, "delete_sig", choices = c("No signatures available" = ""), selected = NULL)
  #     return()
  #   }
  #   
  #   delete_choices <- setNames(
  #     as.character(sigs_to_show$signature_id),
  #     paste0(sigs_to_show$signature_name, " (ID: ", sigs_to_show$signature_id, ")")
  #   )
  #   
  #   updateSelectInput(session, "delete_sig", choices = delete_choices, selected = NULL)
  # })
  # 
  # # delete logic
  # observeEvent(input$delete_btn_sig, {
  #   req(input$delete_sig)  # Ensure something is selected
  #   
  #   signature_id_to_delete <- input$delete_sig
  #   
  #   tryCatch({
  #     # Call your delete function
  #     SigRepo::deleteSignature(signature_id = signature_id_to_delete, conn_handler = user_conn_handler())
  #     
  #     showNotification("Signature deleted successfully.", type = "message")
  #     
  #     # Refresh signature table
  #     signature_update_trigger(isolate(signature_update_trigger()) + 1)
  #     
  #   }, error = function(e) {
  #     showNotification(paste("Error deleting signature:", e$message), type = "error")
  #   })
  # })
  # 
  
  

  
  
  # MAIN TABLE LOGIC ####
  
  # output$signature_tbl <- renderDT({
  #   df <- signature_db()
  #   req(df, nrow(df) > 0)
  #   
  #  
  #   
  #   DT::datatable(
  #     df,
  #     extensions = "Buttons",
  #     filter = "top",  # disable default filters
  #     options = list(
  #       pageLength = -1,
  #       scrollY = "500px",
  #       paging = FALSE,
  #       scrollX = TRUE,
  #       ordering = FALSE,
  #       fixedHeader = TRUE,
  #       dom = 'Bfrtip',
  #       buttons = c('copy', 'csv', 'excel'),
  #       columnDefs = list(
  #         list(targets = c(0,6,7,8,11,14,15,16,19,24,25,26), visible = FALSE)  # Replace with the column indices you want to hide
  #       )
  #       
  #     ),
  #     class = "compact stripe hover nowrap",
  #     selection = "single",
  #     rownames = FALSE
  #   )
  #   
  # })
  # 
  signature_data <- reactive({
    user <- shiny::isolate({ input$username }) %>% base::trimws()
    df <- signature_db()
    
    if (user == "root") {
      return(df)  # Admin sees everything
    } else {
      # Filter to only rows owned by user or visible (visibility == 1)
      df_filtered <- df[df$user_name == user | df$visibility == 1, ]
      return(df_filtered)
    }
  })
  
  
  output$signature_tbl <- renderDT({
    df <- signature_data()
    
    datatable(
      df,
      extensions = "Buttons",
      filter = "top",
      options = list(
        pageLength = -1,
        scrollY = "500px",
        paging = FALSE,
        scrollX = TRUE,
        ordering = FALSE,
        fixedHeader = TRUE,
        dom = 'frtipB',
        buttons = c('copy', 'csv', 'excel')
      ),
      class = "compact stripe hover nowrap",
      selection = "single",
      rownames = FALSE
    )
  })
  
  # UI with action buttons
  output$action_buttons <- renderUI({
    req(input$signature_tbl_rows_selected)  # Fix input ID
    row <- input$signature_tbl_rows_selected
    df <- signature_data()  # Access the full data
    selected_sig <- df[row, ]
    
    tagList(
      h4(paste("Actions for Signature:", selected_sig$signature_name)),
      actionButton("view_btn", "View"),
      actionButton("update_btn", "Update"),
      actionButton("delete_btn", "Delete")
    )
  })
  
  view_mode <- reactiveVal("false")
  
  observeEvent(input$signature_tbl_rows_selected, {
    view_mode("false")
  })
  
  
  
  observeEvent(input$view_btn, {
    view_mode("true")
  })
  
  output$view <- reactive({
    view_mode()
  })
  outputOptions(output, "view", suspendWhenHidden = FALSE)
  

  
  
  observeEvent(input$edit_btn, {
    df <- signature_data()
    row <- input$signature_tbl_rows_selected
    showModal(modalDialog(
      title = "Edit Signature",
      paste("Edit form for:", df[row, "Name"]),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$delete_btn, {
    df <- signature_data()
    row <- input$signature_tbl_rows_selected
    req(row)
    
    selected_sig <- df[row, ]
    
    showModal(modalDialog(
      title = "Delete Signature",
      size = "m",
      tagList(
        p("Are you sure you want to delete this signature?"),
        tags$ul(
          tags$li(strong("Name: "), selected_sig$signature_name),
          tags$li(strong("ID: "), selected_sig$signature_id)
        ),
        br(),
        actionButton("confirm_delete_btn", "Delete", class = "btn-danger"),
        modalButton("Cancel")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_delete_btn, {
    df <- signature_data()
    row <- input$signature_tbl_rows_selected
    req(row)
    
    signature_id_to_delete <- df[row, "signature_id"]
    
    tryCatch({
      # Your deletion call
      SigRepo::deleteSignature(signature_id = signature_id_to_delete, conn_handler = user_conn_handler())
      
      showNotification("Signature deleted successfully.", type = "message")
      
      # Refresh your data
      signature_update_trigger(isolate(signature_update_trigger()) + 1)
      
      # Close the modal
      removeModal()
      
    }, error = function(e) {
      showNotification(paste("Error deleting signature:", e$message), type = "error")
    })
  })
  
  
  observeEvent(input$open_upload_modal, {
    showModal(modalDialog(
      title = "Upload Omic Signature",
      fileInput("upload_file_signature", "Choose a .RDS File",
                accept = ".rds"),
      verbatimTextOutput("upload_output"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("upload_btn_signature", "Upload", class = "btn-success")
      ),
      easyClose = TRUE
    ))
  })
  
  
  #### SUMMARY, SIGNATURE, AND DIFEXP TABLE LOGIC ####
  
  
  # SUMMARY SERVER LOGIC ####
  
  
  
  
  
  
  # SUMMARY PLOTS #### 
  
  output$top_features <- renderPlot({
    sig_obj <- selected_signature()
    sig_tbl <- sig_obj$signature  # Signature table
    
    req(!is.null(sig_tbl), nrow(sig_tbl) > 0)
    
    top_features <- sig_tbl %>%
      
      arrange(desc(score)) %>%
      slice_head(n = 10) %>%
      bind_rows(
        sig_tbl %>%
          arrange(score) %>%
          slice_head(n = 10)
      ) %>%
      mutate(feature_name = factor(feature_name, levels = feature_name[order(score)]))
    
    # generating plot
    
    ggplot(top_features, aes(x = feature_name, y = score, fill > 0)) +
      geom_col()+
      coord_flip()+
      scale_fill_manual(values = c("TRUE" = "#1b9e77", "FALSE" = "#d95f02"),
                        labels = c("Downregulated", "upregulated")) +
      labs(
        title= "Top 10 Positive and Negative Signature Features",
        x = "Feature Name",
        y = "Score",
        fill = "Direction",
      ) +
      theme_minimal()
    
  })
  
  
  
  
  
  
  # DIFEXP ###
  
  output$signature_selected <- reactive({
    !is.null(input$signature_tbl_rows_selected) && length(input$signature_tbl_rows_selected) > 0
  })
  outputOptions(output, "signature_selected", suspendWhenHidden = FALSE)
  
  
  selected_signature <- reactive({
    selected <- input$signature_tbl_rows_selected
    if(length(selected) == 0) return(NULL)
    df <- signature_db()
    sig_name <- df[selected, "signature_name"]
    sig_list <- SigRepo::getSignature(conn_handler = user_conn_handler(),
                                      signature_name = sig_name)
    
    sig_list[[1]]
  })
  
  observeEvent(input$signature_tbl_rows_selected, {
    if (!is.null(input$signature_tbl_rows_selected)) {
      shinyjs::show("details_panel")
    }
  })
  
  observeEvent(input$close_panel, {
    shinyjs::hide("details_panel")
  })
  
  
  
  output$signature_file_table <- DT::renderDataTable({
    sig_obj <- selected_signature()
    req(!is.null(sig_obj))

    datatable(
      sig_obj$signature,
      extensions = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = c('csv', 'excel'),
        pageLength = 100,
        scrollY = "500px",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        paging = TRUE,
        fixedHeader = TRUE,
        columnDefs = list(
          list(targets = c(6), visible = FALSE)
        )
      ),
      rownames = FALSE,
      class = "stripe hover compact nowrap"
    )
  })

  output$difexp_file_table <- DT::renderDataTable({
    sig_obj <- selected_signature()
    req(!is.null(sig_obj))

    datatable(
      sig_obj$difexp,
      extensions  = "Buttons",
      options = list(
        dom = 'Bfrtip',
        buttons = c( 'csv', 'excel'),
        pageLength = 100,
        scrollY = "500px",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        paging = TRUE,
        fixedHeader = TRUE

      ),
      rownames = FALSE,
      class = "stripe hover compact nowrap"
    )
  })

  
  ### SUMMARY SERVER LOGIC ####
  
  ### SIGNATURE TITLE ####
  
  
  output$signature_title <- renderUI({
    sig_obj <- selected_signature()
    metadata <- sig_obj$metadata

    h2(metadata$signature_name)
  })



  output$signature_description <- renderText({
    sig_obj <- selected_signature()
    metadata <- sig_obj$metadata

    # Try to get description, or fallback
    if (!is.null(metadata$description)) {
      return(metadata$description)
    } else {
      return("No description provided for this signature.")
    }
  })


  output$signature_metadata <- renderUI({
    sig_obj <- selected_signature()
    metadata <- sig_obj$metadata

    # Safely extract required fields (will throw if missing)
    req_fields <- c( "organism", "direction_type", "assay_type", "phenotype")

    missing_fields <- setdiff(req_fields, names(metadata))
    if (length(missing_fields) > 0) {
      return(div(class = "alert alert-danger",
                 paste("Missing required metadata fields:", paste(missing_fields, collapse = ", "))))
    }

    # Optional fields
    optional_fields <- c("platform", "sample_type", "covariates", "score_cutoff", "adj_p_cutoff")

    # Build UI output
    output_tags <- tagList(
      p(strong("Organism:"), metadata$organism),
      p(strong("Assay Type:"), metadata$assay_type),
      p(strong("Phenotype:"), metadata$phenotype),
      p(strong("Direction Type:"), metadata$direction_type)
    )

    # Conditionally add optional metadata fields
    for (field in optional_fields) {
      if (!is.null(metadata[[field]])) {
        output_tags <- tagAppendChild(
          output_tags,
          p(strong(paste0(tools::toTitleCase(gsub("_", " ", field)), ":")), metadata[[field]])
        )
      }
    }

    return(output_tags)
  })
  
  
  
  
  #### DOWNLOAD HANDLER LOGIC ####
  
  observe({
    selected <- input$signature_tbl_rows_selected
    if (length(selected) > 0) {
      shinyjs::show("oms_download_wrapper")
    } else {
      shinyjs::hide("oms_download_wrapper")
    }
  })
  
  
  output$export_table <- downloadHandler(
    filename = function() {
      tab <- input$file_tabs
      prefix <- if (tab == "Difexp") "difexp_file_table" else "signature_file_table"
      paste0(prefix, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      sig_obj <- selected_signature()
      req(!is.null(sig_obj))
      
      if (input$file_tabs == "Difexp") {
        df <- sig_obj$difexp
      } else {
        df <- sig_obj$signature
      }
      
      # Handle case where data might be NULL
      if (is.null(df)) {
        write.csv(data.frame(Message = "No data available for this table"), file, row.names = FALSE)
      } else {
        write.csv(df, file, row.names = FALSE)
      }
    }
  )
  
  
  #### Download Omic Signature Logic ####
  
  output$download_oms_handler <- downloadHandler(
    filename = function() {
      selected_row <- input$signature_tbl_rows_selected
      req(length(selected_row) == 1)
      
      df <- signature_db()
      
      # Extract the signature name from the selected row
      sig_name <- df$signature_name[selected_row]
      
      # Sanitize the name to make it filename-safe
      clean_name <- gsub("[^A-Za-z0-9_\\-]", "_", sig_name)
      
      paste0(clean_name, ".rds")
    },
    content = function(file) {
      selected_row <- input$signature_tbl_rows_selected
      req(length(selected_row) == 1)
      
      df <- signature_db()
      sig_id <- df$signature_id[selected_row]
      
      sig_obj <- getSignature(conn_handler = conn_handler, signature_id = sig_id)
      
      saveRDS(sig_obj, file)
    }
  )
  
  
  
  
  #### UPLOAD BUTTON LOGIC ####
  
  
  output$upload_output <- renderText({ "" })  # Initialize
  
  observeEvent(input$upload_btn_signature, {
    file <- input$upload_file_signature
    if (is.null(file)) return()
    
    new_signatures <- tryCatch({
      readRDS(file$datapath)
    }, error = function(e) {
      showNotification(paste("Upload error:", e$message), type = "error")
      return(NULL)
    })
    
    req(!is.null(new_signatures))
    
    log_output <- character()
    
    success <- tryCatch({
      log_output <- capture.output({
        withCallingHandlers({
          SigRepo::addSignature(
            omic_signature = new_signatures,
            conn_handler = user_conn_handler()
          )
        }, message = function(m) {
          log_output <<- c(log_output, paste("MESSAGE:", conditionMessage(m)))
          invokeRestart("muffleMessage")
        }, warning = function(w) {
          log_output <<- c(log_output, paste("WARNING:", conditionMessage(w)))
          invokeRestart("muffleWarning")
        })
      })
      TRUE
    }, error = function(e) {
      log_output <<- c(log_output, paste("ERROR:", e$message))
      FALSE
    })
    
    output$upload_output <- renderText({
      paste(log_output, collapse = "\n")
    })
    
    if (success) {
      showNotification("Signature uploaded successfully!", type = "message")
      signature_update_trigger(isolate(signature_update_trigger()) + 1)
      removeModal()
    }
  })
  

  #### COLLECTION SERVER LOGIC #####
  
  search_collection_error_msg <- reactiveVal("")
  
  # reactive trigger for refreshing collection table
  
  collection_update_trigger <- reactiveVal(0)
  
  filtered_collection <- reactiveVal(data.frame())
  
  
  # collection DB reactive
  collection_db <- reactive({
    
    collection_update_trigger()
    if (input$main_navbar != 'collections') return(data.frame())
    
    tryCatch({
      df <- SigRepo::searchCollection(conn_handler = user_conn_handler())
      validate(need(nrow(df) > 0, "No Collections Found."))
      df
    },error = function(e){
      showNotification(paste("Error fetching Collections", e$message), type = "error")
      data.frame()
    })
  })
  
  observe({
    req(input$main_navbar == "collections")
    filtered_collection(collection_db())
  })
  
  
  ####Delete Collection Server Logic ####
  
  observe({
    collections <- filtered_collection()
    all_collections <- signature_db()
    
    # Ensure data frames
    if (!is.data.frame(collections)) collections <- data.frame()
    if (!is.data.frame(all_collections)) all_collections <- data.frame()
    
    # Choose collections to show
    collections_to_show <- if (nrow(collections) > 0) collections else all_collections
    
    # Check if we have any valid collections
    if (nrow(collections_to_show) == 0 ||
        !all(c("collection_id", "collection_name") %in% names(collections_to_show))) {
      updateSelectInput(session, "delete_collection", choices = c("No collections available" = ""), selected = NULL)
      return()
    }
    
    # Check lengths
    if (length(collections_to_show$collection_id) != length(collections_to_show$collection_name)) {
      updateSelectInput(session, "delete_collection", choices = c("Collection data invalid" = ""), selected = NULL)
      return()
    }
    
    # Build named choices
    delete_choices <- setNames(
      as.character(collections_to_show$collection_id),
      paste0(collections_to_show$collection_name, " (ID: ", collections_to_show$collection_id, ")")
    )
    
    updateSelectInput(session, "delete_collection", choices = delete_choices, selected = NULL)
  })
  
  
  
  
  
  observeEvent(input$delete_btn_collection, {
    req(input$delete_collection)  # Ensure something is selected
    
    collection_id_to_delete <- input$delete_collection
    
    tryCatch({
      # Call your delete function
      SigRepo::deleteCollection(collection_id = collection_id_to_delete, conn_handler = user_conn_handler())
      
      showNotification("Collection deleted successfully.", type = "message")
      
      # Refresh signature table
      collection_update_trigger(isolate(collection_update_trigger()) + 1)
      
    }, error = function(e) {
      showNotification(paste("Error deleting collection:", e$message), type = "error")
    })
  })
  
  # Main Collection Table Logic ####
  
  output$collection_tbl <- DT::renderDataTable({
    df <- filtered_collection()
    req(!is.null(df), nrow(df) > 0)
    
    # Summarize by collection, collapsing signature names
    df_grouped <- df %>%
      group_by(collection_id, collection_name, description, user_name, date_created, visibility) %>%
      summarise(
        signatures = paste(signature_name, collapse = ", "),
        .groups = "drop"
      )
    
    DT::datatable(
      df_grouped,
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        dom = "Bfrtip",
        buttons = c('copy', 'csv', 'excel'),
        scrollX = TRUE,
        scrollY = "300px",
        fixedHeader = TRUE
      ),
      class = "stripe hover compact nowrap",
      rownames = FALSE
    )
  })
  
  
  # Upload Collection Logic ####
  
  observeEvent(input$upload_btn_collection, {
    
    collection_file <- input$upload_file_collection
    
    # Ensure a file was uploaded
    req(collection_file)
    
    # Read the RDS file into an object
    tryCatch({
      rds_object <- readRDS(collection_file$datapath)
      
      # Now pass the R object to the SigRepo function
      SigRepo::addCollection(
        conn_handler = user_conn_handler(),
        omic_collection = rds_object
      )
      
      showNotification("Collection uploaded and added successfully!", type = "message")
      
      # Trigger a refresh after successful upload
      collection_update_trigger(isolate(collection_update_trigger()) + 1)
      
    }, error = function(e) {
      showNotification(paste("Error reading or uploading collection:", e$message), type = "error")
    })
  })
  
  #### REFERENCE FEATURES TAB LOGIC ####
  
  
  
  ref_features <- eventReactive(input$search_ref_btn, {
    
    
    
    
    tryCatch({
      features <- SigRepo::searchFeature(conn_handler = user_conn_handler(),
                                         assay_type = input$ref_assay)
      
      if (!is.null(features) && "organism" %in% colnames(features)){
        features <- subset(features, organism == input$ref_organism)
      } else {
        showNotification("Warning: no `organism` column found in returned data", type = "warning")
      }
      
      return(features)
    }, error = function(e){
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  
  # render the table
  
  output$ref_feature_tbl <- renderDT({
    req(ref_features())
    
    datatable(
      ref_features(),
      filter = "top",
      extensions = "Buttons",
      options = list(
        pageLength = 10,
        buttons = ('csv'),
        scrollY = "600px",      # Set the scrollable height
        scrollCollapse = TRUE,  # Collapse if fewer rows
        paging = TRUE,         # Optional: remove paging
        fixedHeader = TRUE,
        columnDefs = list(
          list(targets = c(5), visible = FALSE)
        )
      ),
      class = "stripe hover compact nowrap",
      rownames = FALSE
    )
  })
  
  
  
  #### HypeR SERVER LOGIC ####
  
  
  # signature choices logic for hypeR
  
  observe({
    df <- signature_db()
    req(!is.null(df), nrow(df) > 0)
    
    
    sig_choices <- unique(df$signature_name)
    
    
    updateSelectInput(
      session,
      inputId = "signature_add",
      choices = sig_choices,
      selected = NULL
    )
  })
  
  
  #### HypeR SERVER LOGIC ####
  
  # signature tab #
  
  # observeEvent(input$signature_add, {
  #   
  #   signature_id <- filtered_signatures()
  #   
  #   signature <- SigRepo::getSignature(conn_handler = user_conn_handler(),
  #                                 signature_id = signature_id)
  #   
  #   experiment_label <- input$experiment_label
  #   
  #   signature_label <- input$signature_label
  #   
  #   
  #   if (!(experiment_label %in% names(data)))
  # })
  
  
  
  
  
  # output$geneset_table <- DT::renderDataTable({
  #   gsets <- genesets()
  #   req(!is.null(gsets), length(gsets) > 0)
  # 
  #   df <- data.frame(
  #     Geneset = names(gsets),
  #     Symbols = sapply(gsets, function(x) paste(head(x, 5), collapse = ","))
  #   )
  # 
  #   DT::datatable(
  #     df,
  #     rownames = FALSE,
  #     options = list(
  #       pageLength = 20,
  #       lengthMenu = c(10, 20, 50, 100),
  #       scrollX = TRUE,
  #       autoWidth = TRUE,
  #       dom = 'tip',
  #       columnDefs = list(
  #         list(className = 'dt-center', targets = "_all")
  #       )
  #     ),
  #     class = "compact stripe hover nowrap"
  #   )
  # })
  
  
  #### RENDER KNITR HTML LOGIC ####
  
  output$report_ui <- renderUI({
    # Knit or render the Rmd to HTML
    rmarkdown::render(
      input = "report.Rmd",          # Your .Rmd file
      output_file = "report.html",   # Output HTML file
      output_dir = tempdir(),        # Place in a temporary directory
      quiet = TRUE
    )
    
    # Read and display the HTML
    HTML(
      paste(readLines(file.path(tempdir(), "report.html")), collapse = "\n")
    )
  })
  
  ### RESOURCES SERVER LOGIC ####
  selected_resource <- reactiveVal("getting_started")
  
  observeEvent(input$res_getting_started, selected_resource("getting_started"))
  observeEvent(input$res_file_formats, selected_resource("file_formats"))
  observeEvent(input$res_permissions, selected_resource("permissions"))
  observeEvent(input$res_rclient, selected_resource("rclient"))
  
  output$resource_content <- renderUI({
    switch(selected_resource(),
           
           "getting_started" = tagList(
             h4("📘 Getting Started"),
             p("This section helps you understand the basics of SigRepo."),
             tags$ul(
               tags$li("How to upload a signature"),
               tags$li("How to search and filter"),
               tags$li("Basic usage")
             )
           ),
           
           "file_formats" = tagList(
             h4("📂 File Formats"),
             p("Supported file formats for upload:"),
             tags$ul(
               tags$li(".RDS - for omic_signature objects"),
               tags$li(".CSV - for batch metadata uploads")
             )
           ),
           
           "permissions" = tagList(
             h4("🔒 Access & Permissions"),
             p("Learn how visibility and permissions work for users."),
             p("Users can share or restrict access to signatures using role-based permissions.")
           ),
           
           "rclient" = tagList(
             h4("📦 R-Client Tutorial"),
             tags$iframe(
               src = "knits/R_client.html",
               width = "100%",
               height = "600px",
               style = "border: none;"
             )
           )
    )
  })
  
  observeEvent(input$delete_signature_id, {
    sig_id <- input$delete_signature_id
    showModal(
      modalDialog(
        title = "Confirm Delete",
        paste("Are you sure you want to delete signature ID", sig_id, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_delete_signature", "Delete", class = "btn-danger")
        )
      )
    )
    
    # Store ID in a reactiveVal so we can access it in confirm observer
    selected_sig_id_to_delete(sig_id)
  })
  
  # Store the selected ID in a reactiveVal
  selected_sig_id_to_delete <- reactiveVal(NULL)
  
  observeEvent(input$confirm_delete_signature, {
    sig_id <- selected_sig_id_to_delete()
    req(sig_id)
    
    tryCatch({
      SigRepo::deleteSignature(signature_id = sig_id, conn_handler = user_conn_handler())
      
      showNotification("Signature deleted successfully.", type = "message")
      signature_update_trigger(isolate(signature_update_trigger()) + 1)
    }, error = function(e) {
      showNotification(paste("Error deleting signature:", e$message), type = "error")
    })
    
    removeModal()
  })
  
  
} # server end bracket

## Start the app ####
shiny::shinyApp(ui=ui, server=server)




