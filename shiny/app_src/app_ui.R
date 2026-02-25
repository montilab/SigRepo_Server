ui <- shiny::fluidPage(
  
  shinyjs::useShinyjs(),
  
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
    class = "login-wrapper", id = "login-wrapper",
    
    shiny::div(
      class = "login-container",
      
      shiny::div(
        class = "login-form-title",
        shiny::span(class = "login-title", shiny::h1("Sign In"))
      ),
      
      tags$form(
        class = "login-form",
        
        shiny::div(
          class = "validate-input",
          shiny::HTML("<span class='login-label'><b>Username</b></span>"),
          shiny::HTML("<input class='login-input' type='text' id='username' onkeypress='login_keypress(e)' placeholder='Enter Username'>")
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
        
        shiny::div(
          class = "register", 
          shiny::span("Don't have an acount.", shiny::HTML("<a href='#' id='register' class='action-button'>Register here!</a>"))
        )
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
      tabPanel("Browsing", reference_module_ui("references")),
      tabPanel("Feedback ", feedbackUI("feedback")),
      
      
      # custom logo for longevity consortium, need to add it still ###
      
      
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
        src = "images/LC_logo.png ",
        class = "navbar-custom-logo"
      )
      
      
      
    )
  )
  
)
