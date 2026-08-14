# modules/login_module.R

login_module_ui <- function(id) {
  ns <- NS(id)
  
  div(
    class = "login-wrapper",
    id = ns("login-wrapper"),
    
    div(
      class = "login-container",
      
      div(class = "login-title", h2("Sign In")),
      
      tags$form(
        class = "login-form",
        
        div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Username</b></span>"),
          div(
            class = "username-container",
            HTML(
              sprintf("<input class='login-input' type='text' id='%s' onkeypress='login_keypress(event)' placeholder='Enter Username'>", ns("username"))
            )
          )
        ),
        
        div(
          class = "validate-input",
          HTML("<span class='login-label'><b>Password</b></span>"),
          div(
            class = "password-container",
            HTML(
              sprintf("<input class='login-input' type='password' id='%s' onkeypress='login_keypress(event)' placeholder='Enter Password'>", ns("password"))
            ),
            HTML("<span class='toggle-password' onclick='toggle_password()'>👁️</span>")
          )
        ),
        
        div(class = "validate-message", uiOutput(ns("login_error_message"))),
        
        div(
          class = "validate-button",
          actionButton(
            inputId = ns("sign_in_btn"),
            class = "sign-in-button",
            label = "Login",
            onclick = "login_keypress(event)"
          ),
          div(
            class = "forgot_psw",
            HTML(
              sprintf("<a href='#' id='%s' class='action-button'>Forgot password?</a>", ns("forget_password"))
            )
          )
        ),
        
        div(class = "register", span(
          "Don't have an account? ",
          HTML(sprintf("<a href='#' id='%s' class='action-button'>Register here!</a>", ns("register")))
        ))
      )
    )
  )
}

login_module_server <- function(id, validate_login) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Login validation error message
    output$login_error_message <- renderUI({
      NULL  # Placeholder; replace based on your logic
    })
    
    # Handle login
    observeEvent(input$sign_in_btn, {
      username <- session$ns("username")  # Input from JS will not reach here via input$...
      password <- session$ns("password")  # Same
      
      # You would need to fetch the actual values via JS and send them to Shiny.
      # So, you may need JS -> Shiny.setInputValue('username', value) on keypress
      # Here's a placeholder for triggering login logic:
      print("Login button clicked — validate credentials here.")
    })
    
    # Other observers for forgot password / register can be added here
  })
}
