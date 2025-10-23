# UI module for feedback form
feedbackUI <- function(id) {
  ns <- NS(id)
  div(
    style = "padding-top: 70px;",
    tagList(
      textInput(ns("name"), "Your Name", ""),
      textAreaInput(ns("feedback"), "Your Feedback", "", rows = 5),
      actionButton(ns("submit"), "Submit Feedback"),
      textOutput(ns("response"))
    )
  )
}

# Server logic for feedback form
feedbackServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$submit, {
      req(input$feedback)
      
      # Load Slack webhook URL at runtime
      slack_webhook_url <- Sys.getenv("SLACK_WEBHOOK_URL")
     
      
      
      
      # Build Slack message
      slack_msg <- list(
        text = paste0(
          "*New Feedback Submitted:*\n",
          "*From:* ", input$name, "\n",
          "*Message:* ", input$feedback
        )
      )
      
      # Send to Slack
      res <- tryCatch({
        POST(
          url = slack_webhook_url,
          body = toJSON(slack_msg, auto_unbox = TRUE),
          content_type_json()
        )
      }, error = function(e) {
        output$response <- renderText(paste("Error sending to Slack:", e$message))
        return(NULL)
      })
      
      if (!is.null(res) && status_code(res) == 200) {
        output$response <- renderText("Feedback sent successfully!")
      } else if (!is.null(res)) {
        output$response <- renderText("Failed to send feedback. Slack returned an error.")
      }
    })
  })
}
