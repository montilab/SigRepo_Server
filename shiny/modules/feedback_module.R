# module for feedback form that gets sent to slack api

slack_webhook_url <- "https://hooks.slack.com/services/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"

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
      
      # Build Slack message
      slack_msg <- list(
        text = paste0(
          "*New Feedback Submitted:*\n",
          "*From:* ", input$name, "\n",
          "*Message:* ", input$feedback
        )
      )
      
      # Send to Slack
      res <- POST(
        url = slack_webhook_url,
        body = toJSON(slack_msg, auto_unbox = TRUE),
        content_type_json()
      )
      
      if (status_code(res) == 200) {
        output$response <- renderText("Feedback sent successfully!")
      } else {
        output$response <- renderText("Failed to send feedback.")
      }
    })
  })
}