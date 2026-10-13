# The Feedback tab: a name, a message, and a Slack webhook on the other end.

feedbackUI <- function(id) {
  ns <- NS(id)

  div(
    id = ns("feedback_page"),
    class = "sr-page",
    div(
      class = "sr-page-inner",

      tags$header(
        class = "page-titlebar",
        div(
          class = "page-titlebar-text",
          tags$h1("Feedback"),
          span(class = "page-titlebar-sub", "Tell the SigRepo team what is broken or missing. It goes straight to the lab's Slack.")
        )
      ),

      div(
        class = "card",
        textInput(ns("name"), "Your name", "", width = "100%"),
        textAreaInput(ns("feedback"), "Your feedback", "", rows = 6, width = "100%"),
        div(
          class = "sr-actions",
          actionButton(ns("submit"), "Send feedback", class = "btn-primary", icon = icon("paper-plane")),
          textOutput(ns("response"), inline = TRUE)
        )
      )
    )
  )
}

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
