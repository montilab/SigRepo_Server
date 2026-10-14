# The server: session plumbing, the sign-in flows (modules/auth_module.R), the
# two repository frames every tab reads, and the tab modules.

server <- function(input, output, session) {
  base::cat("\nSession started.\n")

  # A periodic flush keeps the websocket busy behind a proxy with an idle
  # timeout (nginx drops a quiet connection after 60 seconds by default), so a
  # tab left open does not grey out. It used to fire every two seconds and
  # print a dot to the log each time (issue #159).
  keepalive <- shiny::reactiveTimer(20000)
  shiny::observe({
    keepalive()
  })

  session$onSessionEnded(function() {
    base::cat("\nSession ended.\n")
  })

  shiny::onStop(function() {
    base::cat("\nSession stopped.\n")
  })

  # Sign in, sign out, register, reset a password, edit the profile. The
  # per-user connection handler it returns is what every tab queries with.
  auth <- auth_server(input, output, session, conn_handler = conn_handler)
  user_conn_handler_rv <- auth$user_conn_handler

  # Bumped by the tabs after an upload or a delete to refetch the frames.
  signature_trigger <- shiny::reactiveVal(0)
  collection_trigger <- shiny::reactiveVal(0)

  signature_db <- shiny::reactive({
    shiny::req(user_conn_handler_rv())
    signature_trigger()

    base::tryCatch({
      df <- SigRepo::searchSignature(conn_handler = user_conn_handler_rv())

      if (nrow(df) == 0) {
        shiny::showNotification("No Signatures Found.", type = "warning")
        return(base::data.frame())
      }
      df
    }, error = function(e) {
      shiny::showNotification(paste("Error fetching signatures:", e$message), type = "error")
      base::data.frame()
    })
  })

  collection_db <- shiny::reactive({
    shiny::req(user_conn_handler_rv())
    collection_trigger()

    base::tryCatch({
      df <- SigRepo::searchCollection(conn_handler = user_conn_handler_rv())

      if (nrow(df) == 0) {
        shiny::showNotification("No collections found.", type = "warning")
        return(base::data.frame())
      }
      df
    }, error = function(e) {
      shiny::showNotification(paste("Error fetching Collections:", e$message), type = "error")
      base::data.frame()
    })
  })

  home_module_server(
    "home",
    signature_db = signature_db,
    parent_session = session
  )

  signature_module_server(
    "signatures",
    signature_db = signature_db,
    user_conn_handler = user_conn_handler_rv,
    signature_trigger = signature_trigger
  )

  collection_module_server(
    "collections",
    collection_db = collection_db,
    user_conn_handler = user_conn_handler_rv,
    collection_trigger = collection_trigger
  )

  annotate_module_server(
    "annotate",
    signature_db = signature_db,
    user_conn_handler = user_conn_handler_rv
  )

  compare_module_server(
    "compare",
    signature_db = signature_db,
    user_conn_handler = user_conn_handler_rv
  )

  reference_module_server("references", user_conn_handler = user_conn_handler_rv)
  feedbackServer("feedback")
}
