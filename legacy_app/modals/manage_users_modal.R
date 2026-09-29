# Grant users access to a signature (issue #96).
#
# This modal used to carry a `type` switch with a Signature branch and a
# Collection branch. The Collection branch was unreachable -- nothing wired this
# modal to the Collection tab -- and wrong where it could be read: it called
# addUserToSignature() with a collection_id argument that function does not
# accept, through a misspelled connection handler. Rather than repair a branch
# no caller reaches, the modal now takes the id and the granting function from
# its caller, so a Collection tab can reuse it by passing its own granter.


manage_users_modal_ui <- function(ns, entity_label, entity_name, user_tbl) {
  choices <- if (is.data.frame(user_tbl)) user_tbl$user_name else character(0)

  modalDialog(
    title = paste0("Manage access to ", entity_label, ": ", entity_name),
    size = "l",
    fluidPage(
      fluidRow(
        column(
          7,
          selectInput(
            inputId = ns("user_selector"),
            label = "Users to add",
            choices = choices,
            selected = NULL,
            multiple = TRUE
          )
        ),
        column(
          5,
          selectInput(
            inputId = ns("access_type_selector"),
            label = "Access level",
            choices = SIGNATURE_ACCESS_TYPES,
            selected = "viewer",
            multiple = FALSE
          )
        )
      ),
      helpText(
        "Viewers can see this signature. Editors can also change it — note that",
        "database grants are table-level, so an editor can modify signatures",
        "beyond this one. Owners can additionally manage access and delete it."
      ),
      tags$h4("Current access"),
      uiOutput(ns("user_access_rows"))
    ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Close"),
      actionButton(ns("add_users_confirm"), "Add Users", class = "btn-primary")
    )
  )
}


#' Wire the Manage Access modal.
#'
#' Call this once when the parent module starts, not from inside an
#' observeEvent: registering it per click stacked a fresh copy of these
#' observers each time, so the nth confirmation fired n grants.
#'
#' @param entity_id Reactive returning the id being shared, or NULL.
#' @param entity_name Reactive returning its display name.
#' @param user_conn_handler Reactive returning the connection handler.
#' @param grant_fn Granting function; see grant_signature_access().
#' @param current_access reactiveVal holding the existing access table.
manage_users_modal_server <- function(input,
                                      output,
                                      session,
                                      entity_id,
                                      entity_name,
                                      user_conn_handler,
                                      grant_fn,
                                      current_access) {
  output$user_access_rows <- renderUI({
    access <- current_access()

    if (is.null(access)) {
      return(div(
        class = "signature-empty",
        "Current access could not be loaded. You can still grant access below."
      ))
    }

    if (nrow(access) == 0) {
      return(div(class = "signature-empty", "No users have been granted access yet."))
    }

    DT::DTOutput(session$ns("user_access_tbl"))
  })

  output$user_access_tbl <- DT::renderDataTable({
    access <- current_access()
    shiny::req(!is.null(access), nrow(access) > 0)

    DatatableFX(
      access,
      hidden_columns = integer(0),
      scrollY = "200px",
      row_selection = "none",
      column_labels = prettify_colnames(names(access))
    )
  }, server = TRUE)

  observeEvent(input$add_users_confirm, {
    shiny::req(entity_id())

    users <- input$user_selector
    access_type <- input$access_type_selector

    if (length(users) == 0) {
      showNotification("Select at least one user to add.", type = "warning")
      return()
    }

    if (length(access_type) != 1 || !nzchar(access_type)) {
      showNotification("Choose an access level.", type = "warning")
      return()
    }

    results <- tryCatch(
      grant_fn(
        conn_handler = user_conn_handler(),
        signature_id = entity_id(),
        user_names = users,
        access_type = access_type
      ),
      error = function(e) {
        data.frame(
          user_name = users,
          success = FALSE,
          message = conditionMessage(e),
          stringsAsFactors = FALSE
        )
      }
    )

    summary <- grant_summary(results)
    showNotification(summary$text, type = summary$type, duration = 8)

    current_access(fetch_signature_access(user_conn_handler(), entity_id()))

    # Keep the modal open on a partial failure so the user can see which grants
    # did not land and try again; it used to close regardless.
    if (all(results$success)) {
      removeModal()
    }
  })
}
