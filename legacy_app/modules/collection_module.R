# Collection page module

collection_module_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("collection_page"),
    class = "sr-page",
    div(
      class = "sr-page-inner",

      tags$header(
        class = "page-titlebar",
        div(
          class = "page-titlebar-text",
          tags$h1("Collections"),
          span(
            class = "page-titlebar-sub",
            "Browse the repository's collections, then open one to review its metadata and member signatures."
          )
        ),
        div(
          class = "page-titlebar-actions",
          actionButton(
            ns("open_upload_modal"),
            "Upload collection",
            icon = icon("upload"),
            class = "btn-primary"
          )
        )
      ),

      div(
        class = "card",
        uiOutput(ns("collection_actions")),
        p(
          class = "sr-helper",
          "Select a collection row to make it active. Use View to load the full collection details below."
        ),
        DT::DTOutput(ns("collection_tbl"))
      ),

      div(
        class = "card",
        tags$h3("Selected collection"),
        p(
          class = "sr-helper",
          "The active collection summary appears immediately. Use View to fetch the collection details and member signatures on demand."
        ),
        uiOutput(ns("collection_detail_panel"))
      )
    )
  )
}

# add_collection_fn is injected so the upload path can be tested without a
# database (tests/testthat/test-shiny-collection-module.R).
collection_module_server <- function(id, collection_db, user_conn_handler, collection_trigger,
                                     add_collection_fn = SigRepo::addCollection) {
  if (is.null(add_collection_fn)) add_collection_fn <- SigRepo::addCollection

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_collection <- reactiveVal(NULL)
    collection_object <- reactiveVal(NULL)

    fetch_selected_collection <- function(collection_id) {
      SigRepo::getCollection(
        conn_handler = user_conn_handler(),
        collection_id = collection_id
      )
    }

    # One row per collection. searchCollection() returns one row per member
    # signature, and a data.frame() with no columns at all when there are
    # none, which group_by() cannot take: hand back the empty shape instead,
    # which DatatableFX() renders as "No data available." (issue #159).
    grouped_collections <- reactive({
      req(user_conn_handler())

      df <- collection_db()
      group_columns <- c("collection_id", "collection_name", "description", "user_name", "date_created", "visibility")
      if (!is.data.frame(df) || nrow(df) == 0 || !all(group_columns %in% names(df))) {
        return(data.frame(
          collection_id = numeric(0), collection_name = character(0), description = character(0),
          user_name = character(0), date_created = character(0), visibility = integer(0),
          signature_count = integer(0), signature_preview = character(0),
          stringsAsFactors = FALSE
        ))
      }

      df %>%
        dplyr::group_by(
          collection_id,
          collection_name,
          description,
          user_name,
          date_created,
          visibility
        ) %>%
        dplyr::summarise(
          signature_count = dplyr::n(),
          signature_preview = paste(utils::head(signature_name, 5), collapse = ", "),
          .groups = "drop"
        )
    })

    current_collection_signatures <- reactive({
      req(selected_collection(), collection_db())

      collection_db()[
        collection_db()$collection_id == selected_collection()$collection_id[[1]],
        ,
        drop = FALSE
      ]
    })

    output$collection_tbl <- renderDT({
      DatatableFX(
        df = grouped_collections(),
        hidden_columns = c(0, 7),
        scrollY = "500px",
        row_selection = "single"
      )
    }, server = TRUE)

    observeEvent(input$collection_tbl_rows_selected, {
      rows <- input$collection_tbl_rows_selected

      if (length(rows) == 0) {
        selected_collection(NULL)
        collection_object(NULL)
        return()
      }

      selected_collection(grouped_collections()[rows, , drop = FALSE])
      collection_object(NULL)
    })

    output$collection_actions <- renderUI({
      collection_selected <- selected_collection()

      if (is.null(collection_selected)) {
        return(
          div(
            class = "sr-selected",
            tags$span(class = "sr-selected-label", "Selection"),
            tags$span(class = "sr-selected-name", "No collection selected")
          )
        )
      }

      div(
        class = "sr-toolbar",
        div(
          class = "sr-selected",
          tags$span(class = "sr-selected-label", "Selected collection"),
          tags$span(class = "sr-selected-name", field_value(collection_selected, "collection_name"))
        ),
        div(
          class = "sr-actions",
          actionButton(ns("view_btn"), "View", class = "btn-primary"),
          actionButton(ns("refresh_btn"), "Refresh"),
          actionButton(ns("update_btn"), "Update"),
          actionButton(ns("delete_btn"), "Delete"),
          actionButton(ns("access_btn"), "Access"),
          downloadButton(ns("download_btn"), "Download")
        )
      )
    })

    output$collection_detail_panel <- renderUI({
      collection_selected <- selected_collection()

      if (is.null(collection_selected)) {
        return(
          div(
            class = "sr-empty",
            "Choose a collection from the table above to inspect it."
          )
        )
      }

      if (is.null(collection_object())) {
        return(
          tagList(
            div(
              class = "sr-summary",
              div(
                class = "sr-summary-item",
                tags$strong("Collection"),
                tags$span(field_value(collection_selected, "collection_name"))
              ),
              div(
                class = "sr-summary-item",
                tags$strong("Owner"),
                tags$span(field_value(collection_selected, "user_name"))
              ),
              div(
                class = "sr-summary-item",
                tags$strong("Visibility"),
                tags$span(field_value(collection_selected, "visibility"))
              ),
              div(
                class = "sr-summary-item",
                tags$strong("Signatures"),
                tags$span(field_value(collection_selected, "signature_count", "0"))
              )
            ),
            div(
              class = "sr-empty",
              "The selected collection has not been loaded yet. Click View to fetch its metadata and member signatures."
            )
          )
        )
      }

      tagList(
        div(
          class = "sr-summary",
          div(
            class = "sr-summary-item",
            tags$strong("Collection"),
            tags$span(field_value(collection_selected, "collection_name"))
          ),
          div(
            class = "sr-summary-item",
            tags$strong("Owner"),
            tags$span(field_value(collection_selected, "user_name"))
          ),
          div(
            class = "sr-summary-item",
            tags$strong("Visibility"),
            tags$span(field_value(collection_selected, "visibility"))
          ),
          div(
            class = "sr-summary-item",
            tags$strong("Signatures"),
            tags$span(field_value(collection_selected, "signature_count", "0"))
          )
        ),
        tabsetPanel(
          tabPanel(
            "Metadata",
            div(class = "sr-table", DT::DTOutput(ns("collection_metadata_table")))
          ),
          tabPanel(
            "Signatures",
            DT::DTOutput(ns("collection_sig_tbl"))
          )
        )
      )
    })

    output$collection_metadata_table <- DT::renderDataTable({
      req(selected_collection())

      collection_selected <- selected_collection()
      metadata_df <- data.frame(
        Field = c("collection_id", names(collection_selected)[names(collection_selected) != "collection_id"]),
        Value = c(
          collection_selected$collection_id[[1]],
          vapply(
            names(collection_selected)[names(collection_selected) != "collection_id"],
            function(field) field_value(collection_selected, field),
            character(1)
          )
        ),
        stringsAsFactors = FALSE
      )

      DatatableFX(
        metadata_df,
        hidden_columns = integer(0),
        scrollY = "360px"
      )
    }, server = TRUE)

    output$collection_sig_tbl <- DT::renderDataTable({
      req(selected_collection(), collection_object())

      DatatableFX(
        current_collection_signatures(),
        hidden_columns = c(3, 4),
        scrollY = "500px"
      )
    }, server = TRUE)

    observeEvent(input$view_btn, {
      req(selected_collection())

      tryCatch({
        collection_object(fetch_selected_collection(selected_collection()$collection_id[[1]]))
        showNotification("Collection details loaded.", type = "message")
      }, error = function(e) {
        collection_object(NULL)
        showNotification(
          paste("Failed to load collection details:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$refresh_btn, {
      req(selected_collection())

      tryCatch({
        collection_object(fetch_selected_collection(selected_collection()$collection_id[[1]]))
        showNotification("Collection details refreshed.", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Failed to refresh collection details:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$open_upload_modal, {
      showModal(upload_modal_ui(session$ns, "Collection"))
    })

    observeEvent(input$upload_btn, {
      req(input$upload_file)

      tryCatch({
        rds_object <- readRDS(input$upload_file$datapath)

        # The dialog's Private/Public choice arrives as "FALSE"/"TRUE".
        add_collection_fn(
          conn_handler = user_conn_handler(),
          omic_collection = rds_object,
          visibility = isTRUE(as.logical(input$visibility))
        )

        showNotification("Collection uploaded and added successfully!", type = "message")
        collection_trigger(isolate(collection_trigger()) + 1)
      }, error = function(e) {
        showNotification(
          paste("Error reading or uploading collection rds object:", e$message),
          type = "error"
        )
      })

      removeModal()
    })

    observeEvent(input$delete_btn, {
      req(selected_collection())

      showModal(
        modalDialog(
          title = "Confirm Delete",
          sprintf(
            "Are you sure you want to delete collection %s?",
            htmltools::htmlEscape(selected_collection()$collection_name[[1]])
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_delete_collection"), "Delete", class = "btn-danger")
          )
        )
      )
    })

    observeEvent(input$confirm_delete_collection, {
      req(selected_collection())

      collection_id <- selected_collection()$collection_id[[1]]

      tryCatch({
        SigRepo::deleteCollection(
          conn_handler = user_conn_handler(),
          collection_id = collection_id
        )

        showNotification("Collection deleted successfully.", type = "message")
        removeModal()
        selected_collection(NULL)
        collection_object(NULL)
        collection_trigger(isolate(collection_trigger()) + 1)
      }, error = function(e) {
        showNotification(
          paste("Failed to delete collection:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$update_btn, {
      req(selected_collection())

      showModal(
        modalDialog(
          title = "Update Collection",
          paste("Collection to update:", selected_collection()$collection_name[[1]]),
          fileInput(session$ns("update_file_upload"), "Choose an RDS file", accept = ".rds"),
          p("The selected collection will be updated with the new collection object you upload."),
          footer = tagList(
            modalButton("Cancel")
          )
        )
      )
    })

    observeEvent(input$access_btn, {
      req(selected_collection())

      showModal(
        modalDialog(
          title = paste("Collection Access:", selected_collection()$collection_name[[1]]),
          easyClose = TRUE,
          footer = modalButton("Close"),
          div(
            class = "sr-helper",
            "Collection access management is still collection-specific and has not been modernized yet in this refactor."
          )
        )
      )
    })

    output$download_btn <- downloadHandler(
      filename = function() {
        req(selected_collection())
        paste0("collection_", selected_collection()$collection_name[[1]], ".rds")
      },
      content = function(file) {
        req(selected_collection())

        collection_download <- fetch_selected_collection(selected_collection()$collection_id[[1]])
        saveRDS(collection_download, file)
      }
    )

    # Exposed so the tab's state can be asserted on in testServer(); the app
    # itself does not read these.
    list(
      grouped = grouped_collections,
      selected_collection = selected_collection
    )
  })
}
