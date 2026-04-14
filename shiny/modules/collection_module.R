# Collection page module

collection_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("collection_page"))

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " {
        padding-top: 28px;
        padding-bottom: 32px;
      }

      ", page_selector, " .collection-hero {
        margin-bottom: 18px;
        padding: 22px 26px;
        border-radius: 14px;
        background: linear-gradient(135deg, #184766 0%, #2d6f8f 100%);
        color: #ffffff;
        box-shadow: 0 10px 24px rgba(24, 71, 102, 0.16);
      }

      ", page_selector, " .collection-hero h2 {
        margin: 0 0 8px 0;
        font-weight: 700;
      }

      ", page_selector, " .collection-hero p {
        margin: 0;
        color: rgba(255, 255, 255, 0.88);
      }

      ", page_selector, " .collection-card {
        margin-bottom: 18px;
        padding: 20px 22px;
        border: 1px solid #d9e3ec;
        border-radius: 12px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }

      ", page_selector, " .collection-card h3,
      ", page_selector, " .collection-card h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .collection-toolbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 16px;
        flex-wrap: wrap;
        margin-bottom: 16px;
      }

      ", page_selector, " .collection-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        align-items: center;
      }

      ", page_selector, " .collection-selected {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }

      ", page_selector, " .collection-selected .collection-label {
        font-size: 12px;
        font-weight: 700;
        color: #4e6782;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .collection-selected .collection-name {
        font-size: 20px;
        font-weight: 700;
        color: #17324d;
      }

      ", page_selector, " .collection-summary-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      ", page_selector, " .collection-summary-item {
        padding: 12px 14px;
        border-radius: 10px;
        background: #f6f9fc;
        border: 1px solid #e1ebf2;
      }

      ", page_selector, " .collection-summary-item strong {
        display: block;
        margin-bottom: 4px;
        color: #0f3b63;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .collection-summary-item span {
        color: #17324d;
        font-size: 15px;
        font-weight: 600;
      }

      ", page_selector, " .collection-empty {
        padding: 20px;
        border: 1px dashed #c5d5e3;
        border-radius: 10px;
        background: #f8fbfd;
        color: #4b647e;
      }

      ", page_selector, " .collection-helper {
        margin-bottom: 14px;
        color: #597189;
      }
    "))),

    div(
      id = ns("collection_page"),

      div(
        class = "collection-hero",
        tags$h2("Browse Collections"),
        tags$p(
          "Select a collection to review its metadata and included signatures without opening a separate dialog."
        )
      ),

      div(
        class = "collection-card",
        div(
          class = "collection-toolbar",
          actionButton(
            ns("open_upload_modal"),
            "Upload Collection",
            icon = icon("upload"),
            class = "btn-primary"
          ),
          uiOutput(ns("collection_actions"))
        ),
        DT::DTOutput(ns("collection_tbl"))
      ),

      div(
        class = "collection-card",
        tags$h3("Selected Collection"),
        p(
          class = "collection-helper",
          "Selecting a row loads the collection details below so you can inspect metadata and member signatures in one place."
        ),
        uiOutput(ns("collection_detail_panel"))
      )
    )
  )
}

collection_module_server <- function(id, collection_db, user_conn_handler, collection_trigger) {
  moduleServer(id, function(input, output, session) {
    selected_collection <- reactiveVal(NULL)
    collection_object <- reactiveVal(NULL)

    collection_field_value <- function(collection_df, field, default = "Not available") {
      if (is.null(collection_df) || !field %in% names(collection_df)) {
        return(default)
      }

      value <- collection_df[[field]][1]
      if (is.null(value) || is.na(value) || identical(as.character(value), "")) {
        return(default)
      }

      as.character(value)
    }

    fetch_selected_collection <- function(collection_id) {
      SigRepo::getCollection(
        conn_handler = user_conn_handler(),
        collection_id = collection_id
      )
    }

    df_grouped <- reactive({
      req(user_conn_handler())

      df <- collection_db()

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
          signatures = paste(signature_name, collapse = ", "),
          .groups = "drop"
        )
    })

    output$collection_tbl <- renderDT({
      DatatableFX(
        df = df_grouped(),
        scrollY = "500px",
        row_selection = "single"
      )
    }, server = TRUE)

    observeEvent(input$collection_tbl_rows_selected, {
      row <- input$collection_tbl_rows_selected

      if (length(row) == 0) {
        selected_collection(NULL)
        collection_object(NULL)
        return()
      }

      df <- df_grouped()
      collection_selected <- df[row, , drop = FALSE]
      selected_collection(collection_selected)

      tryCatch({
        collection_object(fetch_selected_collection(collection_selected$collection_id[[1]]))
      }, error = function(e) {
        collection_object(NULL)
        showNotification(
          paste("Failed to load collection details:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    output$collection_actions <- renderUI({
      collection_selected <- selected_collection()

      if (is.null(collection_selected)) {
        return(
          div(
            class = "collection-selected",
            tags$span(class = "collection-label", "Selection"),
            tags$span(class = "collection-name", "No collection selected")
          )
        )
      }

      div(
        class = "collection-toolbar",
        div(
          class = "collection-selected",
          tags$span(class = "collection-label", "Selected Collection"),
          tags$span(class = "collection-name", collection_field_value(collection_selected, "collection_name"))
        ),
        div(
          class = "collection-actions",
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
            class = "collection-empty",
            "Choose a collection from the table above to inspect its metadata and included signatures."
          )
        )
      }

      tagList(
        div(
          class = "collection-summary-grid",
          div(
            class = "collection-summary-item",
            tags$strong("Collection"),
            tags$span(collection_field_value(collection_selected, "collection_name"))
          ),
          div(
            class = "collection-summary-item",
            tags$strong("Owner"),
            tags$span(collection_field_value(collection_selected, "user_name"))
          ),
          div(
            class = "collection-summary-item",
            tags$strong("Visibility"),
            tags$span(collection_field_value(collection_selected, "visibility"))
          ),
          div(
            class = "collection-summary-item",
            tags$strong("Signatures"),
            tags$span(collection_field_value(collection_selected, "signature_count", "0"))
          )
        ),
        tabsetPanel(
          tabPanel("Metadata", uiOutput(session$ns("collection_metadata"))),
          tabPanel("Signatures", DT::DTOutput(session$ns("collection_sig_tbl")))
        )
      )
    })

    output$collection_metadata <- renderUI({
      req(selected_collection())

      tagList(
        p(tags$strong("Description:"), collection_field_value(selected_collection(), "description")),
        p(tags$strong("Date Created:"), collection_field_value(selected_collection(), "date_created")),
        p(tags$strong("User:"), collection_field_value(selected_collection(), "user_name")),
        p(tags$strong("Total Signatures:"), collection_field_value(selected_collection(), "signature_count", "0"))
      )
    })

    output$collection_sig_tbl <- renderDataTable({
      req(selected_collection(), collection_db())

      filtered_collection_tbl <- collection_db()[
        collection_db()$collection_name == selected_collection()$collection_name[[1]],
        ,
        drop = FALSE
      ]

      DatatableFX(
        filtered_collection_tbl,
        hidden_columns = c(3, 4)
      )
    }, server = TRUE)

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

        SigRepo::addCollection(
          conn_handler = user_conn_handler(),
          omic_collection = rds_object
        )

        showNotification("Collection uploaded and added successfully!")
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
          paste(
            "Are you sure you want to delete collection:",
            selected_collection()$collection_name[[1]],
            "?"
          ),
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
        collection_trigger(collection_trigger() + 1)
      }, error = function(e) {
        message("Error deleting collection: ", e$message)
        showNotification(
          paste("Failed to delete collection:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$update_btn, {
      req(selected_collection())

      collection_name <- selected_collection()$collection_name[[1]]

      showModal(modalDialog(
        title = "Update collection",
        paste("Collection to update:", collection_name),
        fileInput(session$ns("update_file_upload"), "Choose an RDS file", accept = ".rds"),
        p("The selected collection will be updated with the new collection object you upload."),
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })

    observeEvent(input$access_btn, {
      req(selected_collection())

      user_tbl <- SigRepo::searchUser(conn_handler = user_conn_handler())

      showModal(modalDialog(
        title = paste("Manage Users for Collection:", selected_collection()$collection_name[[1]]),
        tabsetPanel(
          tabPanel(
            "Add to Collection",
            fluidRow(
              column(
                6,
                selectInput(
                  inputId = "user_selector",
                  label = "Select users to add:",
                  choices = user_tbl$user_name,
                  multiple = TRUE
                )
              )
            ),
            uiOutput("access_type_ui"),
            actionButton("add_users_confirm", "Add Users", class = "btn-primary")
          ),
          tabPanel(
            "Delete from Collection",
            p("Delete user functionality goes here.")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
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
  })
}
