# Signature page module

signature_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("signature_page"))

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " {
        padding-top: 28px;
        padding-bottom: 32px;
      }

      ", page_selector, " .signature-hero {
        margin-bottom: 18px;
        padding: 22px 26px;
        border-radius: 14px;
        background: linear-gradient(135deg, #143a5a 0%, #245f86 100%);
        color: #ffffff;
        box-shadow: 0 10px 24px rgba(20, 58, 90, 0.16);
      }

      ", page_selector, " .signature-hero h2 {
        margin: 0 0 8px 0;
        font-weight: 700;
      }

      ", page_selector, " .signature-hero p {
        margin: 0;
        color: rgba(255, 255, 255, 0.88);
      }

      ", page_selector, " .signature-card {
        margin-bottom: 18px;
        padding: 20px 22px;
        border: 1px solid #d9e3ec;
        border-radius: 12px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }

      ", page_selector, " .signature-card h3,
      ", page_selector, " .signature-card h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .signature-toolbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 16px;
        flex-wrap: wrap;
        margin-bottom: 16px;
      }

      ", page_selector, " .signature-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        align-items: center;
      }

      ", page_selector, " .signature-selected {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }

      ", page_selector, " .signature-selected .signature-label {
        font-size: 12px;
        font-weight: 700;
        color: #4e6782;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .signature-selected .signature-name {
        font-size: 20px;
        font-weight: 700;
        color: #17324d;
      }

      ", page_selector, " .signature-summary-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      ", page_selector, " .signature-summary-item {
        padding: 12px 14px;
        border-radius: 10px;
        background: #f6f9fc;
        border: 1px solid #e1ebf2;
      }

      ", page_selector, " .signature-summary-item strong {
        display: block;
        margin-bottom: 4px;
        color: #0f3b63;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .signature-summary-item span {
        color: #17324d;
        font-size: 15px;
        font-weight: 600;
      }

      ", page_selector, " .signature-empty {
        padding: 20px;
        border: 1px dashed #c5d5e3;
        border-radius: 10px;
        background: #f8fbfd;
        color: #4b647e;
      }

      ", page_selector, " .signature-helper {
        margin-bottom: 14px;
        color: #597189;
      }

      ", page_selector, " .signature-metadata-table .dataTables_wrapper {
        margin-top: 8px;
      }

      ", page_selector, " .signature-basket-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        align-items: center;
      }

      ", page_selector, " .signature-toolbar-primary {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
      }

      ", page_selector, " .signature-basket-list {
        display: flex;
        flex-direction: column;
        gap: 10px;
      }

      ", page_selector, " .signature-basket-item {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 12px;
        padding: 12px 14px;
        border: 1px solid #e1ebf2;
        border-radius: 10px;
        background: #f6f9fc;
      }

      ", page_selector, " .signature-basket-item-main {
        display: flex;
        flex-direction: column;
        gap: 4px;
      }

      ", page_selector, " .signature-basket-item-title {
        font-weight: 600;
        color: #17324d;
      }

      ", page_selector, " .signature-basket-item-meta {
        color: #597189;
        font-size: 12px;
      }
    "))),

    div(
      id = ns("signature_page"),

      div(
        class = "signature-hero",
        tags$h2("Browse Signatures"),
        tags$p(
          "Select a signature from the repository to review metadata, raw signature values, and differential expression in one place."
        )
      ),

      div(
        class = "signature-card",
        div(
          class = "signature-toolbar",
          div(
            class = "signature-toolbar-primary",
            actionButton(
              ns("open_upload_modal"),
              "Upload Signature",
              icon = icon("upload"),
              class = "btn-primary"
            ),
            uiOutput(ns("basket_toggle"))
          ),
          uiOutput(ns("signature_actions"))
        ),
        p(
          class = "signature-helper",
          "Highlight one or more rows to add them to the basket. The most recently clicked row becomes the active selection, and View will load its full contents on demand."
        ),
        DT::DTOutput(ns("signature_tbl"))
      ),

      div(
        class = "signature-card",
        tags$h3("Selected Signature"),
        p(
          class = "signature-helper",
          "Selecting a row updates the active signature. Use View to load the full metadata and data tables below."
        ),
        uiOutput(ns("signature_detail_panel"))
      )
    )
  )
}


signature_module_server <- function(id, signature_db, user_conn_handler, signature_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_sig <- reactiveVal(NULL)
    sig_object <- reactiveVal(NULL)
    access_user_tbl <- reactiveVal(NULL)
    basket_signatures <- reactiveVal(data.frame())
    last_clicked_row <- reactiveVal(NULL)

    current_sig <- reactive({
      req(sig_object())
      sig_object()[[1]]
    })

    signature_field_value <- function(sig_df, field, default = "Not available") {
      if (is.null(sig_df) || !field %in% names(sig_df)) {
        return(default)
      }

      value <- sig_df[[field]][1]
      if (is.null(value) || is.na(value) || identical(as.character(value), "")) {
        return(default)
      }

      as.character(value)
    }

    fetch_selected_signature <- function(sig_id) {
      SigRepo::getSignature(
        conn_handler = user_conn_handler(),
        signature_id = sig_id
      )
    }

    output$signature_tbl <- renderDT({
      df <- signature_db()

      DatatableFX(
        df = df,
        hidden_columns = c(0, 6, 7, 8, 9, 11, 12, 14, 15, 16, 18, 19, 21, 22, 24, 25, 26),
        scrollY = "500px",
        row_selection = "multiple"
      )
    }, server = TRUE)

    observeEvent(input$signature_tbl_row_last_clicked, {
      row <- input$signature_tbl_row_last_clicked
      if (!is.null(row) && length(row) == 1) {
        last_clicked_row(row)
      }
    })

    observeEvent(input$signature_tbl_rows_selected, {
      rows <- input$signature_tbl_rows_selected

      if (length(rows) == 0) {
        selected_sig(NULL)
        sig_object(NULL)
        last_clicked_row(NULL)
        return()
      }

      detail_row <- last_clicked_row()
      if (is.null(detail_row) || !detail_row %in% rows) {
        detail_row <- rows[[length(rows)]]
        last_clicked_row(detail_row)
      }

      df <- signature_db()
      sig <- df[detail_row, , drop = FALSE]
      selected_sig(sig)
      sig_object(NULL)
    })

    output$signature_actions <- renderUI({
      sig <- selected_sig()

      if (is.null(sig)) {
        return(
          div(
            class = "signature-selected",
            tags$span(class = "signature-label", "Selection"),
            tags$span(class = "signature-name", "No signature selected")
          )
        )
      }

        div(
          class = "signature-toolbar",
          div(
            class = "signature-selected",
            tags$span(class = "signature-label", "Selected Signature"),
            tags$span(class = "signature-name", signature_field_value(sig, "signature_name"))
          ),
          div(
            class = "signature-actions",
            actionButton(ns("view_btn"), "View", class = "btn-primary"),
            actionButton(ns("add_selected_to_basket_btn"), "Add Selected Rows"),
            actionButton(ns("add_to_basket_btn"), "Add to Basket"),
            actionButton(ns("update_btn"), "Update"),
            actionButton(ns("delete_btn"), "Delete"),
            actionButton(ns("access_btn"), "Access"),
            downloadButton(ns("download_btn"), "Download")
          )
        )
    })

    output$basket_toggle <- renderUI({
      basket_df <- basket_signatures()

      div(
        actionButton(
          ns("open_basket_btn"),
          label = sprintf(
            "Basket (%s)",
            nrow(basket_df)
          ),
          icon = icon("shopping-basket")
        )
      )
    })

    output$basket_actions <- renderUI({
      div(
        class = "signature-basket-actions",
        actionButton(ns("remove_from_basket_btn"), "Remove Last"),
        actionButton(ns("clear_basket_btn"), "Clear Basket"),
        downloadButton(ns("download_basket_btn"), "Download Basket")
      )
    })

    output$basket_list <- renderUI({
      basket_df <- basket_signatures()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        return(
          div(
            class = "signature-empty",
            "No signatures in the basket yet."
          )
        )
      }

      basket_items <- lapply(seq_len(nrow(basket_df)), function(i) {
        sig <- basket_df[i, , drop = FALSE]

        div(
          class = "signature-basket-item",
          div(
            class = "signature-basket-item-main",
            tags$span(
              class = "signature-basket-item-title",
              signature_field_value(sig, "signature_name")
            ),
            tags$span(
              class = "signature-basket-item-meta",
              paste(
                signature_field_value(sig, "user_name", "Unknown owner"),
                "|",
                signature_field_value(sig, "visibility", "Unknown visibility"),
                "|",
                signature_field_value(sig, "date_created", "Unknown date")
              )
            )
          ),
          actionButton(
            ns(paste0("remove_basket_item_", i)),
            "Remove",
            class = "btn-default btn-sm"
          )
        )
      })

      div(
        class = "signature-basket-list",
        basket_items
      )
    })

    output$signature_detail_panel <- renderUI({
      sig <- selected_sig()

      if (is.null(sig)) {
        return(
          div(
            class = "signature-empty",
            "Choose a signature from the table above to inspect it."
          )
        )
      }

      if (is.null(sig_object())) {
        return(
          tagList(
            div(
              class = "signature-summary-grid",
              div(
                class = "signature-summary-item",
                tags$strong("Signature"),
                tags$span(signature_field_value(sig, "signature_name"))
              ),
              div(
                class = "signature-summary-item",
                tags$strong("Owner"),
                tags$span(signature_field_value(sig, "user_name"))
              ),
              div(
                class = "signature-summary-item",
                tags$strong("Visibility"),
                tags$span(signature_field_value(sig, "visibility"))
              ),
              div(
                class = "signature-summary-item",
                tags$strong("Created"),
                tags$span(signature_field_value(sig, "date_created"))
              )
            ),
            div(
              class = "signature-empty",
              "The selected signature has not been loaded yet. Click View to fetch its metadata and data tables."
            )
          )
        )
      }

      tagList(
        div(
          class = "signature-summary-grid",
          div(
            class = "signature-summary-item",
            tags$strong("Signature"),
            tags$span(signature_field_value(sig, "signature_name"))
          ),
          div(
            class = "signature-summary-item",
            tags$strong("Owner"),
            tags$span(signature_field_value(sig, "user_name"))
          ),
          div(
            class = "signature-summary-item",
            tags$strong("Visibility"),
            tags$span(signature_field_value(sig, "visibility"))
          ),
          div(
            class = "signature-summary-item",
            tags$strong("Created"),
            tags$span(signature_field_value(sig, "date_created"))
          )
        ),
        tabsetPanel(
          tabPanel(
            "Metadata",
            div(class = "signature-metadata-table", DT::DTOutput(session$ns("signature_metadata_table")))
          ),
          tabPanel("Signature", DT::DTOutput(session$ns("signature_file_table"))),
          tabPanel("Differential Expression", DT::DTOutput(session$ns("difexp_file_table")))
        )
      )
    })

    output$signature_metadata_table <- DT::renderDataTable({
      req(selected_sig())

      sig <- selected_sig()
      df <- data.frame(
        Field = names(sig),
        Value = unlist(sig[1, ], use.names = FALSE),
        stringsAsFactors = FALSE
      )

      DatatableFX(
        df,
        hidden_columns = integer(0),
        scrollY = "360px"
      )
    }, server = TRUE)

    output$signature_file_table <- DT::renderDataTable({
      req(current_sig())

      DatatableFX(
        current_sig()$signature,
        hidden_columns = integer(0),
        scrollY = "500px"
      )
    }, server = TRUE)

    output$difexp_file_table <- DT::renderDataTable({
      req(current_sig())

      DatatableFX(
        current_sig()$difexp,
        hidden_columns = integer(0),
        scrollY = "500px"
      )
    }, server = TRUE)

    observeEvent(input$view_btn, {
      req(selected_sig())

      tryCatch({
        sig_object(fetch_selected_signature(selected_sig()$signature_id[[1]]))
        showNotification("Signature details loaded.", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Failed to load signature details:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$add_to_basket_btn, {
      req(selected_sig())

      basket_df <- basket_signatures()
      sig <- selected_sig()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        basket_signatures(sig)
        showNotification("Signature added to basket.", type = "message")
        return()
      }

      if (sig$signature_id[[1]] %in% basket_df$signature_id) {
        showNotification("That signature is already in the basket.", type = "warning")
        return()
      }

      basket_signatures(rbind(basket_df, sig))
      showNotification("Signature added to basket.", type = "message")
    })

    observeEvent(input$add_selected_to_basket_btn, {
      selected_rows <- input$signature_tbl_rows_selected
      df <- signature_db()

      if (length(selected_rows) == 0) {
        showNotification("Highlight one or more signature rows first.", type = "warning")
        return()
      }

      selected_df <- df[selected_rows, , drop = FALSE]
      basket_df <- basket_signatures()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        basket_signatures(selected_df)
        showNotification(sprintf("%s signature(s) added to basket.", nrow(selected_df)), type = "message")
        return()
      }

      new_rows <- selected_df[!selected_df$signature_id %in% basket_df$signature_id, , drop = FALSE]

      if (nrow(new_rows) == 0) {
        showNotification("All highlighted signatures are already in the basket.", type = "warning")
        return()
      }

      basket_signatures(rbind(basket_df, new_rows))
      showNotification(sprintf("%s signature(s) added to basket.", nrow(new_rows)), type = "message")
    })

    observeEvent(input$open_basket_btn, {
      showModal(
        modalDialog(
          title = "Download Basket",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          p(
            class = "signature-helper",
            "Review the current basket, remove items if needed, or download everything together as a zip archive."
          ),
          uiOutput(ns("basket_actions")),
          uiOutput(ns("basket_list"))
        )
      )
    })

    observeEvent(input$remove_from_basket_btn, {
      basket_df <- basket_signatures()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        showNotification("The basket is already empty.", type = "warning")
        return()
      }

      basket_signatures(basket_df[-nrow(basket_df), , drop = FALSE])
      showNotification("Removed the most recent signature from the basket.", type = "message")
    })

    observeEvent(input$clear_basket_btn, {
      basket_signatures(data.frame())
      showNotification("Basket cleared.", type = "message")
    })

    observe({
      basket_df <- basket_signatures()
      if (is.null(basket_df) || nrow(basket_df) == 0) {
        return()
      }

      lapply(seq_len(nrow(basket_df)), function(i) {
        observeEvent(input[[paste0("remove_basket_item_", i)]], {
          current_basket <- basket_signatures()
          if (!is.null(current_basket) && nrow(current_basket) >= i) {
            basket_signatures(current_basket[-i, , drop = FALSE])
            showNotification("Signature removed from basket.", type = "message")
          }
        }, ignoreInit = TRUE)
      })
    })

    observeEvent(input$open_upload_modal, {
      showModal(upload_modal_ui(session$ns, type = "Signature"))
    })

    observeEvent(input$upload_btn, {
      req(input$upload_file)

      tryCatch({
        rds_object <- readRDS(input$upload_file$datapath)

        SigRepo::addSignature(
          conn_handler = user_conn_handler(),
          omic_signature = rds_object
        )

        showNotification("Signature uploaded and added successfully!")
        signature_trigger(isolate(signature_trigger()) + 1)
      }, error = function(e) {
        showNotification(
          paste("Error reading or uploading signature rds object:", e$message),
          type = "error"
        )
      })

      removeModal()
    })

    observeEvent(input$delete_btn, {
      req(selected_sig())

      showModal(
        delete_modal_ui(
          session$ns,
          type = "Signature",
          name = selected_sig()$signature_name[[1]]
        )
      )
    })

    observeEvent(input$confirm_delete_signature, {
      req(selected_sig())

      sig_id <- selected_sig()$signature_id[[1]]

      tryCatch({
        SigRepo::deleteSignature(
          conn_handler = user_conn_handler(),
          signature_id = sig_id
        )

        showNotification("Signature deleted successfully.", type = "message")
        removeModal()
        selected_sig(NULL)
        sig_object(NULL)
        signature_trigger(signature_trigger() + 1)
      }, error = function(e) {
        message("Error deleting signature: ", e$message)
        showNotification(
          paste("Failed to delete signature:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$update_btn, {
      req(selected_sig())

      sig_name <- selected_sig()$signature_name[[1]]

      showModal(modalDialog(
        title = "Update signature",
        paste("Signature to update:", sig_name),
        fileInput(session$ns("update_file_upload"), "Choose an RDS file", accept = ".rds"),
        p("The selected signature will be updated with the new signature object you upload."),
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })

    observeEvent(input$access_btn, {
      req(selected_sig())

      user_tbl <- SigRepo::searchUser(conn_handler = user_conn_handler())
      access_user_tbl(user_tbl)

      showModal(
        manage_users_modal_ui(
          session$ns,
          name = selected_sig()$signature_name[[1]],
          user_tbl = user_tbl
        )
      )
    })

    observeEvent(input$confirm_add_users, {
      req(selected_sig(), access_user_tbl())

      manage_users_modal_server(
        input = input,
        output = output,
        session = session,
        name = selected_sig()$signature_name[[1]],
        user_tbl = access_user_tbl(),
        type = "Signature",
        selected = reactive(selected_sig()),
        user_conn_handler = user_conn_handler
      )
    })

    output$download_btn <- downloadHandler(
      filename = function() {
        req(selected_sig())
        paste0("signature_", selected_sig()$signature_name[[1]], ".rds")
      },
      content = function(file) {
        req(selected_sig())

        sig_download <- fetch_selected_signature(selected_sig()$signature_id[[1]])
        saveRDS(sig_download, file)
      }
    )

    output$download_basket_btn <- downloadHandler(
      filename = function() {
        paste0("signature_basket_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        basket_df <- basket_signatures()
        req(!is.null(basket_df), nrow(basket_df) > 0)

        export_dir <- file.path(tempdir(), paste0("signature_basket_", as.integer(Sys.time())))
        dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

        exported_files <- character(0)

        for (i in seq_len(nrow(basket_df))) {
          sig_id <- basket_df$signature_id[[i]]
          sig_name <- basket_df$signature_name[[i]]
          sig_download <- fetch_selected_signature(sig_id)

          safe_name <- gsub("[^A-Za-z0-9_-]", "_", sig_name)
          out_file <- file.path(export_dir, paste0("signature_", safe_name, ".rds"))
          saveRDS(sig_download, out_file)
          exported_files <- c(exported_files, out_file)
        }

        utils::zip(zipfile = file, files = exported_files, flags = "-j")
      }
    )
  })
}
