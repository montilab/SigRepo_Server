# Signature page module


# Signatures UI
signaturesUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("open_upload_modal"), "Upload Signature", icon = icon("upload"), class = "btn-primary"),
    fluidRow(
      column(width = 8)
    ),
    br(),
    uiOutput(ns("action_buttons")),
    DTOutput(ns("signature_tbl")),
    absolutePanel(
      id = ns("details_panel"),
      top = 100, right = 100, width = 800, draggable = TRUE,
      style = "z-index: 10; background-color: #fff; padding: 20px; border: 1px solid #ccc; border-radius: 8px; box-shadow: 0px 4px 10px rgba(0,0,0,0.1); display: none;",
      tags$div(
        style = "position: absolute; top: 10px; right: 10px; cursor: pointer; font-size: 20px;",
        actionLink(ns("close_panel"), label = HTML("&times;"))
      ),
      conditionalPanel(
        condition = "output.view === 'true'",
        tabsetPanel(
          tabPanel("Summary",
                   div(id = ns("oms_download_wrapper"),
                       downloadButton(ns("download_oms_handler"), "Download OmicSignature", class = "submit-button", onclick = "sig_tbl_select_rows();")
                   ),
                   uiOutput(ns("signature_title")),
                   uiOutput(ns("signature_description")),
                   br(), br(),
                   fluidRow(
                     column(
                       width = 4,
                       div(class = "signature-info",
                           h4("🧬 Signature Info"),
                           tags$hr(),
                           uiOutput(ns("signature_metadata"))
                       )
                     ),
                     column(
                       width = 8,
                       tabsetPanel(
                         tabPanel("Top Features", plotOutput(ns("top_features")))
                       )
                     )
                   )
          ),
          tabPanel("Signature", DTOutput(ns("signature_file_table"))),
          tabPanel("Difexp", DTOutput(ns("difexp_file_table")))
        )
      )
    )
  )
}

signaturesServer <- function(id, user_conn_handler, signature_update_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === Signature DB ===
    signature_db <- reactive({
      signature_update_trigger()
      if (input$main_navbar != "signatures") return(data.frame())
      
      tryCatch({
        df <- SigRepo::searchSignature(conn_handler = user_conn_handler())
        validate(need(nrow(df) > 0, "No Signatures found."))
        df
      }, error = function(e) {
        showNotification(paste("Error fetching signatures:", e$message), type = "error")
        data.frame()
      })
    })
    
    # === Populate Update Dropdown ===
    observe({
      sigs <- signature_db()
      all_sigs <- signature_db()
      sigs_to_show <- if (!is.null(sigs) && nrow(sigs) > 0) sigs else all_sigs
      
      if (is.null(sigs_to_show) || nrow(sigs_to_show) == 0) {
        updateSelectInput(session, ns("update_sig"), choices = c("No signatures available" = ""), selected = NULL)
        return()
      }
      
      update_choices <- setNames(
        as.character(sigs_to_show$signature_id),
        paste0(sigs_to_show$signature_name, " (ID: ", sigs_to_show$signature_id, ")")
      )
      
      updateSelectInput(session, ns("update_sig"), choices = update_choices, selected = NULL)
    })
    
    # === Update Logic ===
    observeEvent(input$update_btn, {
      req(input$update_sig, input$update_sig_file)
      
      sig_id <- input$update_sig
      file <- input$update_sig_file
      
      omic_signature <- tryCatch({
        readRDS(file$datapath)
      }, error = function(e) {
        showNotification(paste("Error reading RDS file:", e$message), type = "error")
        return(NULL)
      })
      
      req(!is.null(omic_signature))
      
      success <- tryCatch({
        SigRepo::updateSignature(
          conn_handler = user_conn_handler(),
          signature_id = sig_id,
          omic_signature = omic_signature
        )
        TRUE
      }, error = function(e) {
        showNotification(paste("Update failed:", e$message), type = "error")
        FALSE
      })
      
      if (success) {
        showNotification("Signature updated successfully!", type = "message")
        signature_update_trigger(isolate(signature_update_trigger()) + 1)
      }
    })
    
    # === Main Table Rendering ===
    signature_data <- reactive({
      user <- isolate({ input$username }) %>% trimws()
      df <- signature_db()
      
      if (user == "root") {
        return(df)
      } else {
        df[df$user_name == user | df$visibility == 1, ]
      }
    })
    
    output$signature_tbl <- renderDT({
      df <- signature_data()
      
      datatable(
        df,
        extensions = "Buttons",
        filter = "top",
        options = list(
          pageLength = -1,
          scrollY = "500px",
          paging = FALSE,
          scrollX = TRUE,
          ordering = FALSE,
          fixedHeader = TRUE,
          dom = 'frtipB',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "compact stripe hover nowrap",
        selection = "single",
        rownames = FALSE
      )
    })
    
    # === Action Buttons UI ===
    output$action_buttons <- renderUI({
      req(input$signature_tbl_rows_selected)
      row <- input$signature_tbl_rows_selected
      df <- signature_data()
      selected_sig <- df[row, ]
      
      tagList(
        h4(paste("Actions for Signature:", selected_sig$signature_name)),
        actionButton(ns("view_btn"), "View"),
        actionButton(ns("update_btn"), "Update"),
        actionButton(ns("delete_btn"), "Delete")
      )
    })
    
    # === View Mode Toggle ===
    view_mode <- reactiveVal("false")
    
    observeEvent(input$signature_tbl_rows_selected, {
      view_mode("false")
    })
    
    observeEvent(input$view_btn, {
      view_mode("true")
    })
    
    output$view <- reactive({
      view_mode()
    })
    outputOptions(output, "view", suspendWhenHidden = FALSE)
  })
}
