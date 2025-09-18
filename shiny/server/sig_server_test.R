# signaturesModule.R




signaturesModuleUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),  # Ensure this is only used once globally in your main app UI
    
    # Tabs on top
    tabsetPanel(
      id = ns("main_tabs"),
      type = "tabs",
      
      # ---- Upload Tab ----
      tabPanel("Upload",
               h4("Upload Signature"),
               fileInput(ns("upload_file_signature"), "Choose a file to upload"),
               actionButton(ns("upload_btn_signature"), "Upload")
      ),
      
      # ---- Filter Tab ----
      tabPanel("Filter",
               h4("Filter Signatures"),
               selectizeInput(
                 inputId = ns("search_options_signature"),
                 label = "Search Options:",
                 choices = c(
                   "Choose from a list of options below" = "", 
                   "signature_name", 
                   "organism", 
                   "phenotype",
                   "sample_type", 
                   "platform_id", 
                   "assay_type"
                 ),
                 multiple = TRUE,
                 width = "100%"
               ),
               shinyjs::hidden(selectizeInput(ns("signature_name"), "signature_name:", choices = "", multiple = TRUE)),
               shinyjs::hidden(selectizeInput(ns("organism"), "organism:", choices = "", multiple = TRUE)),
               shinyjs::hidden(selectizeInput(ns("phenotype"), "phenotype:", choices = "", multiple = TRUE)),
               shinyjs::hidden(selectizeInput(ns("sample_type"), "sample_type:", choices = "", multiple = TRUE)),
               shinyjs::hidden(selectizeInput(ns("platform_id"), "platform:", choices = "", multiple = TRUE)),
               shinyjs::hidden(selectizeInput(ns("assay_type"), "assay_type:", choices = "", multiple = TRUE)),
               br(),
               actionButton(ns("search_signature"), "Search", class = "submit-button"),
               actionButton(ns("clear_filters"), "Clear Filters"),
               br(), br(),
               uiOutput(ns("search_sig_error_msg"))
      ),
      
      # ---- Update Tab ----
      tabPanel("Update",
               h4("Update a Signature"),
               selectInput(ns("update_sig"), "Select Signature to Update", choices = "", multiple = FALSE),
               fileInput(ns("update_sig_file"), "Choose a file to update"),
               actionButton(ns("update_btn"), "Update"),
               uiOutput(ns("update_sig_error_msg"))
      ),
      
      # ---- Delete Tab ----
      tabPanel("Delete",
               h4("Delete Signature"),
               selectInput(ns("delete_sig"), "Select Signature to Delete", choices = NULL, multiple = FALSE),
               actionButton(ns("delete_btn"), "Delete")
      )
    ),
    
    # Data table section below
    br(),
    fluidRow(
      column(
        width = 12,
        DTOutput(ns("signature_tbl")),
        uiOutput(ns("sig_tbl_error_msg")),
        br(),
        downloadButton(ns("download_oms_handler"), "Download OmicSignature", class = "submit-button",
                       onclick = "sig_tbl_select_rows();")
      )
    )
  )
}



signaturesModuleServer <- function(id, conn_handler) {
  moduleServer(id, function(input, output, session) {
    
    # Validate and initialize connection
    validate(need(!is.null(conn_handler), "Connection handler is not set."))
    
    
    # Trigger for manually refreshing
    signature_update_trigger <- reactiveVal(0)
    
    # Load signatures
    signature_db <- reactive({
      signature_update_trigger()
      tryCatch({
        df <- SigRepo::searchSignature(conn_handler = conn_handler)
        validate(need(nrow(df) > 0, "No signatures found."))
        df
      }, error = function(e) {
        showNotification(paste("Error fetching signatures:", e$message), type = "error")
        data.frame()
      })
    })
    
    # Render data table
    output$signature_tbl <- DT::renderDataTable({
      signature_db()
    }, options = list(
      pageLength = 10,
      autoWidth = TRUE
    ))
    
    # Upload Button Logic ####
    
    upload_sig_error_msg <- shiny::reactiveVal()
    
    observeEvent(input$upload_btn_signature, {
      
      upload_sig_error_msg("")
      
      inputfile <- shiny::isolate({ input$upload_file_signature })
      
      if (is.null(inputfile)) {
        upload_sig_error_msg("Please choose an 'OmicSignature' file to import.")
        return(NULL)
      }
      
      omic_signature <- base::readRDS(inputfile$datapath)
      
      if (!inherits(omic_signature, "R6")) {
        upload_sig_error_msg("The uploaded file is not a valid 'OmicSignature' R6 object.")
        return(NULL)
      }
      
      print("conn_handler contents:")
      str(conn_handler)
      
      # for minimal version this works, but I need to have the option for the user to make their signature public or not in the GUI 
      upload_message <- base::tryCatch({
        SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature, return_signature_id = TRUE, visibility = TRUE, verbose = TRUE)
      }, error = function(e) {
        msg <- gsub("\\n|\\t", "<br>", conditionMessage(e), perl = TRUE)
        upload_sig_error_msg(msg)
        message("Upload error: ", conditionMessage(e))
        return(NULL)
      })
      
      print(upload_message)
      
      upload_sig_error_msg(upload_message)
      
      if (!is.null(upload_message) && upload_message != "") {
        signature_update_trigger(signature_update_trigger() + 1)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    output$upload_sig_error_msg <- renderUI({
      req(upload_sig_error_msg())
      shiny::p(class = "error-message", HTML(upload_sig_error_msg()))
    })
    
    
  })
}
