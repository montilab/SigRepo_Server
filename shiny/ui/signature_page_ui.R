


signaturesUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),  # Make sure this is called ONCE in the main UI (not needed here if already used globally)
    
    
    sidebarLayout(
      sidebarPanel(
        width = 4,
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
                   shinyjs::hidden(
                     selectizeInput(ns("signature_name"), "signature_name:", choices = "", multiple = TRUE, width = "100%")
                   ),
                   shinyjs::hidden(
                     selectizeInput(ns("organism"), "organism:", choices = "", multiple = TRUE, width = "100%")
                   ),
                   shinyjs::hidden(
                     selectizeInput(ns("phenotype"), "phenotype:", choices = "", multiple = TRUE, width = "100%")
                   ),
                   shinyjs::hidden(
                     selectizeInput(ns("sample_type"), "sample_type:", choices = "", multiple = TRUE, width = "100%")
                   ),
                   shinyjs::hidden(
                     selectizeInput(ns("platform_id"), "platform:", choices = "", multiple = TRUE, width = "100%")
                   ),
                   shinyjs::hidden(
                     selectizeInput(ns("assay_type"), "assay_type:", choices = "", multiple = TRUE, width = "100%")
                   ),
                   br(),
                   actionButton(ns("search_signature"), "Search", class = "submit-button"),
                   actionButton(ns("clear_filters"), "Clear Filters"),
                   br(), br(),
                   uiOutput(ns("search_sig_error_msg"))
          ),
          
          # ---- Update Tab ----
          tabPanel("Update",
                   h4("Update a Signature"),
                   selectInput(ns("update_sig"), "Select Signature to Update", choices = "", multiple = FALSE, width = "100%"),
                   fileInput(ns("update_sig_file"), "Choose a file to update"),
                   actionButton(ns("update_btn"), "Update"),
                   uiOutput(ns("update_sig_error_msg"))
          ),
          
          # ---- Delete Tab ----
          tabPanel("Delete",
                   h4("Delete Signature"),
                   selectInput(ns("delete_sig"), "Select Signature to Delete", choices = NULL , multiple = FALSE, width = "100%"),
                   actionButton(ns("delete_btn"), "Delete")
          )
        )
      ),
      
      mainPanel(
        DTOutput(ns("signature_tbl")),
        uiOutput(ns("sig_tbl_error_msg")),  
        br(),
        div(
          downloadButton(ns("download_oms_handler"), "Download OmicSignature", class = "submit-button", 
                         onclick = "sig_tbl_select_rows();")
        )
      )
    )
  )
}
