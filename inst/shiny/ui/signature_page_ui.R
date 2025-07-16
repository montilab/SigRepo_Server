# Signature Management UI Panel
 tabPanel(
  "Signatures",
  
  useShinyjs(),  # Make sure shinyjs is included in the main app only once
  
  h4("Signature Management"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        # ---- Upload Tab ----
        tabPanel("Upload",
                 h4("Upload Signature"),
                 fileInput("upload_file_signature", "Choose a file to upload"),
                 actionButton("upload_btn_signature", "Upload")
        ),
        
        # ---- Filter Tab ----
        tabPanel("Filter",
                 h4("Filter Signatures"),
                 selectizeInput(
                   inputId = "search_options_signature",
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
                   selectizeInput("signature_name", "signature_name:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("organism", "organism:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("phenotype", "phenotype:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("sample_type", "sample_type:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("platform_id", "platform:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("assay_type", "assay_type:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 br(),
                 actionButton("search_signature", "Search", class = "submit-button"),
                 actionButton("clear_filters", "Clear Filters"),
                 br(), br(),
                 uiOutput("search_sig_error_msg")
        ),
        
        # ---- Update Tab ----
        tabPanel("Update",
                 h4("Update a Signature"),
                 selectInput("update_sig", "Select Signature to Update", choices = "", multiple = FALSE, width = "100%"),
                 fileInput("update_sig_file", "Choose a file to update"),
                 actionButton("update_btn", "Update"),
                 uiOutput("update_sig_error_msg")
        ),
        
        # ---- Delete Tab ----
        tabPanel("Delete",
                 h4("Delete Signature"),
                 selectInput("delete_sig", "Select Signature to Delete", choices = NULL , multiple = FALSE, width = "100%"),
                 actionButton("delete_btn", "Delete")
        )
      )
    ),
    
    mainPanel(
      DTOutput("signature_tbl"),
      uiOutput("sig_tbl_error_msg"),  
      br(),
      div(
        downloadButton("download_oms_handler", "Download OmicSignature", class = "submit-button", 
                       onclick = "sig_tbl_select_rows();")
      )
      
    )
    
  )
)
