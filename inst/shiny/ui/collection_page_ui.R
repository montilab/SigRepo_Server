# Signature Collection Management UI Panel
tabPanel(
  "Collections",
  
  useShinyjs(),  # Only call this once in the main UI if it's already included
  
  h4("Signature Collection Management"),
  
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        id = "main_tabs_collection",
        type = "tabs",
        
        # ---- Upload Tab ----
        tabPanel("Upload",
                 h4("Upload Signature Collection"),
                 fileInput("upload_file_collection", "Choose a file to upload"),
                 actionButton("upload_btn_collection", "Upload")
        ),
        
        # ---- Filter Tab ----
        tabPanel("Filter",
                 h4("Filter Collections"),
                 selectizeInput(
                   inputId = "search_options_collection",
                   label = "Search Options:",
                   choices = c(
                     "Choose from a list of options below" = "", 
                     "collection_name", 
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
                   selectizeInput("collection_name", "Collection Name:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("organism", "Organism:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("phenotype", "Phenotype:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("sample_type", "Sample Type:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("platform_id", "Platform:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 shinyjs::hidden(
                   selectizeInput("assay_type", "Assay Type:", choices = "", multiple = TRUE, width = "100%")
                 ),
                 br(),
                 actionButton("search_collection", "Search", class = "submit-button"),
                 actionButton("clear_filters_collection", "Clear Filters"),
                 br(), br(),
                 uiOutput("collection_sig_error_msg")
        ),
        
        # ---- Update Tab ----
        tabPanel("Update",
                 h4("Update a Signature Collection"),
                 selectInput("update_collection", "Select Collection to Update", choices = "", width = "100%"),
                 fileInput("update_collection_file", "Choose a file to update"),
                 actionButton("update_btn_collection", "Update"),
                 uiOutput("update_collection_error_msg")
        ),
        
        # ---- Delete Tab ----
        tabPanel("Delete",
                 h4("Delete Signature Collection"),
                 selectInput("delete_collection", "Select Collection to Delete", choices = NULL, width = "100%"),
                 actionButton("delete_btn_collection", "Delete")
        )
      )
    ),
    
    mainPanel(
      DTOutput("collection_tbl"),
      uiOutput("collection_sig_tbl_error_msg"),
      br(),
      
      # Only show these when on the Filter tab
      conditionalPanel(
        condition = "input.main_tabs_collection == 'Filter'",
        shinyjs::hidden(
          actionButton("download_oms", "Download Omic Signature Collection", class = "submit-button", onclick = "sig_tbl_select_rows();")
        ),
        div(
          downloadButton("download_oms_handler", "Download")
        ),
        DTOutput("sig_up_regulated_tbl"),
        DTOutput("sig_down_regulated_tbl")
      )
    )
  )
)
