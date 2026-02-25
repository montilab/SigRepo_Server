annotate_module_ui <- function(id) {
  ns <- NS(id)
  
  div(
    style = "padding-top: 70px;",
      
    tabsetPanel(
      id = ns("annotate_tabs"),
      type = "tabs",
        
        ## =========================
        ## TAB 1: Test Parameters
        ## =========================
        tabPanel(
          title = "Test Parameters",
          
          ## ---- General parameters ----
          fluidRow(
            column(
              width = 6,
              
              textInput(
                ns("experiment_label"),
                "Experiment Label",
                placeholder = "E.g. Knockout Experiment"
              ),
              
              shiny::radioButtons(
                ns("enrichment_type"),
                "Enrichment Type:",
                choices = c(
                  "Hypergeometric" = "hypergeo",
                  "Kstest"         = "kstest",
                  "Gsea"           = "gsea"
                ),
                inline = TRUE
              ),
              
              helpText(
                "Signature format depends on enrichment type: Hypergeometric uses a list of features, KStest uses ranked features, and GSEA requires ranked features with numeric weights."
              )
            ),
            
            column(
              width = 6,
              numericInput(ns("enrichment_thresh"), "Threshold", 0.05),
              numericInput(ns("enrichment_bg"), "Background", 36000)
            )
          ),
          
          hr(),
          
          ## ---- Nested tabs: Signature / Genesets ----
          tabsetPanel(
            id = ns("parameter_tabs"),
            
            ## --- Signatures tab ---
            tabPanel(
              title = "Signatures",
              
              h4("[1] Signatures"),
              
              DT::DTOutput(ns("signature_hypeR")),
              
              actionButton(
                ns("signature_add"),
                "Add Signature(s)"
              ),
              uiOutput(ns("signature_feedback"))
            ),
            
            ## --- Genesets tab ---
            tabPanel(
              title = "Genesets",
              
              h4("[2] Genesets"),
              selectInput(
                ns("species"),
                "Species",
                choices = msigdbr::msigdbr_species()$species_name,
                selected = "Homo Sapiens"
              ),
              genesets_hypeR_UI(ns("genesets")),
              
            
            ),
            tabPanel(
              title = "Preview",
              actionButton(
                ns("enrichment_do"),
                "Run hypeR"
              ),
              uiOutput(ns("signature_preview")) 
            )
          ),
          
        
          
         
        ),
        
        ## =========================
        ## TAB 2: Experiment Results
        ## =========================
        tabPanel(
          title = "Experiment Results",
          
          fluidRow(
            column(
              width = 12,
              
              actionButton(
                ns("generate_report"),
                "HTML Report"
              ),
              actionButton(
                ns("export_hyp"),
                "Export"
              ),
                
                  uiOutput(ns("enrichment")),
                  plotOutput("dotplot", height = "400px", width = "100%")
                  
                
              
            )
          )
        )
    )
  )
}



#annotate server logic


annotate_module_server <- function(id, signature_db, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # selected signatures
    active_signatures <- reactiveVal(list())
    
    
  # list of signature choices in the database
    output$signature_hypeR <- renderDT({
      df <- signature_db()
      
      # util function for datatable
      
      DatatableFX(
        df = df,
        hidden_columns = c(0, 6, 7, 8, 9, 11,12, 14, 15, 16,18, 19,21,22, 24, 25, 26),
        scrollY = "300px",
        row_selection = "multiple"
     
        
      )
    }, server = TRUE)
    
    observeEvent(input$signature_add, {
      # Get selected rows from the DT
      selected_rows <- input$signature_hypeR_rows_selected
      req(selected_rows)
      
      df <- signature_db()
      req(!is.null(df))
      
      sig_rows <- df[selected_rows, , drop = FALSE]
      
      current <- active_signatures()
      
      for (i in seq_len(nrow(sig_rows))) {
        sig_row <- sig_rows[i, ]
        key <- sig_row$signature_name
        if (!key %in% names(current)) {
          current[[key]] <- list(
            experiment      = input$experiment_label,
            signature_name  = sig_row$signature_name,
            signature_id    = sig_row$signature_id
          )
        }
      }
      
      active_signatures(current)
      # Feedback message
      output$signature_feedback <- renderUI({
        tags$div(
          style = "color: green; font-weight: bold; margin-top: 5px;",
          paste(length(sig_rows), "signature(s) added successfully!")
        )
      })
    })
    
    
    output$signature_preview <- renderUI({
      
      sig_list <- active_signatures()
      
      if (length(sig_list) == 0) {
        return(tags$em("No signatures added yet."))
      }
      
      # Extract selected signature names
      sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
      
      # Filter original signature_db
      df <- signature_db()
      req(df)
      
      selected_df <- df[df$signature_name %in% sig_names, , drop = FALSE]
      
      gs_names <- names(genesets())
      
      tagList(
        tags$h5(paste("Experiment:", input$experiment_label)),
        tags$h5(paste("Enrichment Type:", input$enrichment_type)),
        tags$h6(paste("Enrichment Threshold:", input$enrichment_thresh)),
        tags$h6(paste("Background:", input$enrichment_bg)),
        tags$hr(),
        
        tags$h5("Selected Signatures:"),
        DT::datatable(
          selected_df,
          options = list(
            scrollX = TRUE,
            columnDefs = list(
              list(visible = FALSE, targets = 0)  # hide first column
            )
          ),
          class = "nowrap",  # <- prevent wrapping
          escape = FALSE
        ) %>%
          DT::formatStyle(
            columns = names(selected_df),
            `white-space` = "nowrap"
          )
        ,
        
        tags$hr(),
        
        tags$h5("Selected Genesets:"),
        if (length(gs_names) > 0) {
          tags$ul(lapply(gs_names, tags$li))
        } else {
          tags$em("No genesets selected yet.")
        }
      )
    })
    
    
    
    
 # genesets logic 
    
 genesets <- genesets_hypeR_Server(
   id = "genesets",
   species = reactive(input$species),
   clean = TRUE
 )
  
   
    
 observeEvent(input$enrichment_do, {
   sig_list <- active_signatures()
   req(length(sig_list) > 0)
   
   gsets <- genesets()
   req(length(gsets) > 0)
   
   # Collect all selected signature IDs
   sig_ids <- sapply(sig_list, function(sig) sig$signature_id)
   
   # Fetch all signatures at once
   sig_objs <- SigRepo::getSignature(
     conn_handler = user_conn_handler(),
     signature_id = sig_ids
   )
   
   # sig_objs is a named list where names are signature_name
   # Each element is a vector of genes (e.g., difexp$symbol)
   
   signature_vectors <- lapply(sig_objs, function(x) {
     # Skip if x$signature is NULL
     if (is.null(x$signature)) return(NULL)
     
     # Convert symbols to character and remove NAs
     symbols <- as.character(x$difexp$symbol)
     symbols <- symbols[!is.na(symbols)]
     
     # Skip empty vectors
     if (length(symbols) == 0) return(NULL)
     
     symbols
   })
   
   # Remove any NULL entries
   signature_vectors <- signature_vectors[!sapply(signature_vectors, is.null)]
   
   if (length(signature_vectors) == 0) {
     showNotification("No valid signatures to run enrichment", type = "error")
     return()
   }
   
   # Run hypeR
   hyp <- hypeR::hypeR(
     signature  = signature_vectors,  # named list of multiple signatures
     genesets   = gsets,
     test       = input$enrichment_type,
     background = input$enrichment_bg,
     fdr        = input$enrichment_thresh,
     plotting   = FALSE,
     quiet      = TRUE
   )
   print(hyp)
   
  
   # generate html report
   
   observeEvent(input$generate_report, {
     
     
     
   })
   
   # Render results table
   
   output$dotplot <- renderPlot({
     hypeR::hyp_dots(hyp, merge = TRUE, fdr = input$enrichment_thresh, title = input$experiment_label)
   })
   output$enrichment <- renderUI({
     tagList(
       tags$h5("Enrichment Results"),
       hypeR::rctbl_build(hyp)
     )
   })
   
   
   
  

   
 })
 
 
    
  
  })
}
