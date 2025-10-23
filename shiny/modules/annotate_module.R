annotate_module_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Annotate",
    value = "annotate",
    
    # Top padding wrapper
    div(style = "padding-top: 70px;",
        
        sidebarLayout(
          sidebarPanel(
            width = 4,
            tabsetPanel(
              id = ns("sidebar_tabs"),
              
              tabPanel(
                "[1] Signature",
                textInput(ns("experiment_label"), tags$b("Experiment Label"), placeholder = "E.g. Knockout Experiment"),
                textInput(ns("signature_label"), tags$b("Signature Label"), placeholder = "E.g. Downregulated Genes"),
                
                selectInput(
                  ns("signature_hypeR"),
                  "Select a signature",
                  choices = '', # placeholder for choices 
                  multiple = FALSE
                ),
                
                actionButton(ns("signature_add"), "Add Signature")
              ),
              
              tabPanel("[2] Genesets",
                       fluidRow(
                         column(4, hypeR::genesets_UI(ns("genesets"))),
                         column(8)
                       )
              ),
              
              tabPanel("[3] Enrichment",
                       numericInput(ns("enrichment_thresh"), "Threshold", 0.05),
                       numericInput(ns("enrichment_bg"), "Background", 36000),
                       actionButton(ns("enrichment_do"), "Do Enrichment")
              )
            )
          ),
          
          mainPanel(
            width = 8,
            
            conditionalPanel(
              condition = sprintf("input['%s'] === '[1] Signature'", ns("sidebar_tabs")),
              DT::dataTableOutput(ns("signature_preview"))
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === '[2] Genesets'", ns("sidebar_tabs")),
              uiOutput(ns("geneset_table"))
            ),
            
            conditionalPanel(
              condition = sprintf("input['%s'] === '[3] Enrichment'", ns("sidebar_tabs")),
              uiOutput(ns("enrichment"))
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
    
    
  # list of signature choices in the database
    observe({
      sigs <- signature_db()
      if (!is.null(sigs) && "signature_name" %in% names(sigs)) {
        updateSelectInput(
          session,
          inputId = "signature_hypeR",
          choices = sigs$signature_name
        )
      }
    })
    

    # 1. Call hypeR's genesets module
    genesets <- hypeR::genesets_Server(ns("genesets"), clean = TRUE)
    
    # 2. Geneset table rendering
    output$geneset_table <- renderUI({
      gsets <- genesets()
      req(length(gsets) > 0)
      
      df <- data.frame(
        Geneset = names(gsets),
        Symbols = sapply(gsets, function(x) paste(head(x, 5), collapse = ","))
      )
      
      tbl <- DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = 20,
          autoWidth = TRUE,
          dom = 'tip',
          class = "compact stripe hover",
          lengthMenu = c(10, 20, 50)
        ),
        class = "display nowrap"
      )
      
      htmltools::div(class = "dt-wrapper", tbl)
    })
    
    # 3. Enrichment logic
    observeEvent(input$enrichment_do, {
      req(input_signature())
      df <- signature_db()
      sig_id <- df$signature_id[df$signature_name == input_signature()][1]
      
      sig_obj <- SigRepo::getSignature(conn_handler = user_conn_handler(), signature_id = sig_id)
      req(!is.null(sig_obj$signature))
      
      genes <- sig_obj$signature
      data <- setNames(list(genes), input_signature())
      gsets <- genesets()
      req(!is.null(gsets))
      
      lmhyp <- lapply(data, function(sig) {
        hypeR::hypeR(
          signature = sig,
          genesets = gsets,
          test = "hypergeometric",
          background = input$enrichment_bg,
          fdr = input$enrichment_thresh,
          plotting = FALSE,
          quiet = TRUE
        )
      })
      
      html <- div(class = "container")
      for (experiment in names(lmhyp)) {
        hyp <- lmhyp[[experiment]]
        div_block <- hypeR::rctbl_build(hyp)
        html <- tagAppendChild(html, tags$h5(glue::glue("{experiment}")))
        html <- tagAppendChild(html, div_block)
        html <- tagAppendChild(html, tags$br())
      }
      
      output$enrichment <- renderUI({
        html
      })
    })
    
    # 4. Render knit HTML report
    output$report_ui <- renderUI({
      rmarkdown::render(
        input = "report.Rmd",
        output_file = "report.html",
        output_dir = tempdir(),
        quiet = TRUE
      )
      
      HTML(paste(readLines(file.path(tempdir(), "report.html")), collapse = "\n"))
    })
  })
}

