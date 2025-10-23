reference_module_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "References",
    value = "references",
    
    div(
      style = "padding-top: 70px;", 
      
      sidebarLayout(
        sidebarPanel(
          width = 4,
          
          tabsetPanel(
            id = ns("sidebar_tabs"),
            type = "tabs",
            
            tabPanel(
              title = "References",
              h4("Reference Features"),
              
              selectInput(
                inputId = ns("ref_organism"),
                label = "Select an organism",
                choices = c("Homo sapiens", "Mus musculus")
              ),
              
              selectInput(
                inputId = ns("ref_assay"),
                label = "Select an assay type",
                choices = c("transcriptomics", "proteomics")
              ),
              
              actionButton(ns("search_ref_btn"), "Search")
            ),
            
            tabPanel(
              title = "Platforms",
              h4("Platform Search"),
              
           
              
              actionButton(ns("search_platform_btn"), "Search Platforms")
            ),
            tabPanel(
              title = "Users",
              h4("Users Search")
              
            )
          )
        ),
        
        mainPanel(
          width = 8,
          DTOutput(ns("ref_feature_tbl"))
        )
      )
    )
  )
  
}

# reference server logic


reference_module_server <- function(id, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    
    ref_features <- eventReactive(input$search_ref_btn, {
      tryCatch({
        # Select the correct function based on assay type
        features <- switch(input$ref_assay,
                           "transcriptomics" = SigRepo::searchTranscriptomicsFeatureSet(conn_handler = user_conn_handler()),
                           "proteomics" = SigRepo::searchProteomicsFeatureSet(conn_handler = user_conn_handler()),
                           NULL)
        
        if (is.null(features)) {
          showNotification("Invalid assay type or no data returned", type = "error")
          return(NULL)
        }
        
        # Filter by organism if available
        if ("organism" %in% colnames(features)) {
          features <- subset(features, organism == input$ref_organism)
        } else {
          showNotification("Warning: no `organism` column found in returned data",
                           type = "warning")
        }
        
        return(features)
        
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
    })
    
    
    
  # table Rendering
    
    output$ref_feature_tbl <- renderDT({
      req(ref_features())
      
      DatatableFX(ref_features(), 
                  hidden_columns = 5)
      
    })
    
    
  })
}

