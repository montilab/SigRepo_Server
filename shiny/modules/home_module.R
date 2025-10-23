# R/mod_home.R

home_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(style = "margin-top: 15px;",
    tags$head(tags$style(HTML("
      .homepage-title {
        text-align: center;
        font-size: 36px;
        font-weight: bold;
        margin-top: 30px;
      }
      .homepage-subtitle {
        text-align: center;
        font-size: 20px;
        color: #666;
        margin-bottom: 30px;
      }
      .homepage-section {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .homepage-icon {
        font-size: 40px;
        color: #007bff;
        margin-bottom: 10px;
      }
    "))),
    
    fluidRow(
      column(
        width = 4,
        div(
          class = "homepage-section text-intro",
          HTML(
            "
    <div style='padding: 10px; line-height: 1.6; font-size: 16px; height: 450px;'>
      <p>
        <strong>Welcome to the Signature Repository (SigRepo)!</strong>
      </p>
      <p>
        The Signature Repository is a collaborative platform designed for storing and managing biological signatures and their associated data.
      </p>
      <p>
        This R Shiny application provides a user-friendly interface to:
      </p>
      <ul>
        <li>Browse, upload, and search for signatures</li>
        <li>Manage access and permissions</li>
        <li>Perform gene set enrichment and annotation</li>
      </ul>
      <p>
        You can explore both public signatures and the ones you've contributed.
      </p>
    </div>
  "
          )
        )
      ),
      column(
        width = 8,
        div(
          class = "homepage-section",
          h4("Signature Overview"),
          tabsetPanel(
            tabPanel("By Organism", plotOutput(ns("organism_plot"), height = "450px")),
            tabPanel("By Assay", plotOutput(ns("assay_plot"), height = "450px")),
            tabPanel("Top Users", plotOutput(ns("top_users_plot"), height = "450px"))
          )
        )
      )
    ),
    
    fluidRow(
      column(
        width = 4,
        div(
          class = "homepage-section text-center",
          span(class = "homepage-icon", icon("dna")),
          h4("Signatures"),
          p("Browse, filter, and manage gene expression signatures."),
          actionButton(ns("go_signatures"), "Go to Signatures", class = "btn-primary")
        )
      ),
      column(
        width = 4,
        div(
          class = "homepage-section text-center",
          span(class = "homepage-icon", icon("layer-group")),
          h4("Collections"),
          p("Explore curated collections of signatures."),
          actionButton(ns("go_collections"), "Go to Collections", class = "btn-primary")
        )
      ),
      column(
        width = 4,
        div(
          class = "homepage-section text-center",
          span(class = "homepage-icon", icon("upload")),
          h4("R-Client"),
          p("View our R-Client Documentation to use SigRepo in R."),
          actionButton(ns("go_upload"), "Github Docs", class = "btn-success")
        )
      )
    ),
    
    br(),
    
    fluidRow(
      column(
        12,
        div(
          class = "text-center text-muted",
          
          # Add logo image
          img(
            src = "images/LC_logo.png",
            style = "height: 30px; vertical-align: middle; margin-right: 10px;"
          ),
          
          # Footer text
          span("Created by SigRepo Team · Version 1.0 · © 2025")
        )
      )
    )
    
    )
  )
}

home_module_server <- function(id, signature_db) {
  moduleServer(id, function(input, output, session) {
    
    # Top users plot
    output$top_users_plot <- renderPlot({
      df <- signature_db()
      req(nrow(df) > 0)
      
      user_counts <- df %>%
        dplyr::count(user_name, name = "num_signatures") %>%
        dplyr::arrange(desc(num_signatures)) %>%
        dplyr::slice_head(n = 10)
      
      ggplot(user_counts, aes(x = reorder(user_name, num_signatures), y = num_signatures)) +
        geom_bar(stat = "identity", fill = "#2c7fb8") +
        coord_flip() +
        labs(title = "Top 10 Most Active Users", x = "User", y = "Number of Signatures") +
        theme_minimal()
    })
    
    # Organism plot
    output$organism_plot <- renderPlot({
      df <- signature_db()
      req(nrow(df) > 0, "organism" %in% names(df))
      
      ggplot(df, aes(x = organism, fill = organism)) +
        geom_bar() +
        labs(x = "Organism", y = "Signature Count") +
        theme_minimal() +
        theme(legend.position = "right")
    })
    
    # Assay type plot
    output$assay_plot <- renderPlot({
      df <- signature_db()
      req(nrow(df) > 0, "assay_type" %in% names(df))
      
      ggplot(df, aes(x = assay_type, fill = assay_type)) +
        geom_bar() +
        labs(x = "Assay Type", y = "Signature Count") +
        theme_minimal() +
        theme(legend.position = "right")
    })
    
    # Redirect buttons
    observeEvent(input$go_signatures, {
      updateTabsetPanel(parent_session, "main_navbar", selected = "signatures")
    })

    observeEvent(input$go_collections, {
      updateTabsetPanel(parent_session, "main_navbar", selected = "collections")
    })
  })
}
