# R/mod_home.R

home_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("home_page"))

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " {
        padding-top: 28px;
        padding-bottom: 32px;
      }

      ", page_selector, " .home-hero {
        margin-bottom: 18px;
        padding: 26px 30px;
        border-radius: 16px;
        background: linear-gradient(135deg, #153b59 0%, #28658d 100%);
        color: #ffffff;
        box-shadow: 0 12px 28px rgba(21, 59, 89, 0.18);
      }

      ", page_selector, " .home-hero h1 {
        margin: 0 0 10px 0;
        font-size: 36px;
        font-weight: 700;
      }

      ", page_selector, " .home-hero p {
        margin: 0;
        max-width: 760px;
        color: rgba(255, 255, 255, 0.9);
        font-size: 16px;
        line-height: 1.6;
      }

      ", page_selector, " .home-hero-actions {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        margin-top: 18px;
      }

      ", page_selector, " .home-card {
        margin-bottom: 18px;
        padding: 20px 22px;
        border: 1px solid #d9e3ec;
        border-radius: 12px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }

      ", page_selector, " .home-card h3,
      ", page_selector, " .home-card h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .home-stat-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
      }

      ", page_selector, " .home-stat-card {
        padding: 16px 18px;
        border-radius: 12px;
        background: #f6f9fc;
        border: 1px solid #e1ebf2;
      }

      ", page_selector, " .home-stat-label {
        display: block;
        margin-bottom: 6px;
        color: #58708a;
        font-size: 12px;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .home-stat-value {
        color: #17324d;
        font-size: 28px;
        font-weight: 700;
        line-height: 1.1;
      }

      ", page_selector, " .home-section-copy {
        margin-bottom: 14px;
        color: #5a728a;
      }

      ", page_selector, " .home-action-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 14px;
      }

      ", page_selector, " .home-action-card {
        padding: 18px;
        border-radius: 12px;
        border: 1px solid #dbe5ee;
        background: linear-gradient(180deg, #ffffff 0%, #f7fafc 100%);
      }

      ", page_selector, " .home-action-icon {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 52px;
        height: 52px;
        margin-bottom: 14px;
        border-radius: 14px;
        background: #e7f1f9;
        color: #1c5d87;
        font-size: 22px;
      }

      ", page_selector, " .home-action-card h4 {
        margin-bottom: 8px;
      }

      ", page_selector, " .home-action-card p {
        min-height: 54px;
        color: #5a728a;
      }

      ", page_selector, " .home-footer {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 8px;
        color: #6b7f92;
        font-size: 13px;
      }
    "))),

    div(
      id = ns("home_page"),

      div(
        class = "home-hero",
        tags$h1("Welcome to SigRepo"),
        tags$p(
          "SigRepo is a collaborative signature repository for browsing, organizing, and annotating biological signatures. Use the workspace below to move quickly between repository exploration, collection management, and downstream enrichment analysis."
        ),
        div(
          class = "home-hero-actions",
          actionButton(ns("go_signatures"), "Browse Signatures", class = "btn-primary"),
          actionButton(ns("go_collections"), "Explore Collections", class = "btn-default"),
          tags$a(
            class = "btn btn-default",
            href = "https://github.com/montilab/SigRepo",
            target = "_blank",
            rel = "noopener noreferrer",
            "R Client Docs"
          )
        )
      ),

      div(
        class = "home-card",
        tags$h3("Repository Snapshot"),
        p(
          class = "home-section-copy",
          "A quick view of the current repository footprint based on the signatures available to your account."
        ),
        uiOutput(ns("home_stats"))
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "home-card",
            tags$h3("By Organism"),
            p(class = "home-section-copy", "Distribution of signatures across supported organisms."),
            plotOutput(ns("organism_plot"), height = "320px")
          )
        ),
        column(
          width = 4,
          div(
            class = "home-card",
            tags$h3("By Assay"),
            p(class = "home-section-copy", "Breakdown of signatures by assay type."),
            plotOutput(ns("assay_plot"), height = "320px")
          )
        ),
        column(
          width = 4,
          div(
            class = "home-card",
            tags$h3("Top Contributors"),
            p(class = "home-section-copy", "Most active users based on visible signatures."),
            plotOutput(ns("top_users_plot"), height = "320px")
          )
        )
      ),

      div(
        class = "home-card",
        tags$h3("Quick Actions"),
        p(
          class = "home-section-copy",
          "Jump directly into the areas of the platform you are most likely to use next."
        ),
        div(
          class = "home-action-grid",
          div(
            class = "home-action-card",
            span(class = "home-action-icon", icon("dna")),
            tags$h4("Signatures"),
            tags$p("Browse, inspect, download, and manage repository signatures."),
            actionButton(ns("go_signatures_secondary"), "Open Signatures", class = "btn-primary")
          ),
          div(
            class = "home-action-card",
            span(class = "home-action-icon", icon("layer-group")),
            tags$h4("Collections"),
            tags$p("Review grouped signatures and manage reusable collection sets."),
            actionButton(ns("go_collections_secondary"), "Open Collections", class = "btn-primary")
          ),
          div(
            class = "home-action-card",
            span(class = "home-action-icon", icon("flask")),
            tags$h4("Annotate"),
            tags$p("Launch enrichment and annotation workflows using repository signatures."),
            actionButton(ns("go_annotate"), "Open Annotate", class = "btn-primary")
          )
        )
      ),

      div(
        class = "home-footer",
        img(
          src = "images/LC_logo.png",
          style = "height: 30px; vertical-align: middle;"
        ),
        span("Created by the SigRepo Team"),
        span("·"),
        span("Version 1.0"),
        span("·"),
        span("© 2025")
      )
    )
  )
}

home_module_server <- function(id, signature_db, parent_session) {
  moduleServer(id, function(input, output, session) {

    signature_summary <- reactive({
      df <- signature_db()
      req(nrow(df) > 0)

      list(
        total_signatures = nrow(df),
        total_users = dplyr::n_distinct(df$user_name),
        total_organisms = if ("organism" %in% names(df)) dplyr::n_distinct(df$organism) else 0,
        total_assays = if ("assay_type" %in% names(df)) dplyr::n_distinct(df$assay_type) else 0
      )
    })

    plot_theme <- function() {
      ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(face = "bold", size = 13, color = "#17324d"),
          axis.title = ggplot2::element_text(color = "#4f657b"),
          axis.text = ggplot2::element_text(color = "#3f556b"),
          panel.grid.minor = ggplot2::element_blank(),
          legend.position = "none"
        )
    }

    output$home_stats <- renderUI({
      stats <- signature_summary()

      div(
        class = "home-stat-grid",
        div(
          class = "home-stat-card",
          tags$span(class = "home-stat-label", "Total Signatures"),
          tags$span(class = "home-stat-value", stats$total_signatures)
        ),
        div(
          class = "home-stat-card",
          tags$span(class = "home-stat-label", "Active Users"),
          tags$span(class = "home-stat-value", stats$total_users)
        ),
        div(
          class = "home-stat-card",
          tags$span(class = "home-stat-label", "Organisms"),
          tags$span(class = "home-stat-value", stats$total_organisms)
        ),
        div(
          class = "home-stat-card",
          tags$span(class = "home-stat-label", "Assay Types"),
          tags$span(class = "home-stat-value", stats$total_assays)
        )
      )
    })

    output$top_users_plot <- renderPlot({
      df <- signature_db()
      req(nrow(df) > 0)

      user_counts <- df %>%
        dplyr::count(user_name, name = "num_signatures") %>%
        dplyr::arrange(desc(num_signatures)) %>%
        dplyr::slice_head(n = 10)

      ggplot(user_counts, aes(x = reorder(user_name, num_signatures), y = num_signatures)) +
        geom_col(fill = "#2d6f8f", width = 0.75) +
        coord_flip() +
        labs(x = "User", y = "Signatures") +
        plot_theme()
    })

    output$organism_plot <- renderPlot({
      df <- signature_db()
      req(nrow(df) > 0, "organism" %in% names(df))

      ggplot(df, aes(x = organism, fill = organism)) +
        geom_bar(width = 0.72) +
        labs(x = "Organism", y = "Signatures") +
        scale_fill_brewer(palette = "Blues") +
        plot_theme() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    output$assay_plot <- renderPlot({
      df <- signature_db()
      req(nrow(df) > 0, "assay_type" %in% names(df))

      ggplot(df, aes(x = assay_type, fill = assay_type)) +
        geom_bar(width = 0.72) +
        labs(x = "Assay Type", y = "Signatures") +
        scale_fill_brewer(palette = "PuBu") +
        plot_theme() +
        theme(axis.text.x = element_text(angle = 20, hjust = 1))
    })

    observeEvent(input$go_signatures, {
      updateNavbarPage(parent_session, "main_navbar", selected = "Signatures")
    })

    observeEvent(input$go_signatures_secondary, {
      updateNavbarPage(parent_session, "main_navbar", selected = "Signatures")
    })

    observeEvent(input$go_collections, {
      updateNavbarPage(parent_session, "main_navbar", selected = "Collections")
    })

    observeEvent(input$go_collections_secondary, {
      updateNavbarPage(parent_session, "main_navbar", selected = "Collections")
    })

    observeEvent(input$go_annotate, {
      updateNavbarPage(parent_session, "main_navbar", selected = "Annotate")
    })
  })
}
