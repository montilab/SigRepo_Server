# modules/home_module.R
#
# The Home tab, laid out like the React dashboard (web/src/pages/
# DashboardPage.tsx): a thin title bar with one "Browse signatures" button,
# four stat cards, and three chart cards. Styling lives in
# www/assets/css/home_dashboard.css; the numbers and figures come from
# utils/home_utils.R so this file is only layout and wiring.

home_stat_card <- function(label, output_id, icon_name) {
  div(
    class = "stat-card",
    div(
      class = "stat-card-top",
      span(class = "stat-card-label", label),
      span(class = "stat-card-icon", icon(icon_name))
    ),
    div(class = "stat-card-value", textOutput(output_id, inline = TRUE))
  )
}

home_chart_card <- function(title, subtitle, ...) {
  div(
    class = "card dash-chart",
    div(
      class = "card-head",
      div(
        tags$h3(class = "card-title", title),
        p(class = "card-subtitle", subtitle)
      )
    ),
    div(class = "card-body", ...)
  )
}

home_module_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/css/home_dashboard.css"),

    div(
      id = ns("home_page"),
      class = "sr-dash",
      div(
        class = "sr-dash-inner",

        tags$header(
          class = "page-titlebar",
          div(
            class = "page-titlebar-text",
            tags$h1("Welcome to SigRepo"),
            span(class = "page-titlebar-sub", "A snapshot of the repository.")
          ),
          div(
            class = "page-titlebar-actions",
            actionButton(
              ns("go_signatures"),
              label = tagList("Browse signatures", icon("arrow-right")),
              class = "sr-btn sr-btn-primary"
            )
          )
        ),

        div(
          class = "stat-row",
          home_stat_card("Total signatures", ns("stat_signatures"), "dna"),
          home_stat_card("Active users", ns("stat_users"), "users"),
          home_stat_card("Organisms", ns("stat_organisms"), "microscope"),
          home_stat_card("Assay types", ns("stat_assays"), "flask")
        ),

        div(
          class = "dash-charts",
          home_chart_card(
            "By organism", "Distribution across organisms",
            div(class = "chart-fill", plotOutput(ns("organism_plot"), height = "220px")),
            uiOutput(ns("organism_legend"))
          ),
          home_chart_card(
            "By assay", "Counts per assay type",
            div(class = "chart-fill", plotOutput(ns("assay_plot"), height = "300px"))
          ),
          home_chart_card(
            "Top contributors", "Most active by visible signatures",
            div(class = "chart-fill", plotOutput(ns("top_users_plot"), height = "300px"))
          )
        )
      )
    ),

    # navbarPage is fixed-top and wraps to two rows on laptop widths, so pad
    # the page by the navbar's measured height instead of a guessed constant;
    # the stylesheet's padding-top is the fallback before this runs.
    tags$script(HTML("
      (function () {
        function padHomeForNavbar() {
          var nav = document.querySelector('.navbar-fixed-top');
          var page = document.querySelector('.sr-dash');
          if (nav && page) { page.style.paddingTop = (nav.offsetHeight + 16) + 'px'; }
        }
        window.addEventListener('resize', padHomeForNavbar);
        $(document).on('shiny:connected shiny:visualchange', padHomeForNavbar);
        padHomeForNavbar();
      })();
    "))
  )
}

home_module_server <- function(id, signature_db, parent_session) {
  moduleServer(id, function(input, output, session) {

    # signature_db() is an empty frame when the user has no visible
    # signatures, which the helpers turn into zeros and "no data" charts
    # rather than a blank tab.
    summary_counts <- reactive(home_summary_counts(signature_db()))
    by_organism <- reactive(home_count_by(signature_db(), "organism"))
    by_assay <- reactive(home_count_by(signature_db(), "assay_type"))
    top_users <- reactive(home_count_by(signature_db(), "user_name", n = 5))

    output$stat_signatures <- renderText(format(summary_counts()$total_signatures))
    output$stat_users <- renderText(format(summary_counts()$total_users))
    output$stat_organisms <- renderText(format(summary_counts()$total_organisms))
    output$stat_assays <- renderText(format(summary_counts()$total_assays))

    output$organism_plot <- renderPlot(home_donut_plot(by_organism()), bg = "transparent")
    output$organism_legend <- renderUI(home_legend_tags(by_organism()))
    output$assay_plot <- renderPlot(home_bar_plot(by_assay()), bg = "transparent")
    output$top_users_plot <- renderPlot(home_hbar_plot(top_users()), bg = "transparent")

    observeEvent(input$go_signatures, {
      updateNavbarPage(parent_session, "main_navbar", selected = "Signatures")
    })
  })
}
