# The page shell: the sign-in card, then the navbar and its tabs. Every tab's
# UI lives in modules/; the look lives in www/assets/css/sigrepo.css.

ui <- shiny::fluidPage(

  shinyjs::useShinyjs(),

  title = "SigRepo - Signature Repository",

  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "assets/css/sigrepo.css"),
    tags$script(src = "assets/js/app.js", type = "text/javascript"),
    # The navbar is fixed-top and wraps to two rows on narrow windows, so each
    # tab's page is padded by the navbar's measured height rather than the
    # stylesheet's guess. shiny:idle follows every flush, which covers the
    # navbar first appearing after sign-in.
    tags$script(HTML("
      (function () {
        function padPagesForNavbar() {
          var nav = document.querySelector('.navbar-fixed-top');
          if (!nav || nav.offsetHeight === 0) { return; }
          var pad = (nav.offsetHeight + 16) + 'px';
          document.querySelectorAll('.sr-page').forEach(function (page) { page.style.paddingTop = pad; });
        }
        window.addEventListener('resize', padPagesForNavbar);
        $(document).on('shiny:connected shiny:idle', padPagesForNavbar);
      })();
    "))
  ),

  auth_signin_ui(),

  shiny::div(
    id = "content-wrapper",
    style = "display:none;",

    navbarPage(
      title = tagList(
        tags$img(src = "images/logo.png", alt = "SigRepo"),
        div(
          class = "navbar-user",
          actionLink(
            inputId = "edit_profile",
            label = NULL,
            icon = icon("user-circle"),
            title = "Your profile"
          ),
          uiOutput(outputId = "welcome_msg", inline = TRUE, class = "navbar-user-name"),
          actionLink(
            inputId = "log_out_btn",
            icon = tags$i(class = "fa fa-sign-out"),
            label = "Log out"
          )
        ),
        # The Longevity Consortium logo, pinned to the navbar's right edge.
        tags$img(
          src = "images/LC_logo.png",
          alt = "Longevity Consortium",
          class = "navbar-custom-logo"
        )
      ),
      id = "main_navbar",
      position = "fixed-top",

      tabPanel("Home", home_module_ui("home")),
      tabPanel("Signatures", signature_module_ui("signatures")),
      tabPanel("Collections", collection_module_ui("collections")),
      tabPanel("Annotate", annotate_module_ui("annotate")),
      tabPanel("Compare", compare_module_ui("compare")),
      tabPanel("Browsing", reference_module_ui("references")),
      tabPanel("Feedback", feedbackUI("feedback"))
    )
  )
)
