# Shiny entry point.
#
# app_src/bootstrap.R attaches the packages, loads the SigRepo, OmicSignature
# and hypeR packages and sources every module, modal and helper into this
# environment. This file only finds the app directory, so bootstrap.R knows
# where to source from, and wires ui and server together.

shiny_path <- if (file.exists(file.path(getwd(), "app_src", "bootstrap.R"))) {
  normalizePath(getwd())
} else if (file.exists(file.path(getwd(), "legacy_app", "app_src", "bootstrap.R"))) {
  normalizePath(file.path(getwd(), "legacy_app"))
} else if (nzchar(Sys.getenv("SIGREPO_SERVER_DIR"))) {
  file.path(normalizePath(Sys.getenv("SIGREPO_SERVER_DIR")), "legacy_app")
} else {
  stop("Cannot resolve Shiny app path. Set SIGREPO_SERVER_DIR or run from the repo/app directory.")
}

options(sigrepo.shiny_path = shiny_path)

app_env <- environment()
sys.source(file.path(shiny_path, "app_src", "bootstrap.R"), envir = app_env)
sys.source(file.path(shiny_path, "app_src", "app_ui.R"), envir = app_env)
sys.source(file.path(shiny_path, "app_src", "app_server.R"), envir = app_env)

shiny::shinyApp(ui, server)
