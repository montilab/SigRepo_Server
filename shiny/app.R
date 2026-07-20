# Thin app entrypoint
env_root <- base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "")

shiny_path <- if (file.exists(file.path(getwd(), "app_src", "bootstrap.R"))) {
  normalizePath(getwd())
} else if (file.exists(file.path(getwd(), "shiny", "app_src", "bootstrap.R"))) {
  normalizePath(file.path(getwd(), "shiny"))
} else if (nzchar(env_root)) {
  file.path(normalizePath(env_root), "shiny")
} else {
  stop("Cannot resolve Shiny app path. Set SIGREPO_SERVER_DIR or run from the repo/app directory.")
}

options(sigrepo.shiny_path = shiny_path)
app_env <- environment()
sys.source(file.path(shiny_path, "app_src", "bootstrap.R"), envir = app_env)

source_app_file <- function(...) {
  sys.source(file.path(shiny_path, ...), envir = app_env)
}

# Ensure app-local symbols are available in the same environment as ui/server.
source_app_file("modules", "home_module.R")
source_app_file("modules", "signature_module.R")
source_app_file("modules", "collection_module.R")
source_app_file("modules", "annotate_module.R")
source_app_file("modules", "compare_module.R")
source_app_file("modules", "reference_module.R")
source_app_file("modules", "resource_module.R")
source_app_file("modules", "feedback_module.R")
source_app_file("modules", "hypeR_module.R")
source_app_file("modules", "test_module.R")
source_app_file("modals", "manage_users_modal.R")
source_app_file("modals", "delete_modal.R")
source_app_file("modals", "upload_modal.R")
source_app_file("modals", "view_modal.R")
source_app_file("utils", "utils.R")
source_app_file("utils", "validateUser.R")

sys.source(file.path(shiny_path, "app_src", "app_ui.R"), envir = app_env)
sys.source(file.path(shiny_path, "app_src", "app_server.R"), envir = app_env)

shiny::shinyApp(ui, server)
