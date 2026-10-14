# Loads everything the Shiny app needs, once.
#
# app.R sets options(sigrepo.shiny_path) and sources this file into its own
# environment. Everything sourced here lands in that same environment, so ui
# and server find the modules, modals and helpers without a second pass.

# Shiny itself, plus DT for every table in the app.
library(shinyjs)
library(shiny)
library(DT)

# httr and jsonlite post feedback to Slack; dplyr and ggplot2 are used across
# the modules.
library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Attach a package from a checkout named by an environment variable when
# pkgload or devtools can load it, otherwise from the installed package.
load_repo_package <- function(repo_dir, package_name, required = TRUE) {
  repo_dir <- base::Sys.getenv(repo_dir, unset = repo_dir)

  if (base::nzchar(repo_dir) && base::dir.exists(repo_dir)) {
    if (requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(path = repo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
      return(invisible(TRUE))
    }

    if (requireNamespace("devtools", quietly = TRUE)) {
      devtools::load_all(path = repo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
      return(invisible(TRUE))
    }
  }

  if (requireNamespace(package_name, quietly = TRUE)) {
    base::library(package_name, character.only = TRUE)
    return(invisible(TRUE))
  }

  if (!required) {
    return(invisible(FALSE))
  }

  base::stop(
    base::sprintf(
      "Cannot load package '%s'. Checked repo path '%s' and installed packages, but neither pkgload/devtools nor the installed package were available.",
      package_name,
      repo_dir
    )
  )
}

load_repo_package("SIGREPO_DIR", "SigRepo")
load_repo_package("OMICSIG_DIR", "OmicSignature")
load_repo_package("HYPER_DIR", "hypeR")

shiny_path <- base::getOption("sigrepo.shiny_path", "")
if (!base::nzchar(shiny_path)) {
  base::stop("bootstrap.R expects options(sigrepo.shiny_path) to be set; run the app through app.R.")
}
shiny_path <- base::normalizePath(shiny_path)

bootstrap_env <- environment()

source_app_file <- function(...) {
  sys.source(file.path(shiny_path, ...), envir = bootstrap_env)
}

# The on-disk MSigDB cache helpers are shared with the API
# (api/lib/msigdb_cache.R); the Annotate tab loads genesets with them.
server_root <- base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "")
if (!base::nzchar(server_root)) {
  server_root <- base::normalizePath(file.path(shiny_path, ".."), mustWork = FALSE)
}
sys.source(file.path(server_root, "api", "lib", "msigdb_cache.R"), envir = bootstrap_env)

# Registration, password reset and the mail-service call are the API's
# (api/lib/user.R); the sign-in flows call them so the form and the routes
# behave the same.
sys.source(file.path(server_root, "api", "lib", "user.R"), envir = bootstrap_env)

# Helpers first, then the modals the modules open, then the modules.
source_app_file("utils", "utils.R")
source_app_file("utils", "signature_utils.R")
source_app_file("utils", "home_utils.R")
source_app_file("utils", "compare_utils.R")
source_app_file("utils", "annotate_utils.R")
source_app_file("utils", "auth_utils.R")

source_app_file("modals", "manage_users_modal.R")
source_app_file("modals", "delete_modal.R")
source_app_file("modals", "upload_modal.R")

source_app_file("modules", "auth_module.R")
source_app_file("modules", "home_module.R")
source_app_file("modules", "signature_module.R")
source_app_file("modules", "collection_module.R")
source_app_file("modules", "annotate_module.R")
source_app_file("modules", "compare_module.R")
source_app_file("modules", "reference_module.R")
source_app_file("modules", "feedback_module.R")

# The server's own connection, used only to look users up before and during
# sign-in. Every other call goes through the per-user handler a login creates.
conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"),
  host = base::Sys.getenv("DB_HOST"),
  port = base::as.integer(Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD"),
  api_host = base::Sys.getenv("API_HOST")
)

# Without API_HOST the per-user handlers fall back to the SigRepo client's own
# default address, so every API call from this app would leave this
# deployment. The installer and compose files set it; say so if it is missing.
if (!base::nzchar(base::Sys.getenv("API_HOST"))) {
  base::message("API_HOST is not set: API calls from the Shiny app will go to the SigRepo client's default address. Set API_HOST (and API_PORT) in the app's .Renviron.")
}
