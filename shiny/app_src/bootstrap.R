# app for modules

# loading in packages

#installing sigrepo_client and then validating it is the correct branch
#devtools::install_github("montilab/SigRepo")
#packageDescription("SigRepo")$GithubRef



# R packages for building shiny dashboard
library(shinyjs)
library(shiny)
library(DT)

# Package for knitting PDF
library(rmarkdown)

# Packages for API
library(httr)
library(jsonlite)

# Package for data cleaning, extraction and manipulation
library(dplyr)

# Package for plotting
library(ggplot2)

# For loading and installing packages
library(devtools)

# Load SigRepo package
devtools::load_all(base::Sys.getenv("SIGREPO_DIR"))

# Loading OmicSignature package
devtools::load_all(base::Sys.getenv("OMICSIG_DIR"))

# # Loading hypeR package
devtools::load_all(base::Sys.getenv("HYPER_DIR"))

# Package for parallel processes
library(promises)
library(future)
future::plan(multisession)


shiny_path <- if (nzchar(getOption("sigrepo.shiny_path", ""))) {
  normalizePath(getOption("sigrepo.shiny_path"))
} else if (file.exists(file.path(getwd(), "app_src", "bootstrap.R"))) {
  normalizePath(getwd())
} else if (file.exists(file.path(getwd(), "shiny", "app_src", "bootstrap.R"))) {
  normalizePath(file.path(getwd(), "shiny"))
} else {
  env_root <- base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "")
  if (nzchar(env_root)) {
    file.path(normalizePath(env_root), "shiny")
  } else {
    stop("Cannot resolve Shiny app path in bootstrap.")
  }
}

# If the resolved directory is the repo root, shift to the shiny app directory.
if (!file.exists(file.path(shiny_path, "modules", "home_module.R")) &&
    file.exists(file.path(shiny_path, "shiny", "modules", "home_module.R"))) {
  shiny_path <- normalizePath(file.path(shiny_path, "shiny"))
}

bootstrap_env <- .GlobalEnv

# sourcing modules
sys.source(file.path(shiny_path, "modules", "home_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "signature_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "collection_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "annotate_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "compare_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "reference_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "resource_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "feedback_module.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modules", "hypeR_module.R"), envir = bootstrap_env)

# sourcing modals
sys.source(file.path(shiny_path, "modals", "manage_users_modal.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modals", "delete_modal.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modals", "upload_modal.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "modals", "view_modal.R"), envir = bootstrap_env)

# testing module
sys.source(file.path(shiny_path, "modules", "test_module.R"), envir = bootstrap_env)

# utils
sys.source(file.path(shiny_path, "utils", "utils.R"), envir = bootstrap_env)
sys.source(file.path(shiny_path, "utils", "validateUser.R"), envir = bootstrap_env)

# default connection handler for root, DONT USE IN MAIN APP

conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"),
  host = base::Sys.getenv("DB_HOST"),
  port = base::as.integer(Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD"),
  api_host = base::Sys.getenv("API_HOST")
)
