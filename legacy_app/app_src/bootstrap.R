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

# Load SigRepo package
load_repo_package("SIGREPO_DIR", "SigRepo")

# Loading OmicSignature package
load_repo_package("OMICSIG_DIR", "OmicSignature")

# # Loading hypeR package
load_repo_package("HYPER_DIR", "hypeR")

hypgem_dir <- base::Sys.getenv("HYPERGEM_DIR", unset = "")
if (!nzchar(hypgem_dir)) {
  sibling_hypgem <- normalizePath(
    file.path(dirname(normalizePath(getwd())), "hypeR-GEM"),
    mustWork = FALSE
  )
  if (dir.exists(sibling_hypgem)) {
    hypgem_dir <- sibling_hypgem
  }
}

options(sigrepo.hypergem_available = FALSE)
options(sigrepo.hypergem_error = NULL)

if (nzchar(hypgem_dir) && dir.exists(hypgem_dir)) {
  hypgem_loaded <- tryCatch({
    if (requireNamespace("pkgload", quietly = TRUE)) {
      pkgload::load_all(path = hypgem_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
    } else if (requireNamespace("devtools", quietly = TRUE)) {
      devtools::load_all(path = hypgem_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
    } else if (requireNamespace("hypeR.GEM", quietly = TRUE)) {
      base::library("hypeR.GEM", character.only = TRUE)
    } else {
      base::stop("Neither pkgload/devtools nor an installed hypeR.GEM package were available.")
    }
    TRUE
  }, error = function(e) {
    options(sigrepo.hypergem_error = conditionMessage(e))
    FALSE
  })

  options(sigrepo.hypergem_available = hypgem_loaded)
}

# Package for parallel processes
library(promises)
library(future)
future::plan(multisession)


shiny_path <- if (nzchar(getOption("sigrepo.shiny_path", ""))) {
  normalizePath(getOption("sigrepo.shiny_path"))
} else if (file.exists(file.path(getwd(), "app_src", "bootstrap.R"))) {
  normalizePath(getwd())
} else if (file.exists(file.path(getwd(), "legacy_app", "app_src", "bootstrap.R"))) {
  normalizePath(file.path(getwd(), "legacy_app"))
} else {
  env_root <- base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "")
  if (nzchar(env_root)) {
    file.path(normalizePath(env_root), "legacy_app")
  } else {
    stop("Cannot resolve Shiny app path in bootstrap.")
  }
}

# If the resolved directory is the repo root, shift to the shiny app directory.
if (!file.exists(file.path(shiny_path, "modules", "home_module.R")) &&
    file.exists(file.path(shiny_path, "legacy_app", "modules", "home_module.R"))) {
  shiny_path <- normalizePath(file.path(shiny_path, "legacy_app"))
}

options(sigrepo.shiny_path = shiny_path)

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
