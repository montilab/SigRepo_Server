
# For MCP
library(mcptools)
library(ellmer)

# For DB connection
library(RMySQL)
library(DBI)

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

## Create a database handler (used by api/lib/auth.R's require_api_key)
conn_handler <- SigRepo::newConnHandler(
  dbname = base::Sys.getenv("DB_NAME"),
  host = base::Sys.getenv("DB_LOCAL_HOST"),
  port = base::as.integer(base::Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD")
)

# Get sigrepo server path
sigrepo_server_path <- base::Sys.getenv("SIGREPO_SERVER_DIR")

# Reuse the same auth/DB/signature logic the Plumber API is built on, plus
# the new MCP-specific query and tool-definition files.
for (lib_file in c(
  base::file.path(sigrepo_server_path, "api", "lib", "common.R"),
  base::file.path(sigrepo_server_path, "api", "lib", "auth.R"),
  base::file.path(sigrepo_server_path, "api", "lib", "signature.R"),
  base::file.path(sigrepo_server_path, "mcp", "lib", "queries.R"),
  base::file.path(sigrepo_server_path, "mcp", "lib", "tools.R")
)) {
  base::source(lib_file, local = TRUE)
}

mcptools::mcp_server(
  tools = build_mcp_tools(),
  type = "http",
  host = "0.0.0.0",
  port = base::as.integer(base::Sys.getenv("MCP_PORT", "8021")),
  session_tools = FALSE
)
