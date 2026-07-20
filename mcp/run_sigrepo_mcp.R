
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

## Create a database handler (used by api/lib/auth.R's require_api_key, and
## by SigRepo:: functions like getSignature()/runHypeR() that round-trip
## through the REST API for the difexp table -- not just DB access).
## newConnHandler()'s own api_host/api_port defaults point at production;
## that's only correct if this MCP server is *also* running against
## production. API_LOCAL_HOST/API_LOCAL_PORT (falling back to plain
## API_HOST/API_PORT, matching this repo's existing .Renviron convention)
## override it for anywhere else -- a local dev stack, a test stack --
## mirroring how DB_LOCAL_HOST already overrides the production DB default.
## Left unset, behavior is unchanged from before: newConnHandler()'s own
## default applies, exactly as it did for a real production deployment.
conn_handler_args <- base::list(
  dbname = base::Sys.getenv("DB_NAME"),
  host = base::Sys.getenv("DB_LOCAL_HOST"),
  port = base::as.integer(base::Sys.getenv("DB_PORT")),
  user = base::Sys.getenv("DB_USER"),
  password = base::Sys.getenv("DB_PASSWORD")
)

api_host_override <- base::Sys.getenv("API_LOCAL_HOST", unset = base::Sys.getenv("API_HOST", unset = ""))
api_port_override <- base::Sys.getenv("API_LOCAL_PORT", unset = base::Sys.getenv("API_PORT", unset = ""))

if (base::nzchar(api_host_override)) {
  conn_handler_args$api_host <- api_host_override
}
if (base::nzchar(api_port_override)) {
  conn_handler_args$api_port <- base::as.integer(api_port_override)
}

conn_handler <- base::do.call(SigRepo::newConnHandler, conn_handler_args)

# Get sigrepo server path
sigrepo_server_path <- base::Sys.getenv("SIGREPO_SERVER_DIR")

# Reuse the same auth/DB/signature/MSigDB logic the Plumber API is built
# on, plus the new MCP-specific query and tool-definition files.
for (lib_file in c(
  base::file.path(sigrepo_server_path, "api", "lib", "common.R"),
  base::file.path(sigrepo_server_path, "api", "lib", "auth.R"),
  base::file.path(sigrepo_server_path, "api", "lib", "signature.R"),
  base::file.path(sigrepo_server_path, "api", "lib", "msigdb_genesets_admin.R"),
  base::file.path(sigrepo_server_path, "mcp", "lib", "queries.R"),
  base::file.path(sigrepo_server_path, "mcp", "lib", "tools.R")
)) {
  base::source(lib_file, local = TRUE)
}

# Resolved once at boot, same as api.R -- run_enrichment()'s msigdb_collection
# path (mcp/lib/queries.R) reads/writes this same cache directory via
# ensure_msigdb_geneset_resource(), so an agent-triggered fetch here and one
# triggered through POST /geneset_resources/ensure land in the same place.
msigdb_cache_dir <- default_msigdb_cache_dir(sigrepo_server_path)

mcptools::mcp_server(
  tools = build_mcp_tools(),
  type = "http",
  host = "0.0.0.0",
  port = base::as.integer(base::Sys.getenv("MCP_PORT", "8021")),
  session_tools = FALSE
)
