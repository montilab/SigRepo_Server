#!/usr/bin/env Rscript
# Build the Rummagene catalog. Intended to run weekly, and NOT on the droplet --
# the GMT is ~700MB and the NCBI pass takes about 20 minutes.
#
#   Rscript scripts/build_rummagene_catalog.R /path/to/latest.gmt
#
# With no argument the GMT is downloaded to a temporary file first.
base::setwd(base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "/SigRepo_Server"))
for (f in base::sort(base::list.files("api/lib", pattern = "[.]R$", full.names = TRUE))) {
  base::source(f)
}

args <- base::commandArgs(trailingOnly = TRUE)
gmt <- if (base::length(args) > 0) args[1] else download_rummagene_gmt(base::tempfile(fileext = ".gmt"))
version <- base::paste0("latest.gmt ", base::format(base::file.mtime(gmt), "%Y-%m-%d"))

conn <- db_connect_local()
on.exit(DBI::dbDisconnect(conn), add = TRUE)

result <- build_rummagene_catalog(conn, gmt_path = gmt, gmt_version = version)

base::cat("\nexamined :", result$examined, "\n")
base::cat("qualified:", result$qualified, "\n")
for (r in base::names(result$rejected)) {
  base::cat(base::sprintf("  rejected %-16s %d\n", r, result$rejected[[r]]))
}
