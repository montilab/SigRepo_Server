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
# Second-level precision, not just the date: two distinct downloads on the
# same calendar day previously got the same gmt_version label. Since upsert
# keys on term_hashkey and prune deletes WHERE gmt_version <> this value, a
# row the first pull wrote that the second pull does not reproduce would
# SURVIVE the prune -- a stale row retained under a current-looking label.
# Deterministic for the same unchanged file (file.mtime() does not change
# between reads), so a genuine re-run over the same download is still
# idempotent -- verified in task-7-fix-report.md.
version <- base::paste0("latest.gmt ", base::format(base::file.mtime(gmt), "%Y-%m-%d %H:%M:%S"))

conn <- db_connect_local()
on.exit(DBI::dbDisconnect(conn), add = TRUE)

result <- build_rummagene_catalog(conn, gmt_path = gmt, gmt_version = version)

base::cat("\nexamined :", result$examined, "\n")
base::cat("unparsed :", result$unparsed, "\n")
base::cat("qualified:", result$qualified, "\n")
for (r in base::names(result$rejected)) {
  base::cat(base::sprintf("  rejected %-16s %d\n", r, result$rejected[[r]]))
}
