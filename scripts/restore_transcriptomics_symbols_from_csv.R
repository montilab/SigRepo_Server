#!/usr/bin/env Rscript
# Fill EMPTY transcriptomics_features.gene_symbol values for one organism from
# the reference CSV shipped in mysql/data/. Interim repair for the symbols a
# /update_transcriptomics run erased (the client fetched `hgnc_symbol`, which
# is empty for mouse, and wrote the blanks over the symbols /init_db had
# loaded). It never touches a row that already has a symbol.
#
# Run inside the API container, where .Renviron points at the right database:
#   docker exec -w /SigRepo_Server sigrepo-api \
#     Rscript scripts/restore_transcriptomics_symbols_from_csv.R --organism "Mus musculus"          # dry run
#   docker exec -w /SigRepo_Server sigrepo-api \
#     Rscript scripts/restore_transcriptomics_symbols_from_csv.R --organism "Mus musculus" --apply  # write
suppressMessages(library(DBI))

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(flag, default = NULL) {
  i <- match(flag, args)
  if (is.na(i) || i == length(args)) default else args[i + 1]
}
organism <- arg_value("--organism", "Mus musculus")
apply_changes <- "--apply" %in% args
csv_by_organism <- c("Homo sapiens" = "Transcriptomics_Homo_Sapiens.csv", "Mus musculus" = "Transcriptomics_Mus_Musculus.csv")
csv_name <- csv_by_organism[organism]
if (is.na(csv_name)) stop("No shipped reference CSV for organism '", organism, "'. Known: ", paste(names(csv_by_organism), collapse = ", "))
server_dir <- Sys.getenv("SIGREPO_SERVER_DIR", unset = ".")
csv_path <- file.path(server_dir, "mysql", "data", csv_name)
if (!file.exists(csv_path)) stop("Reference CSV not found: ", csv_path)

con <- DBI::dbConnect(RMySQL::MySQL(), host = Sys.getenv("DB_LOCAL_HOST"), port = as.integer(Sys.getenv("DB_PORT")),
                      user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASSWORD"), dbname = Sys.getenv("DB_NAME"))
on.exit(suppressWarnings(DBI::dbDisconnect(con)), add = TRUE)

organism_id <- DBI::dbGetQuery(con, sprintf("SELECT organism_id FROM organisms WHERE organism = %s", DBI::dbQuoteString(con, organism)))$organism_id
if (length(organism_id) != 1) stop("Organism '", organism, "' not found in the organisms table.")

ref <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
ref <- ref[!is.na(ref$gene_symbol) & nzchar(trimws(ref$gene_symbol)), c("feature_name", "gene_symbol")]
ref <- ref[!duplicated(tolower(ref$feature_name)), ]

stored <- DBI::dbGetQuery(con, sprintf(
  "SELECT feature_id, feature_name, gene_symbol FROM transcriptomics_features WHERE organism_id = %d", organism_id))
blank <- is.na(stored$gene_symbol) | !nzchar(trimws(stored$gene_symbol))
hit <- match(tolower(stored$feature_name), tolower(ref$feature_name))
to_fill <- stored[blank & !is.na(hit), c("feature_id", "feature_name")]
to_fill$gene_symbol <- ref$gene_symbol[hit[blank & !is.na(hit)]]

cat(sprintf("%s (organism_id %d): %d stored features, %d without a symbol, %d of those covered by %s\n",
            organism, organism_id, nrow(stored), sum(blank), nrow(to_fill), csv_name))
cat(sprintf("%d rows lack a symbol and are not in the CSV; they are left alone.\n", sum(blank) - nrow(to_fill)))
if (nrow(to_fill) > 0) print(utils::head(to_fill, 5), row.names = FALSE)

if (!apply_changes) {
  cat("Dry run: nothing written. Re-run with --apply to fill these symbols.\n")
  quit(save = "no", status = 0)
}

invisible(DBI::dbExecute(con, "START TRANSACTION"))
updated <- 0L
for (chunk in split(to_fill, ceiling(seq_len(nrow(to_fill)) / 1000))) {
  # One UPDATE per chunk via CASE, restricted to rows that are still blank.
  cases <- paste(sprintf("WHEN %d THEN %s", chunk$feature_id, DBI::dbQuoteString(con, chunk$gene_symbol)), collapse = " ")
  ids <- paste(chunk$feature_id, collapse = ",")
  updated <- updated + DBI::dbExecute(con, sprintf(
    "UPDATE transcriptomics_features SET gene_symbol = CASE feature_id %s END
     WHERE organism_id = %d AND feature_id IN (%s) AND (gene_symbol IS NULL OR gene_symbol = '')", cases, organism_id, ids))
}
invisible(DBI::dbExecute(con, "COMMIT"))
cat(sprintf("Applied: %d rows updated.\n", updated))
