# Rebuild the local development repository from scratch: schema, two dev
# accounts, and real signature data imported from a downloaded basket.
#
# Run inside the API container:
#   docker exec sigrepo-local-api Rscript /SigRepo_Server/.local-data/rebuild_local_db.R
#
# Safe to re-run: it drops and recreates everything.
suppressMessages(library(DBI)); suppressMessages(library(digest))
con <- dbConnect(RMySQL::MySQL(), host = Sys.getenv("DB_LOCAL_HOST"), port = as.integer(Sys.getenv("DB_PORT")),
                 user = Sys.getenv("DB_USER"), password = Sys.getenv("DB_PASSWORD"), dbname = "sigrepo")
on.exit(suppressWarnings(dbDisconnect(con)), add = TRUE)
ex <- function(sql) invisible(dbExecute(con, sql))
sq <- function(x) paste0("'", gsub("'", "''", x), "'")
hash <- function(x) substr(digest(x, algo = "md5"), 1, 32)

# --- schema -----------------------------------------------------------------
ex("SET FOREIGN_KEY_CHECKS=0")
for (t in dbListTables(con)) ex(sprintf("DROP TABLE IF EXISTS `%s`", t))
# Each schema file holds exactly one CREATE TABLE. Strip the leading comment
# block and the trailing semicolon and run it whole -- splitting on ";" breaks
# on the CHECK constraints, which contain none.
for (f in sort(list.files("/SigRepo_Server/mysql/schema", pattern = "[.]sql$", full.names = TRUE))) {
  lines <- readLines(f, warn = FALSE)
  lines <- lines[!grepl("^\\s*--", lines)]
  stmt <- trimws(paste(lines, collapse = "\n"))
  stmt <- sub(";\\s*$", "", stmt)
  if (!nzchar(stmt)) next
  tryCatch(ex(stmt), error = function(e) cat("  schema warn:", basename(f), "-", substr(conditionMessage(e), 1, 80), "\n"))
}
ex("SET FOREIGN_KEY_CHECKS=1")
cat("schema:", length(dbListTables(con)), "tables\n")

# --- the dev admin account --------------------------------------------------
# Local only. Deliberately a fixed, non-secret key so the stack is reproducible
# and nothing here resembles a production credential. SigRepo authenticates a
# login by opening a MySQL connection as the user, so the matching MySQL
# account is created too -- without it the portal rejects this user even though
# the row exists.
ex(sprintf(
  "INSERT INTO users (user_name, user_password_hashkey, user_email, user_first, user_last, user_role, api_key, date_created, active, user_hashkey)
   VALUES ('devadmin', %s, 'devadmin@example.com', 'Dev', 'Admin', 'admin', 'devadmin_local_key_000000000000', NOW(), 1, 'usr_devadmin_local_000000000000')",
  sq(hash("devadmin"))))
# A second, non-admin account. Two accounts are the minimum needed to test
# anything that is supposed to be per-user (the signature basket) or
# role-gated (delete, visibility), which a single admin cannot exercise.
ex(sprintf(
  "INSERT INTO users (user_name, user_password_hashkey, user_email, user_first, user_last, user_role, api_key, date_created, active, user_hashkey)
   VALUES ('deveditor', %s, 'deveditor@example.com', 'Dev', 'Editor', 'editor', 'deveditor_local_key_00000000000', NOW(), 1, 'usr_deveditor_local_00000000000')",
  sq(hash("deveditor"))))

for (u in c("devadmin", "deveditor")) {
  for (host in c("%", "localhost")) {
    tryCatch({
      ex(sprintf("CREATE USER IF NOT EXISTS '%s'@'%s' IDENTIFIED BY '%s'", u, host, u))
      ex(sprintf("GRANT ALL PRIVILEGES ON sigrepo.* TO '%s'@'%s'", u, host))
    }, error = function(e) cat("  grant warn:", substr(conditionMessage(e), 1, 80), "\n"))
  }
}
ex("FLUSH PRIVILEGES")

# --- signature data ---------------------------------------------------------
# Real signatures, from a basket downloaded out of the production repository,
# rather than invented ones: .local-data/import_basket.R seeds the vocabulary
# and reference features from the export itself. Point BASKET_DIR at the
# unpacked download.
#
#   docker cp ~/Documents/GitHub/signature_basket_20260826 sigrepo-local-api:/tmp/basket
#   docker exec sigrepo-local-api Rscript /SigRepo_Server/.local-data/rebuild_local_db.R
basket_dir <- Sys.getenv("BASKET_DIR", unset = "/tmp/basket")
if (dir.exists(basket_dir) && length(list.files(basket_dir, pattern = "[.]rds$")) > 0) {
  cat("\nimporting signatures from", basket_dir, "\n")
  # Run as a separate process rather than source()d. Sourcing at top level
  # registers the importer's on.exit(dbDisconnect(...)) on THIS frame, which
  # closes the connection above mid-run -- "corrupt connection handle".
  suppressWarnings(dbDisconnect(con))
  status <- system2("Rscript", c("/SigRepo_Server/.local-data/import_basket.R", shQuote(basket_dir)))
  if (status != 0) stop("import_basket.R failed with status ", status)
  quit(save = "no", status = 0)
} else {
  cat("\nno signature basket at", basket_dir, "-- schema and accounts only.\n",
      "Copy a downloaded basket in and re-run, or run import_basket.R directly.\n")
}
