# Which schema migrations a database has had.
#
# The files in mysql/migrations/ are applied by scripts/migrate.sh, which
# records each one in the schema_migrations table. Two things in the API need
# to agree with that record:
#
#   * generate_db_schema() builds the CURRENT schema from mysql/schema/, so a
#     database it builds already contains every change the migrations make. It
#     records them all as applied; otherwise a fresh instance would look like
#     one that has never been migrated.
#   * At boot the API refuses a database that is missing a migration this code
#     ships. New code against an old schema does not fail at startup by
#     itself; it fails per request, with "Unknown column", on whichever route
#     touches the changed table. That is how the type/platform rename broke
#     /signatures/compare on a database nobody had migrated.

# The forward migrations this code ships, in the order they apply. Rollback
# scripts live in mysql/migrations/rollback/ and are not listed.
shipped_schema_migrations <- function(sigrepo_server_path = base::Sys.getenv("SIGREPO_SERVER_DIR")) {
  files <- base::list.files(
    base::file.path(sigrepo_server_path, "mysql", "migrations"),
    pattern = "\\.sql$",
    recursive = FALSE
  )
  # Same rule as scripts/migrate.sh: a *-rollback.sql file is never a forward
  # migration, wherever it sits.
  base::sort(files[!base::grepl("-rollback\\.sql$", files)])
}

# Mark every shipped migration as applied. Only correct straight after the
# schema has been built from mysql/schema/.
record_schema_migrations <- function(conn, sigrepo_server_path = base::Sys.getenv("SIGREPO_SERVER_DIR")) {
  base::suppressWarnings(DBI::dbGetQuery(conn = conn, statement = "
    CREATE TABLE IF NOT EXISTS `schema_migrations` (
      `name` VARCHAR(255) NOT NULL,
      `applied_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (`name`)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;"))

  shipped <- shipped_schema_migrations(sigrepo_server_path)

  # The names go into SQL text, so hold them to what migrate.sh accepts.
  unexpected <- shipped[!base::grepl("^[A-Za-z0-9._-]+$", shipped)]
  if (base::length(unexpected) > 0) {
    base::stop("Unexpected migration file name: ", base::paste(unexpected, collapse = ", "))
  }

  for (name in shipped) {
    base::suppressWarnings(DBI::dbGetQuery(
      conn = conn,
      statement = base::sprintf("INSERT IGNORE INTO `schema_migrations` (`name`) VALUES ('%s');", name)
    ))
  }

  base::invisible(shipped)
}

# The shipped migrations this database has no record of. A database with no
# signatures table has not been through /init_db yet and is not behind
# anything: the API has to be able to start on it, because /init_db is an API
# route.
pending_schema_migrations <- function(conn, sigrepo_server_path = base::Sys.getenv("SIGREPO_SERVER_DIR")) {
  tables <- DBI::dbGetQuery(
    conn = conn,
    statement = "SELECT TABLE_NAME AS table_name FROM information_schema.TABLES WHERE TABLE_SCHEMA = DATABASE();"
  )$table_name

  if (!("signatures" %in% tables)) {
    return(base::character(0))
  }

  recorded <- base::character(0)
  if ("schema_migrations" %in% tables) {
    recorded <- DBI::dbGetQuery(conn = conn, statement = "SELECT `name` FROM `schema_migrations`;")$name
  }

  base::setdiff(shipped_schema_migrations(sigrepo_server_path), recorded)
}

# Stop unless the database has every shipped migration. `connect` is a
# function returning a connection, so that a database which cannot be reached
# at boot is reported and tolerated: the API has always been able to start
# before MySQL is up, and a schema that cannot be read cannot be judged.
assert_schema_migrated <- function(connect, sigrepo_server_path = base::Sys.getenv("SIGREPO_SERVER_DIR")) {
  pending <- base::tryCatch(
    pending_schema_migrations(connect(), sigrepo_server_path),
    error = function(err) {
      base::message("Could not check schema migrations, continuing: ", base::conditionMessage(err))
      NULL
    }
  )

  if (base::is.null(pending)) {
    return(base::invisible(FALSE))
  }

  if (base::length(pending) > 0) {
    base::stop(base::sprintf(
      base::paste(
        "The database is behind this version of SigRepo: %d schema migration(s) have not been applied (%s).",
        "On an installed instance, run update_sigrepo.sh.",
        "Anywhere else, run scripts/migrate.sh; see mysql/migrations/README.md.",
        "The API will not start until then, because its queries assume the migrated schema."
      ),
      base::length(pending),
      base::paste(pending, collapse = ", ")
    ))
  }

  base::invisible(TRUE)
}
