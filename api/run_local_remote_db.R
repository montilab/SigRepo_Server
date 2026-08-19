# Local launcher for the SigRepo Plumber API, pointed at a REMOTE MySQL.
#
# Lets the React dev app (Vite proxy -> :3838) show real repository data without
# standing up a local MySQL/Docker stack. Read-oriented UI use only.
#
# No credentials live in this file. Put them in an untracked .Renviron.local
# beside it (see .gitignore), for example:
#
#   DB_NAME=sigrepo
#   DB_LOCAL_HOST=your.db.host
#   DB_PORT=3306
#   DB_USER=your_user
#   DB_PASSWORD=your_password
#   ADMIN_KEY=...
#   SENDMAIL_KEY=...
#   SIGREPO_SERVER_DIR=/path/to/SigRepo_Server
#   SIGREPO_DIR=/path/to/SigRepo
#   DIFEXP_DIR=/tmp/sigrepo-local-difexp
#
# then run:  Rscript api/run_local_remote_db.R

env_file <- file.path(dirname(getwd()), ".Renviron.local")
if (!file.exists(env_file)) {
  env_file <- ".Renviron.local"
}
if (file.exists(env_file)) {
  readRenviron(env_file)
} else {
  message(
    "No .Renviron.local found. Falling back to the environment already set; ",
    "DB_LOCAL_HOST/DB_USER/DB_PASSWORD must be exported for this to connect."
  )
}

# Defaults for the non-secret settings only.
if (!nzchar(Sys.getenv("DB_NAME"))) Sys.setenv(DB_NAME = "sigrepo")
if (!nzchar(Sys.getenv("DB_PORT"))) Sys.setenv(DB_PORT = "3306")
if (!nzchar(Sys.getenv("DIFEXP_DIR"))) Sys.setenv(DIFEXP_DIR = "/tmp/sigrepo-local-difexp")
if (!nzchar(Sys.getenv("SIGREPO_SERVER_DIR"))) Sys.setenv(SIGREPO_SERVER_DIR = getwd())

required <- c("DB_LOCAL_HOST", "DB_USER", "DB_PASSWORD")
missing <- required[!nzchar(Sys.getenv(required))]
if (length(missing) > 0) {
  stop(
    "Missing required environment variable(s): ", paste(missing, collapse = ", "),
    ". Set them in .Renviron.local (see the header of this file)."
  )
}

dir.create(Sys.getenv("DIFEXP_DIR"), showWarnings = FALSE, recursive = TRUE)

library(plumber)
api_path <- file.path(Sys.getenv("SIGREPO_SERVER_DIR"), "api")
api <- plumber::plumb(file = file.path(api_path, "api.R"))
api$run(host = "127.0.0.1", port = 3838)
