
# R packages
library(shiny)

# Get package path
shiny_path <- base::file.path(base::Sys.getenv("SIGREPO_SERVER_DIR"), "legacy_app")

# Run the app
shiny::runApp(appDir = shiny_path, host = "0.0.0.0", port = 3838)

