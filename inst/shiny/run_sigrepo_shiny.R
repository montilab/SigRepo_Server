
# R packages
library(shiny)

# Load packages
library(devtools)

# Load sigrepo package
load_all()

# Get sigrepo package path
app_path <- system.file("shiny", package = "SigRepo")

# Run the app
shiny::runApp(file.path(app_path, "app.R"), host='0.0.0.0', port=3838)