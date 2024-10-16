
# R packages
library(shiny)
library(devtools)
load_all()

# Run the app
shiny::runApp("app.R", host='0.0.0.0', port=38380)