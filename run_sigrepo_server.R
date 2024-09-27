
# R packages
library(plumber)

# For loading and installing packages
library(devtools)
load_all()

# Run the app
sigrepo_api <- plumber::plumb("inst/api/sigrepo_api.R")
sigrepo_api$run(host='0.0.0.0', port=38380)