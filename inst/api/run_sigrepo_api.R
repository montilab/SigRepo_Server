
# Load R packages
library(plumber)
  
# Start a Plumber API instance
api <- plumber::plumb("api.R")

# Deploy Plumber API on localhost at port 3838
api$run(host='0.0.0.0', port=3838)
