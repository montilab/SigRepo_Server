
# Load R packages
library(plumber)

# The package path
api_path <- "/SigRepo/inst/activate-user-api/api.R"

# Start a Plumber API instance
api <- plumber::plumb(file = api_path)

# Deploy Plumber API on localhost at port 3838
api$run(host = '0.0.0.0', port = 3838)
