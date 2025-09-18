
# Load R packages
library(plumber)

# Get package path
api_path <- base::file.path(base::Sys.getenv("SIGREPO_SERVER_DIR"), "api")

# Start a Plumber API instance
api <- plumber::plumb(file = base::file.path(api_path, "api.R"))

# Deploy Plumber API on localhost at port 3838
api$run(host = "0.0.0.0", port = 3838)


