
############# Build Stage ##################

# Get shiny+tidyverse+devtools packages from rocker image
FROM montilab/sigrepo:latest
	
# Create package directory 
ENV PACKAGE_DIR=/SigRepo 

# Make package as working directory
WORKDIR ${PACKAGE_DIR}

# Install dependencies for sendmailR 
RUN R -e "install.packages('sendmailR', dependencies=TRUE, repos='http://cran.rstudio.com/')"

# Make Shiny App or Plumber API available at port 3838
EXPOSE 3838



