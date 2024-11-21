# Build according to a specified version of R
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.4.0}

############# Build Stage: CaDrA ##################

# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/shiny-verse:${R_VERSION} AS base

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install system libraries of general use
RUN apt-get update --allow-releaseinfo-change --fix-missing \
  && apt-get -y --no-install-recommends install \
  librsvg2-dev \
  libudunits2-dev \
  libv8-dev \
  libsodium-dev \
  libbz2-dev \
  liblzma-dev \
  tcl8.6-dev \
  tk8.6-dev \
  git \
  dos2unix \
  vim \
  && apt clean autoclean \
  && apt autoremove --yes \
	&& rm -rf /var/lib/{apt,dpkg,cache,log}/

# Create package directory 
ENV PACKAGE_DIR=/SigRepo 

# Make package as working directory
WORKDIR ${PACKAGE_DIR}

# Copy package code to Docker image
COPY . ${PACKAGE_DIR}

# Copy DESCRIPTION to Docker image
COPY DESCRIPTION ${PACKAGE_DIR}/DESCRIPTION

# Copy script to install r packages to Docker image
COPY install_r_packages.R ${PACKAGE_DIR}/install_r_packages.R

# Load CaDrA package and install CaDrA.shiny dependencies
RUN Rscript "${PACKAGE_DIR}/install_r_packages.R"

# Make Shiny App/Plumber API available at port 3838
EXPOSE 3838

# Copy bash script that starts shiny-server to Docker image
COPY inst/shiny/shiny-server.sh ${PACKAGE_DIR}/inst/shiny/shiny-server.sh

# Convert bash script from Windows style line endings to Unix-like control characters
RUN dos2unix ${PACKAGE_DIR}/inst/shiny/shiny-server.sh

# Allow permissions to execute the bash script
RUN chmod a+x ${PACKAGE_DIR}/inst/shiny/shiny-server.sh

# Execute the bash script to launch shiny app or plumber api
CMD ["/bin/bash", "-c", "${PACKAGE_DIR}/inst/shiny/shiny-server.sh"]


