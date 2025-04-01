# Build according to a specified version of R
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.4.0}

############# Build Stage: CaDrA ##################

# Get shiny+tidyverse+devtools packages from rocker image
FROM rocker/shiny-verse:${R_VERSION} AS base

# local apt mirror support
# start every stage with updated apt sources
ARG APT_MIRROR_NAME=
RUN if [ -n "$APT_MIRROR_NAME" ]; then sed -i.bak -E '/security/! s^https?://.+?/(debian|ubuntu)^http://'"$APT_MIRROR_NAME"'/\1^' /etc/apt/sources.list && grep '^deb' /etc/apt/sources.list; fi

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
  curl \
  && apt clean autoclean \
  && apt autoremove --yes \
	&& rm -rf /var/lib/{apt,dpkg,cache,log}/
	
# Create a difexp directory 
ENV DIFEXP_DIR=/difexp

# Set up a volume to mount difexp data
VOLUME ${DIFEXP_DIR}	

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

# Install package dependencies 
RUN Rscript "${PACKAGE_DIR}/install_r_packages.R"

# Install dependencies for OmicSignature 
RUN R -e "BiocManager::install('limma')"

# Install OmicSignature 
RUN R -e "devtools::install_github(repo = 'montilab/OmicSignature', dependencies=TRUE)"

# Make Shiny App or Plumber API available at port 3838
EXPOSE 3838

# Copy bash script that starts shiny-server to Docker image
COPY inst/shiny/shiny-server.sh ${PACKAGE_DIR}/inst/shiny/shiny-server.sh

# Convert bash script from Windows style line endings to Unix-like control characters
RUN dos2unix ${PACKAGE_DIR}/inst/shiny/shiny-server.sh

# Allow permissions to execute the bash script
RUN chmod a+x ${PACKAGE_DIR}/inst/shiny/shiny-server.sh

# Copy bash script that starts shiny-server to Docker image
COPY inst/api/shiny-server.sh ${PACKAGE_DIR}/inst/api/shiny-server.sh

# Convert bash script from Windows style line endings to Unix-like control characters
RUN dos2unix ${PACKAGE_DIR}/inst/api/shiny-server.sh

# Allow permissions to execute the bash script
RUN chmod a+x ${PACKAGE_DIR}/inst/api/shiny-server.sh


