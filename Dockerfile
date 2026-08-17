# Build according to a specified version of R
ARG R_VERSION
ARG R_VERSION=${R_VERSION:-4.4.3}

############# Build Stage ##################

# Use a multi-arch Rocker base.
FROM rocker/r-ver:${R_VERSION} AS base

# Build according to a specified version of R
ARG SIGREPO_BRANCH
ENV SIGREPO_BRANCH=${SIGREPO_BRANCH:-master}

# local apt mirror support
# start every stage with updated apt sources
ARG APT_MIRROR_NAME=
RUN if [ -n "$APT_MIRROR_NAME" ]; then sed -i.bak -E '/security/! s^https?://.+?/(debian|ubuntu)^http://'"$APT_MIRROR_NAME"'/\1^' /etc/apt/sources.list && grep '^deb' /etc/apt/sources.list; fi

# Define a system argument
ARG DEBIAN_FRONTEND=noninteractive

# Install system libraries of general use
RUN apt-get update --allow-releaseinfo-change --fix-missing \
  && apt-get -y --no-install-recommends install \
  build-essential \
  pkg-config \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  default-libmysqlclient-dev \
  librsvg2-dev \
  libudunits2-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libv8-dev \
  libsodium-dev \
  libbz2-dev \
  liblzma-dev \
  tcl8.6-dev \
  tk8.6-dev \
  libabsl-dev \
  ca-certificates \
  cmake \
  git \
  vim \
  curl \
  dos2unix \
  && apt clean autoclean \
  && apt autoremove --yes \
	&& rm -rf /var/lib/{apt,dpkg,cache,log}/

# Create a difexp directory 
ENV DIFEXP_DIR=/difexp

# Set up a volume to mount difexp data
VOLUME ${DIFEXP_DIR}
	
# Set working directory to install OmicSignature
WORKDIR / 

# Create package directory 
ENV OMICSIG_DIR=/OmicSignature

# Clone OmicSignature repo
RUN git clone https://github.com/montilab/OmicSignature.git

# Create package directory 
ENV HYPER_DIR=/hypeR

# Clone hypeR repo
RUN git clone https://github.com/montilab/hypeR.git

# Create package directory 
ENV SIGREPO_DIR=/SigRepo 

# Clone SigRepo repo
RUN git clone https://github.com/montilab/SigRepo.git

# Create server directory 
ENV SIGREPO_SERVER_DIR=/SigRepo_Server 

# Make package as working directory
WORKDIR ${SIGREPO_SERVER_DIR}

# Copy package code to Docker image
COPY . ${SIGREPO_SERVER_DIR}

# Copy DESCRIPTION to Docker image
COPY DESCRIPTION ${SIGREPO_SERVER_DIR}/DESCRIPTION

# Copy script to install r packages to Docker image
COPY install_r_packages.R ${SIGREPO_SERVER_DIR}/install_r_packages.R

# Install package dependencies 
RUN Rscript "${SIGREPO_SERVER_DIR}/install_r_packages.R"

# Install dependencies for OmicSignature 
RUN R -e "BiocManager::install('limma')"

# Install OmicSignature 
RUN R -e "remotes::install_github(repo = 'montilab/OmicSignature', dependencies = c('Depends','Imports','LinkingTo'))"

# Install dependencies for OmicSignature 
RUN R -e "BiocManager::install('biomaRt')"

# Install SigRepo 
RUN R -e "branch <- base::Sys.getenv('SIGREPO_BRANCH'); remotes::install_github(repo = 'montilab/SigRepo', ref = branch, dependencies = c('Depends','Imports','LinkingTo'))"

# Install hypeR 
RUN R -e "remotes::install_github(repo = 'montilab/hypeR', dependencies = c('Depends','Imports','LinkingTo'))"

# Expose app at port 3838
EXPOSE 3838

# Expose the MCP server's port (internal to the Docker network by default --
# see docker-compose.yml, no `ports:` mapping is published for this service)
ENV MCP_PORT=8021
EXPOSE 8021


# Convert bash script from Windows style line endings to Unix-like control characters

# Allow permissions to execute the bash script

# Copy bash script that starts api-server
COPY api/api-server.sh ${SIGREPO_SERVER_DIR}/api/api-server.sh

# Convert bash script from Windows style line endings to Unix-like control characters
RUN dos2unix ${SIGREPO_SERVER_DIR}/api/api-server.sh

# Allow permissions to execute the bash script
RUN chmod a+x ${SIGREPO_SERVER_DIR}/api/api-server.sh

# Copy bash script that starts the MCP server
COPY mcp/mcp-server.sh ${SIGREPO_SERVER_DIR}/mcp/mcp-server.sh

# Convert bash script from Windows style line endings to Unix-like control characters
RUN dos2unix ${SIGREPO_SERVER_DIR}/mcp/mcp-server.sh

# Allow permissions to execute the bash script
RUN chmod a+x ${SIGREPO_SERVER_DIR}/mcp/mcp-server.sh
