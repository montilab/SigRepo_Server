# Create an argument to pull a particular version of base image
ARG mysql_image
ARG mysql_image=${mysql_image:-mysql:8.0-bookworm}

############# Build Stage ##################
FROM ${mysql_image} AS base

# Install system libraries of general use
RUN apt-get update --allow-releaseinfo-change --fix-missing \
  && apt-get -y --no-install-recommends install \
  vim \
  curl \
  && apt clean autoclean \
  && apt autoremove --yes \
	&& rm -rf /var/lib/{apt,dpkg,cache,log}/

# Expose mysql to host
EXPOSE 3306 33060