# Create an argument to pull a particular version of base image
ARG mysql_image
ARG mysql_image=${mysql_image:-mysql:8.0-bookworm}

############# Build Stage ##################
FROM ${mysql_image} AS base

# Install small debug tools across distro variants used by mysql base images.
RUN set -eux; \
  if command -v apt-get >/dev/null 2>&1; then \
    apt-get update --allow-releaseinfo-change --fix-missing; \
    apt-get -y --no-install-recommends install vim curl; \
    apt-get clean; \
    rm -rf /var/lib/apt/lists/*; \
  elif command -v microdnf >/dev/null 2>&1; then \
    microdnf install -y vim-minimal curl; \
    microdnf clean all; \
  elif command -v yum >/dev/null 2>&1; then \
    yum install -y vim-minimal curl; \
    yum clean all; \
  else \
    echo "No supported package manager found in base image"; \
    exit 1; \
  fi

# Expose mysql to host
EXPOSE 3306 33060
