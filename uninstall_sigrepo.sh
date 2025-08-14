#!/bin/bash

set -e

# Stop and remove containers by name
docker stop sigrepo-shiny sigrepo-api sigrepo-mysql 2>/dev/null || true
docker rm sigrepo-shiny sigrepo-api sigrepo-mysql 2>/dev/null || true

# Remove Docker network
docker network rm db-net 2>/dev/null || echo "Network 'db-net' not found or already removed."

# Remove volume directories
INSTALL_PATH="$1"
if [ -z "$INSTALL_PATH" ]; then
  echo "Usage: $0 /installation/path"
  exit 1
fi

rm -rf "$INSTALL_PATH/mysql"


echo "✅ SigRepo containers and data have been removed."
