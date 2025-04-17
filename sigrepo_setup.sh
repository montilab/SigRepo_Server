#!/bin/bash

# Exit on error
set -e

# Function to check if a command exists
command_exists() {
  command -v "$1" &> /dev/null
}

# Cleanup function
cleanup() {
  echo "An error occurred. Cleaning up..."
  [ -d "$SIGREPO_DIR/mysql" ] && rm -rf "$SIGREPO_DIR/mysql"
  [ -d "$SIGREPO_DIR/api" ] && rm -rf "$SIGREPO_DIR/api"
  docker compose -f "$SIGREPO_DIR/docker-compose-dockerhub.yml" down &>/dev/null || true
  docker network rm db-net &>/dev/null || true
  echo "Cleanup done. Please retry the setup."
}

# Trap any error
trap 'cleanup; exit 1' ERR

# Validate install path
if [ -z "$1" ]; then
  echo " ERROR: Installation path not provided."
  echo "Usage: ./setup_sigrepo.sh /installation/path"
  exit 1
fi

BASE_INSTALL_DIR="$1"
SIGREPO_DIR="$BASE_INSTALL_DIR/SigRepo"

# Get current script dir
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo " Installing SigRepo to: $SIGREPO_DIR"

# Copy everything into $INSTALL_DIR/SigRepo
mkdir -p "$SIGREPO_DIR"
cp -R "$SCRIPT_DIR"/* "$SIGREPO_DIR"

# Create mysql.env file
echo " Creating mysql.env..."
cat > "$SIGREPO_DIR/mysql.env" <<EOL
MYSQL_DATABASE='sigrepo'
MYSQL_USER='montilab'
MYSQL_PASSWORD='sigrepo'
MYSQL_ROOT_PASSWORD='sigrepo'
EOL

# Create directories
echo " Creating required directories..."
mkdir -p "$SIGREPO_DIR/mysql/database"
mkdir -p "$SIGREPO_DIR/api"

# Create Docker network
echo " Creating Docker network..."
docker network create -d bridge db-net || echo "Docker network 'db-net' already exists."

# Edit docker-compose file to use INSTALL_DIR
echo "  Updating docker-compose-dockerhub.yml..."
cd "$SIGREPO_DIR"
if [[ "$OSTYPE" == "darwin"* ]]; then
  sed -i '' 's|\$HOME|'"$SIGREPO_DIR"'|g' docker-compose-dockerhub.yml
else
  sed -i 's|\$HOME|'"$SIGREPO_DIR"'|g' docker-compose-dockerhub.yml
fi

# Start Docker
echo " Starting Docker containers..."
docker compose -f docker-compose-dockerhub.yml up -d

# Check containers
echo "Listing Docker containers..."
docker ps

# Get MySQL IP
echo " Getting MySQL container IP..."
MYSQL_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' sigrepo-mysql)
echo "  MySQL IP Address: $MYSQL_IP"

# Write .Renviron
echo "Creating .Renviron file..."
cat > "$SIGREPO_DIR/.Renviron" <<EOL
DBNAME = "sigrepo"
HOST = "127.0.0.1"
HOST_DB_NET = "$MYSQL_IP"
PORT = 3306
API_PORT = 8020
USER = "root"
PASSWORD = "sigrepo"
EOL

# Restart Docker containers
echo " Restarting containers..."
docker compose -f docker-compose-dockerhub.yml restart

# Done
echo ""
echo "SigRepo is installed at: $SIGREPO_DIR"
echo "Database and API are running in Docker."
echo "Please navigate to the installation directory and open the SigRepo R Project"
