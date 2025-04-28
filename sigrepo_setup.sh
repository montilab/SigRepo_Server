#!/bin/bash

# Exit on error
set -e

# Function to check if a command exists
command_exists() {
  command -v "$1" &> /dev/null
}

# Function to find an available port
find_available_port() {
  local port=$1
  while lsof -i :"$port" &>/dev/null; do
    port=$((port + 1))
  done
  echo "$port"
}

# Get current script directory (assumes running from cloned repo)
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIGREPO_DIR="$SCRIPT_DIR"

# Validate install path
if [ -z "$1" ]; then
  echo " ERROR: Installation path not provided."
  echo "Usage: ./setup_sigrepo.sh /installation/path"
  exit 1
fi

BASE_INSTALL_DIR="$1"
MYSQL_DIR="$BASE_INSTALL_DIR/mysql"
API_DIR="$BASE_INSTALL_DIR/api"

# Cleanup function
cleanup() {
  echo "An error occurred. Cleaning up..."
  [ -d "$MYSQL_DIR" ] && rm -rf "$MYSQL_DIR"
  [ -d "$API_DIR" ] && rm -rf "$API_DIR"
  docker compose -f "$SIGREPO_DIR/docker-compose-dockerhub.yml" down &>/dev/null || true
  docker network rm db-net &>/dev/null || true
  echo "Cleanup done. Please retry the setup."
}

# Trap any error
trap 'cleanup; exit 1' ERR

echo " Running setup from: $SIGREPO_DIR"
echo " Creating volumes at: $BASE_INSTALL_DIR"

# Prompt for MySQL user/password
read -p "Enter MySQL username: " MYSQL_USER
read -s -p "Enter MySQL password: " MYSQL_PASSWORD
echo ""

# Assign available MySQL port starting from 3306
MYSQL_PORT=$(find_available_port 3306)

# Create necessary directories for volumes
echo " Creating volume directories..."
mkdir -p "$MYSQL_DIR/database"
mkdir -p "$API_DIR"

# Create mysql.env file in the repo directory
echo " Creating mysql.env..."
cat > "$SIGREPO_DIR/mysql.env" <<EOL
MYSQL_DATABASE='sigrepo'
MYSQL_USER='$MYSQL_USER'
MYSQL_PASSWORD='$MYSQL_PASSWORD'
MYSQL_ROOT_PASSWORD='$MYSQL_PASSWORD'
EOL

# Create Docker network if it doesn't exist
echo " Creating Docker network..."
docker network create -d bridge db-net || echo "Docker network 'db-net' already exists."

# Update docker-compose file with correct paths and port
echo " Updating docker-compose-dockerhub.yml..."
cd "$SIGREPO_DIR"
if [[ "$OSTYPE" == "darwin"* ]]; then
  sed -i '' "s|\$HOME|$BASE_INSTALL_DIR|g" docker-compose-dockerhub.yml
  sed -i '' "s|3306:3306|$MYSQL_PORT:3306|g" docker-compose-dockerhub.yml
else
  sed -i "s|\$HOME|$BASE_INSTALL_DIR|g" docker-compose-dockerhub.yml
  sed -i "s|3306:3306|$MYSQL_PORT:3306|g" docker-compose-dockerhub.yml
fi

# Start Docker containers
echo " Starting Docker containers..."
docker compose -f "$SIGREPO_DIR/docker-compose-dockerhub.yml" up -d

# List running containers
docker ps

# Get MySQL container IP
echo " Getting MySQL container IP..."
MYSQL_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' sigrepo-mysql)
echo "  MySQL IP Address: $MYSQL_IP"

# Write .Renviron file
echo "Creating .Renviron file..."
cat > "$SIGREPO_DIR/.Renviron" <<EOL
DBNAME = "sigrepo"
HOST = "127.0.0.1"
HOST_DB_NET = "$MYSQL_IP"
PORT = $MYSQL_PORT
API_PORT = 8020
USER = "$MYSQL_USER"
PASSWORD = "$MYSQL_PASSWORD"
EOL



# Restart containers
echo " Restarting containers..."
docker compose -f "$SIGREPO_DIR/docker-compose-dockerhub.yml" restart

# Done
echo ""
echo "SigRepo is ready!"
echo "Repo used from: $SIGREPO_DIR"
echo "Volumes are in: $BASE_INSTALL_DIR"
echo "MySQL is running on port $MYSQL_PORT"
echo "Open the SigRepo R Project in your editor to get started!"
