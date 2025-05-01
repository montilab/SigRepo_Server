#!/bin/bash

# Exit immediately if any command fails
set -e

# Get current script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIGREPO_DIR="$SCRIPT_DIR"

# Ensure install path is provided
if [ -z "$1" ]; then
  echo "ERROR: Installation path not provided."
  echo "Usage: ./setup_sigrepo.sh /installation/path"
  exit 1
fi

BASE_INSTALL_DIR="$1"
MYSQL_DIR="$BASE_INSTALL_DIR/mysql"
API_DIR="$BASE_INSTALL_DIR/api"

# Warn about spaces in path
if [[ "$BASE_INSTALL_DIR" == *" "* ]]; then
  echo "WARNING: Installation path contains spaces. Docker volume mounting may fail."
fi

# Prompt for MySQL username
echo "Enter MySQL username:" 
read MYSQL_USER

# Prompt for MySQL root password (hidden input)
read -s -p "Enter MySQL root password: " MYSQL_PASSWORD
echo ""


# Ask if user wants to clear existing MySQL data
if [ -d "$MYSQL_DIR/database" ]; then
  read -p "Existing MySQL data found. Do you want to delete it to re-run init.sql? [y/N]: " CONFIRM
  if [[ "$CONFIRM" =~ ^[Yy]$ ]]; then
    echo "Deleting existing MySQL data..."
    rm -rf "$MYSQL_DIR/database"
  else
    echo "Keeping existing MySQL data. init.sql will NOT be re-executed."
  fi
fi

# Create necessary volume directories
echo "Creating volume directories..."
mkdir -p "$MYSQL_DIR/database"
mkdir -p "$MYSQL_DIR/difexp"
mkdir -p "$API_DIR"

# Create mysql.env file
echo "Creating mysql.env..."
cat > "$SIGREPO_DIR/mysql.env" <<EOL
MYSQL_DATABASE='sigrepo'
MYSQL_USER=$MYSQL_USER
MYQL_PASSWORD=$MYSQL_PASSWORD
MYSQL_ROOT_PASSWORD='sigrepo'


EOL

# Create Docker network if it doesn't exist
echo "Creating Docker network..."
docker network create -d bridge db-net || echo "Docker network 'db-net' already exists."

# Export env vars for Docker Compose
export BASE_INSTALL_DIR
export SIGREPO_DIR

# Start Docker containers
echo "Starting Docker containers..."
docker compose -f "$SIGREPO_DIR/docker-compose-dockerhub.yml" up -d --build

# Get MySQL container IP
MYSQL_IP=$(docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' sigrepo-mysql)
MYSQL_PORT=3307

# Create .Renviron file for R
echo "Creating .Renviron..."
cat > "$SIGREPO_DIR/.Renviron" <<EOL
DBNAME=sigrepo
HOST=127.0.0.1
HOST_DB_NET=$MYSQL_IP
PORT=$MYSQL_PORT
API_PORT=8020
USER=$MYSQL_USER
PASSWORD=$MYSQL_PASSWORD
EOL

# Done
echo ""
echo "SigRepo setup is complete!"
echo "Volumes are in: $BASE_INSTALL_DIR"
echo "MySQL is running on port $MYSQL_PORT"
echo "init.sql was executed if the database was freshly created."
