#!/bin/bash

# Exit immediately if any command fails
set -e

# Get current script directory ####
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIGREPO_DIR="$SCRIPT_DIR"



if [ "$(realpath "$path1")" = "$(realpath "$path2")" ]; then
  echo "The directories are the same."
else
  echo "The directories are different."
fi


# Ensure install path is provided ####
if [ -z "$1" ]; then
  echo "ERROR: Installation path not provided."
  echo "Usage: ./install_sigrepo.sh /installation/path"
  exit 1
fi

BASE_INSTALL_DIR="$1"
MYSQL_DIR="$BASE_INSTALL_DIR/mysql"
API_DIR="$BASE_INSTALL_DIR/api"

# Warn about spaces in path ####
if [[ "$BASE_INSTALL_DIR" == *" "* ]]; then
  echo "WARNING: Installation path contains spaces. Docker volume mounting may fail."
fi

# Prompt for MySQL username ####
echo "Please create a username (this will be the super user for the installation (admin)):"
read MYSQL_USER

# Prompt for MySQL user password (hidden input) ####
read -s -p "Please create a MySQL user password: " MYSQL_PASSWORD
echo ""

# Prompt for MySQL root password (hidden input) ####
read -s -p "Please create a MySQL root password: " MYSQL_ROOT_PASSWORD
echo ""

# Function to find an available port ####
find_available_port() {
  local port=$1
  while :; do
  if ! lsof -i:$port >/dev/null 2>&1; then
  echo $port
  return
  fi
  port=$((port + 1))
  done
}

# Use default port 3306 initially ####
DEFAULT_PORT=3306
MYSQL_PORT=$DEFAULT_PORT

# Check if default port is available ####
if lsof -i:$MYSQL_PORT >/dev/null 2>&1; then
  echo "Default port $MYSQL_PORT is in use."

# Find next available port after default ####
SUGGESTED_PORT=$(find_available_port $((MYSQL_PORT + 1)))

while true; do
echo "Suggested available port is $SUGGESTED_PORT. Do you want to use this port? (y/n)"
read -r confirm
if [[ "$confirm" =~ ^[Yy]$ ]]; then
MYSQL_PORT=$SUGGESTED_PORT
break
else
  echo "Please enter an alternative port:"
read MYSQL_PORT

# Validate input is numeric ####
if ! [[ "$MYSQL_PORT" =~ ^[0-9]+$ ]]; then
echo "Invalid port. Please enter a numeric value."
continue
fi

# Check if entered port is free ####
if lsof -i:$MYSQL_PORT >/dev/null 2>&1; then
echo "Port $MYSQL_PORT is in use. Please try again."
else
  break
fi
fi
done
fi

echo "Using MySQL port: $MYSQL_PORT"

# Create necessary volume directories ####
echo "Creating volume directories..."
mkdir -p "$MYSQL_DIR/database"
mkdir -p "$MYSQL_DIR/difexp"

# Create mysql.env file ####
echo "Creating mysql.env..."
cat > "$SIGREPO_DIR/mysql.env" <<EOL
MYSQL_DATABASE='sigrepo'
MYSQL_USER=$MYSQL_USER
MYSQL_PASSWORD=$MYSQL_PASSWORD
MYSQL_ROOT_PASSWORD=$MYSQL_ROOT_PASSWORD
EOL

# Create Docker network if it doesn't exist ####
echo "Creating Docker network..."
docker network create -d bridge db-net || echo "Docker network 'db-net' already exists."

# Export env vars for Docker Compose ####
export BASE_INSTALL_DIR
export SIGREPO_DIR
export MYSQL_PORT

# Start Docker containers
echo "Starting Docker containers..."
docker compose -f "$SIGREPO_DIR/docker-compose-local.yml" up -d --build

# Create .Renviron file for R, and populate with user prompts ####
echo "Creating .Renviron..."
cat > "$SIGREPO_DIR/.Renviron" <<EOL
DB_NAME=sigrepo
DB_HOST=172.18.0.2
DB_HOST_DB_NET=172.18.0.2
DB_PORT=$MYSQL_PORT
DB_API_PORT=8020
DB_USER=$MYSQL_USER
PASSWORD=$MYSQL_PASSWORD
EOL

# Done
echo ""
echo "SigRepo setup is complete!"
echo "Volumes are in: $BASE_INSTALL_DIR"
echo "MySQL is running on port $MYSQL_PORT"


