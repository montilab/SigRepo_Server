#!/bin/bash

# Exit immediately if any command fails
set -e

# Get current script directory, script directory will be the SigRepo GitHub directory ####

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIGREPO_DIR="$SCRIPT_DIR"

# Ensure install path is provided ####
if [ -z "$1" ]; then
  echo "ERROR: Installation path not provided."
  echo "Usage: ./setup_sigrepo.sh /installation/path"
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
echo "Enter MySQL username:" 
read MYSQL_USER

# Prompt for MySQL user password (hidden input) ####
read -s -p "Enter MySQL user password: " MYSQL_PASSWORD
echo ""

# prompt for MySQL root password ####

read -s -p "Enter MYSQL root password" MYSQL_ROOT_PASSWORD

# Ask if user wants to clear existing MySQL data ####
if [ -d "$MYSQL_DIR/database" ]; then
  read -p "Existing MySQL data found. Do you want to delete it to re-run init.sql? [y/N]: " CONFIRM
  if [[ "$CONFIRM" =~ ^[Yy]$ ]]; then
    echo "Deleting existing MySQL data..."
    rm -rf "$MYSQL_DIR/database"
  else
    echo "Keeping existing MySQL data. init.sql will NOT be re-executed."
  fi
fi

# Create necessary volume directories ####
echo "Creating volume directories..."
mkdir -p "$MYSQL_DIR/database"
mkdir -p "$MYSQL_DIR/difexp"

# Create mysql.env file ####
echo "Creating mysql.env..."
cat > "$SIGREPO_DIR/mysql.env" <<EOL
MYSQL_DATABASE='sigrepo'
MYSQL_USER=$MYSQL_USER
MYQL_PASSWORD=$MYSQL_PASSWORD
MYSQL_ROOT_PASSWORD=$MYSQL_ROOT_PASSWORD


EOL

# Create Docker network if it doesn't exist ####
echo "Creating Docker network..."
docker network create -d bridge db-net || echo "Docker network 'db-net' already exists."

# Export env vars for Docker Compose ####
export BASE_INSTALL_DIR
export SIGREPO_DIR

# Start Docker containers ####
echo "Starting Docker containers..."
docker compose -f "$SIGREPO_DIR/docker-compose-dockerhub.yml" up -d --build

# find available port ####

DEFAULT_PORT=3306
PORT=$DEFAULT_PORT

# function to check if a port is free ####

is_port_free(){
  ! lsof -iTCP -sTCP:LISTEN -P -n | grep -q ":$1"
}

# loop to find a free port ####

while ! is_port_free $PORT; do
  ((PORT++))
done

echo "Usinig MySQL port: $PORT"

# Create .Renviron file for R ####
echo "Creating .Renviron..."
cat > "$SIGREPO_DIR/.Renviron" <<EOL
DBNAME=sigrepo
HOST=127.0.0.1
HOST_DB_NET=172.18.0.2
PORT=$PORT
API_PORT=8020
USER=$MYSQL_USER
PASSWORD=$MYSQL_PASSWORD
EOL

# Done ####
echo ""
echo "SigRepo setup is complete!"
echo "Volumes are in: $BASE_INSTALL_DIR"
echo "MySQL is running on port $MYSQL_PORT"
echo "init.sql was executed if the database was freshly created."
