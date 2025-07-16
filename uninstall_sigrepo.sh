#!/bin/bash

# exit on any error ####

set -e


# ensure install path is provided correctly ####


if [-z "$1"]; then
  echo "ERRO: Installation Path not provided"
  echo "Usage: ./uninstall_sigrepo.sh <Installation Path>"
fi


BASE_INSTALL_DIR="$1"
MYSQL_DIR="$BASE_INSTALL_DIR/mysql"
API_DI="$BASE_INSTALL_DIR/api"
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Confirm uninstallation ####

echo "WARNING: This will permanently remove all SigRepo containers, volumes, and configuration files in:"
echo "  - $BASE_INSTALL_DIR"
read -p "Are you sure you want to continue? (y/n): " confirm
if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
  echo "Aborted."
  exit 0
fi

# Stopping Docker Containers ####
echo  "Stopping Docker containers..."
docker compose -f "$SCRIPT_DIR/docker-compose-dockerhub.yml" down || echo "Docker containers not running"

# Remove Docker network
echo "Removing Docker network..."
docker network rm db-net || echo "Docker network 'db-net' does not exist or is already removed."

# Remove MySQL volume directories
if [ -d "$MYSQL_DIR" ]; then
  echo "Removing MySQL volume directories..."
  rm -rf "$MYSQL_DIR"
else
  echo "MySQL volume directories not found."
fi

# Remove .Renviron file
if [ -f "$SCRIPT_DIR/.Renviron" ]; then
  echo "Removing .Renviron file..."
  rm -f "$SCRIPT_DIR/.Renviron"
fi

# Remove mysql.env file
if [ -f "$SCRIPT_DIR/mysql.env" ]; then
  echo "Removing mysql.env file..."
  rm -f "$SCRIPT_DIR/mysql.env"
fi

echo "Uninstallation complete."
