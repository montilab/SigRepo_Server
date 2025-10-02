#!/bin/bash

# Exit immediately if any command fails
set -e

# Get current script directory ####
SCRIPT_DIR="$( realpath $(dirname "${BASH_SOURCE[0]}") )"

echo "=============================="
echo "   SET UP DATABASE INSTANCE   "
echo "=============================="
echo "1) Local Machine"
echo "2) Virtual Machine"
echo ""

# Prompt until valid input is entered
while true; do
  read -p "Choose an option: " choice
  
  # Check if input is empty
  if [[ -z "$choice" ]]; then
    echo "WARNING: Input cannot be empty. Please try again."
    continue
  fi
  
  # Check if input is a number between 1 and 2
  if [[ "$choice" =~ ^[1-2]$ ]]; then
    break
  else
    echo "WARNING: Invalid option. Please choose between option 1 or 2."
  fi
done

# Act based on choice
case $choice in
  1)
    DB_HOST='0.0.0.0'
    DB_URL="localhost"
    ;;
  2)
    while true; do
      read -p "Please enter the domain of your server (e.g., sigrepo.org) or its IP address (e.g., 142.93.67.157): " DB_HOST
      # Check if input is empty
      if [[ -z "${DB_HOST}" ]]; then
        echo "WARNING: Input cannot be empty. Please try again."
        continue
      else
        DB_URL=${DB_HOST}
        break
      fi
    done
    ;;
esac

# Prompt the user for their name
echo "Please provide a location to set up your MySQL database: "
read MYSQL_DIR

# Get the absolute path of directory
MYSQL_DIR="$( realpath ${MYSQL_DIR} )"

# Create the directory for mysql and difexp
DATABASE_DIR=${MYSQL_DIR}/database
DIFEXP_DIR=${MYSQL_DIR}/difexp

#echo ${DATABASE_DIR}
#echo ${DIFEXP_DIR}

echo "Set up the data storage for MySQL database and difexp..."
mkdir -p ${DATABASE_DIR}
mkdir -p ${DIFEXP_DIR}

# Allow permissions for mysql and difexp folders ####
echo "Apply 775 permission to the storage. When prompted, please enter the admin password to proceed..."
sudo chmod 775 ${DATABASE_DIR}
sudo chmod 775 ${DIFEXP_DIR}

# Prompt for MySQL root password (hidden input) ####
while true; do
  read -s -p "Assign a root password to the database (must be at least 10 characters long): " MYSQL_ROOT_PASSWORD
  echo ""
  
  if [ ${#MYSQL_ROOT_PASSWORD} -lt 10 ]; then
    echo "WARNING: Password must be at least 10 characters long. Please try again."
    continue
  else
    break
  fi
done

# Prompt for an admin key (hidden input) ####
while true; do
  read -s -p "Create an administrator key for accessing the API endpoints (must be at least 10 characters long): " ADMIN_KEY
  echo ""
  
  if [ ${#ADMIN_KEY} -le 10 ]; then
    echo "WARNING: An administrator key must be at least 10 characters long. Please try again."
    continue
  else
    break
  fi
done

echo "Create the database configuration file..."
cat > "${MYSQL_DIR}/.mysql_env" <<EOF
MYSQL_DATABASE = 'sigrepo'
MYSQL_USER = 'montilab'
MYSQL_PASSWORD = 'sigrepo'
MYSQL_ROOT_PASSWORD = '${MYSQL_ROOT_PASSWORD}'
MYSQL_ROOT_HOST = '%'
EOF

# Stop previously containers
echo "Shut down existing containers. Enter the admin password if prompted for permission...."
sudo docker stop sigrepo-mysql sigrepo-api sigrepo-shiny &>/dev/null || echo ""

# Removing previously images
echo "Remove existing images. Enter the admin password if prompted for permission..."
sudo docker rmi --force montilab/sigrepo-mysql:latest montilab/sigrepo:latest &>/dev/null || echo ""

echo "Clean out the stopped containers and cached images. Enter 'y' to procced..."
sudo docker system prune -a || echo ""

# Remove all unwanted files in the folders
sudo rm -rf ${DATABASE_DIR}/*
sudo rm -rf ${DIFEXP_DIR}/*

# Function to find an available port
find_available_port () {
  local port=$1
  local check_port=$(echo -n $(lsof -i:${port}) | wc -m)
  while [ ${check_port} -gt 0 ]
  do
    port=$(printf "%04d" $(( RANDOM % 5999 + 4001 )))
    check_port=$(echo -n $(lsof -i:${port}) | wc -m)
  done
  echo ${port}
}

# Check if default ports are available for database, api, and shiny
echo "Locate open ports to host MySQL database, API, and Shiny..."
DB_PORT=$( find_available_port 3306 )
DB_API_PORT=$( find_available_port 8020 )
DB_SHINY_PORT=$( find_available_port 8050 )

echo "DB PORT: ${DB_PORT}"
echo "API PORT: ${DB_API_PORT}"
echo "SHINY PORT: ${DB_SHINY_PORT}"

echo "Configure docker-compose.yml file to initialize the containers..."
cat > "${MYSQL_DIR}/docker-compose.yml" <<EOF
x-sql-volume:
  &sql-volume
  type: bind
  source: ${DATABASE_DIR}
  target: /var/lib/mysql
  
x-difexp-volume:
  &difexp-volume
  type: bind
  source: ${DIFEXP_DIR}
  target: /difexp
  
services:
  sigrepo-mysql:
    container_name: sigrepo-mysql
    platform: linux/amd64
    image: montilab/sigrepo-mysql:latest
    env_file:
      - .mysql_env
    networks:
      - db-net
    ports:
      - ${DB_PORT}:3306
    restart: always
    volumes:
      - *sql-volume
    command: ["--default-authentication-plugin=mysql_native_password"]

  sigrepo-api:
    container_name: sigrepo-api
    platform: linux/amd64
    image: montilab/sigrepo:latest
    depends_on:
      - sigrepo-mysql
    networks:
      - db-net
    ports:
      - ${DB_API_PORT}:3838
    restart: always
    volumes:
      - .Renviron:/SigRepo_Server/api/.Renviron
      - *difexp-volume
    entrypoint: ["/bin/bash", "-c", "/SigRepo_Server/api/api-server.sh"]

  sigrepo-shiny:
    container_name: sigrepo-shiny
    platform: linux/amd64
    image: montilab/sigrepo:latest
    depends_on:
      - sigrepo-mysql
      - sigrepo-api
    networks:
      - db-net
    ports:
      - ${DB_SHINY_PORT}:3838
    restart: always
    volumes:
      - .Renviron:/SigRepo_Server/shiny/.Renviron
    entrypoint: ["/bin/bash", "-c", "/SigRepo_Server/shiny/shiny-server.sh"]

networks:
  db-net:
    external: true   
EOF

# Create Docker network
echo "Create a network to connect the containers. If prompted, enter the admin password to give permission..."
sudo docker network create -d bridge db-net &>/dev/null || echo "Docker network db-net already exists."

# Start Docker containers
echo "Start the mysql container. If prompted, enter the admin password to give permission..."
sudo docker compose -f ${MYSQL_DIR}/docker-compose.yml up -d sigrepo-mysql

# Retrieve mysql local ip address
DB_LOCAL_HOST=$( docker inspect -f '{{range .NetworkSettings.Networks}}{{.IPAddress}}{{end}}' sigrepo-mysql )

# Create .Renviron file for R, and populate with user prompts ####
echo "Set up .Renviron to initialize SigRepo API and Shiny"
cat > "${MYSQL_DIR}/.Renviron" <<EOF
DB_NAME = 'sigrepo'
DB_HOST = '${DB_HOST}'
DB_LOCAL_HOST = '${DB_LOCAL_HOST}'
DB_PORT = '${DB_PORT}'
DB_API_PORT = '${DB_API_PORT}'
DB_USER = 'root'
DB_PASSWORD = '${MYSQL_ROOT_PASSWORD}'
ADMIN_KEY = '${ADMIN_KEY}'
EOF

# Start sigrepo-api containers
echo "Start the sigrepo-api container. If prompted, enter the admin password to give permission..."
sudo docker compose -f ${MYSQL_DIR}/docker-compose.yml up -d sigrepo-api

# Start sigrepo-shiny containers
echo "Start the sigrepo-shiny container. If prompted, enter the admin password to give permission..."
sudo docker compose -f ${MYSQL_DIR}/docker-compose.yml up -d sigrepo-shiny

# Done
echo ""
echo "SigRepo setup is complete!"
echo "SigRepo MySQL database is currently deployed at port ${DB_PORT}"
echo "SigRepo API is currently deployed at ${DB_URL}:${DB_API_PORT}/__docs__/"
echo "SigRepo Shiny is currently deployed at ${DB_URL}:${DB_SHINY_PORT}"
echo ""

