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
    SERVER_URL="localhost"
    ;;
  2)
    while true; do
      read -p "Please enter the public domain of your server (e.g., sigrepo.org) or its public IP address (e.g., 142.93.67.157): " DB_HOST
      # Check if input is empty
      if [[ -z "${DB_HOST}" ]]; then
        echo "WARNING: Input cannot be empty. Please try again."
        continue
      else
        SERVER_URL=${DB_HOST}
        break
      fi
    done
    ;;
esac

# Prompt the user for a storage location
echo "Please provide a location to set up your MySQL database: "
read MYSQL_DIR

if [[ -z "${MYSQL_DIR}" ]]; then
  echo "ERROR: a storage location is required."
  exit 1
fi

# Expand a leading ~ by hand: it is only expanded by the shell when unquoted at
# the start of a word, and `read` gives us the literal characters.
MYSQL_DIR="${MYSQL_DIR/#\~/$HOME}"

# Create the directory BEFORE resolving it. `realpath` fails on a path that does
# not exist yet, and `set -e` then kills the script -- which is exactly what the
# prompt above invites, since it asks where to SET UP the database. BSD/macOS
# realpath has no `-m`, so resolving-without-existing is not portable.
mkdir -p "${MYSQL_DIR}"
MYSQL_DIR="$( cd "${MYSQL_DIR}" && pwd -P )"

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

# Wiping the data directories is how a re-run gets a clean database -- and it is
# also how someone re-running this to "upgrade" loses everything they have
# stored. Never do it silently: if there is a database here, say so and make the
# user type the word.
if [ -n "$(ls -A ${DATABASE_DIR} 2>/dev/null)" ] || [ -n "$(ls -A ${DIFEXP_DIR} 2>/dev/null)" ]; then
  echo ""
  echo "WARNING: ${MYSQL_DIR} already contains SigRepo data:"
  echo "  database: $(du -sh ${DATABASE_DIR} 2>/dev/null | cut -f1) in ${DATABASE_DIR}"
  echo "  difexp:   $(du -sh ${DIFEXP_DIR} 2>/dev/null | cut -f1) in ${DIFEXP_DIR}"
  echo ""
  echo "Continuing ERASES both. Every signature, collection and user in this"
  echo "instance is deleted, and there is no undo. To keep them, answer no and"
  echo "back up ${MYSQL_DIR} first."
  echo ""
  read -p "Type 'erase' to delete this data and start fresh, or anything else to abort: " confirm_wipe
  if [ "${confirm_wipe}" != "erase" ]; then
    echo "Aborted. Nothing was deleted."
    exit 1
  fi
  echo "Erasing the existing database..."
fi

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
  
  if [ ${#ADMIN_KEY} -lt 10 ]; then
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
# sigrepo-shiny is retired, but is still named here so an upgrade from an
# older install stops the leftover container instead of leaving it running.
sudo docker stop sigrepo-mysql sigrepo-api sigrepo-web sigrepo-shiny &>/dev/null || echo ""

# Removing previously images
echo "Remove existing images. Enter the admin password if prompted for permission..."
sudo docker rmi --force montilab/sigrepo-mysql:latest montilab/sigrepo:latest montilab/sigrepo-web:latest &>/dev/null || echo ""

# NOT `docker system prune -a`: that deletes every unused image, container,
# network and build cache on the whole machine, including other projects'. This
# removes only the containers this installer manages.
echo "Remove the previous SigRepo containers..."
sudo docker rm -f sigrepo-mysql sigrepo-api sigrepo-web sigrepo-shiny &>/dev/null || true

sudo rm -rf ${DATABASE_DIR}/* ${DATABASE_DIR}/.[!.]* 2>/dev/null || true
sudo rm -rf ${DIFEXP_DIR}/* ${DIFEXP_DIR}/.[!.]* 2>/dev/null || true

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

# Check if default ports are available for database, api, and the web interface
echo "Locate open ports to host MySQL database, API, and the web interface..."
DB_HOST_PORT=$( find_available_port 3306 )
API_PORT=$( find_available_port 8020 )
WEB_PORT=$( find_available_port 8050 )

echo "DB PORT (host): ${DB_HOST_PORT}"
echo "API PORT: ${API_PORT}"
echo "WEB PORT: ${WEB_PORT}"

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
      - ${DB_HOST_PORT}:3306
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
      - ${API_PORT}:3838
    restart: always
    volumes:
      - *difexp-volume
      - .Renviron:/SigRepo_Server/.Renviron
    entrypoint: ["/bin/bash", "-c", "/SigRepo_Server/api/api-server.sh"]

  sigrepo-web:
    container_name: sigrepo-web
    platform: linux/amd64
    image: montilab/sigrepo-web:latest
    depends_on:
      - sigrepo-mysql
      - sigrepo-api
    networks:
      - db-net
    ports:
      - ${WEB_PORT}:80
    restart: always

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

# Address MySQL by its container name, not its IP. Docker assigns a new IP every
# time a container is recreated, so an IP written here works until the first
# `docker compose down && up` and then silently stops resolving. The name is
# stable and is what the production compose file uses.
DB_LOCAL_HOST=sigrepo-mysql

# The port MySQL listens on INSIDE the network is always 3306. ${DB_HOST_PORT}
# is only the host-side mapping, and using it here is a connection failure
# waiting to happen: if 3306 is already taken on this machine (a local MySQL
# install is enough), the mapping moves to a random port and the API would then
# try to reach the database on a port nothing is listening on.
DB_CONTAINER_PORT=3306

# Create .Renviron file for R, and populate with user prompts ####
echo "Set up .Renviron to initialize the SigRepo API and web interface"
cat > "${MYSQL_DIR}/.Renviron" <<EOF
DB_NAME = 'sigrepo'
DB_LOCAL_HOST = '${DB_LOCAL_HOST}'
DB_PORT = '${DB_CONTAINER_PORT}'
DB_USER = 'root'
DB_PASSWORD = '${MYSQL_ROOT_PASSWORD}'
ADMIN_KEY = '${ADMIN_KEY}'
EOF

# Start sigrepo-api containers
echo "Start the sigrepo-api container. If prompted, enter the admin password to give permission..."
sudo docker compose -f ${MYSQL_DIR}/docker-compose.yml up -d sigrepo-api

# DB_HOST / API_HOST / API_PORT are not read by the API -- it reaches MySQL over
# the Docker network using DB_LOCAL_HOST above. They exist so an R client can be
# pointed at this instance, so they have to be addresses reachable from OUTSIDE
# the containers: the published host ports, never a container IP. A container IP
# is not routable from the host at all on Docker Desktop (macOS and Windows).
case $choice in
  1)
    DB_HOST=127.0.0.1
    CONTAINER_API_HOST=127.0.0.1
    CONTAINER_API_PORT=${API_PORT}
    ;;
  2)
    DB_HOST=${DB_HOST}
    CONTAINER_API_HOST=${DB_HOST}
    CONTAINER_API_PORT=${API_PORT}
    ;;
esac

# Append the rest of variables to environment file
chmod 600 "${MYSQL_DIR}/.Renviron" "${MYSQL_DIR}/.mysql_env" 2>/dev/null || true

echo "DB_HOST = '${DB_HOST}'" >> "${MYSQL_DIR}/.Renviron" 
echo "API_HOST = '${CONTAINER_API_HOST}'" >> "${MYSQL_DIR}/.Renviron" 
echo "API_PORT = '${CONTAINER_API_PORT}'" >> "${MYSQL_DIR}/.Renviron"
# The published MySQL port, for an R client connecting from the host. DB_PORT
# above is the in-network port and is what the API uses; these differ whenever
# 3306 was already taken on this machine.
echo "DB_HOST_PORT = '${DB_HOST_PORT}'" >> "${MYSQL_DIR}/.Renviron" 

# Start sigrepo-web containers
echo "Start the sigrepo-web container. If prompted, enter the admin password to give permission..."
sudo docker compose -f ${MYSQL_DIR}/docker-compose.yml up -d sigrepo-web

# Done
echo ""
echo "The SigRepo containers are running."
echo ""
echo "  MySQL database   port ${DB_HOST_PORT}"
echo "  API              http://${SERVER_URL}:${API_PORT}/__docs__/"
echo "  Web interface    http://${SERVER_URL}:${WEB_PORT}"
echo ""
echo "ONE STEP REMAINS. The database has no schema yet, so the web interface"
echo "will sign you in to an empty instance. Create the tables and reference"
echo "data by running the init_db endpoint once, with the administrator key you"
echo "chose above:"
echo ""
echo "  open http://${SERVER_URL}:${API_PORT}/__docs__/ and run POST /init_db"
echo ""
echo "It takes a few minutes -- it loads the organism, platform, phenotype and"
echo "feature reference tables. When it finishes, sign in at"
echo "http://${SERVER_URL}:${WEB_PORT} with username 'montilab' and password"
echo "'sigrepo', and change that password."
echo ""

