#!/bin/bash

SIGREPO_SERVER_DIR=${SIGREPO_SERVER_DIR:-/SigRepo_Server}

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];
then
    # push the "real" application logs to stdout with xtail in detached mode
    exec xtail /var/log/shiny-server/ &
fi

# Start shiny server
exec Rscript ${SIGREPO_SERVER_DIR}/sendmail/run_sendmail_api.R 2>&1