
#!/bin/bash

PACKAGE_DIR=${PACKAGE_DIR:-/SigRepo}

# Make sure the directory for individual app logs exists
mkdir -p /var/log/shiny-server
chown shiny.shiny /var/log/shiny-server

# push the "real" application logs to stdout with xtail in detached mode
if [ "$APPLICATION_LOGS_TO_STDOUT" != "false" ];
then
    exec xtail /var/log/shiny-server/ &
fi

# Start shiny server
exec Rscript ${PACKAGE_DIR}/inst/register-app/run_sigrepo_shiny.R 2>&1
