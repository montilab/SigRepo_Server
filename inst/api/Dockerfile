FROM montilab/sigrepo_shiny:latest

VOLUME /srv/shiny-server/
WORKDIR /srv/shiny-server/
 
RUN Rscript -e 'install.packages("plumber");'

EXPOSE 38380

# RUN chown -R shiny:shiny /srv/shiny-server
# RUN chmod -R 775 /srv/shiny-server
# Copy configuration files to Docker image
# COPY shiny-server.sh /usr/bin/shiny-server.sh

# # Allow permission
# RUN ["chmod", "+rwx", "/srv/shiny-server/"]
# RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# CMD ["/usr/bin/shiny-server.sh"]
# CMD ["/init"]
