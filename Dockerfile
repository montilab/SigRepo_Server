FROM bioconductor/bioconductor_docker:devel

VOLUME /srv/shiny-server/
WORKDIR /srv/shiny-server/
 
#COPY --chown=rstudio:rstudio . /home/rstudio/
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-utils \
    build-essential \
    libglpk-dev \
    libnode-dev \
    libcurl4-openssl-dev \
    libmysqlclient-dev \
    libxml2-dev \
    libssl-dev \
    openssh-server \
    openssh-client && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* 

RUN Rscript -e 'install.packages("bslib");'
RUN Rscript -e 'install.packages("devtools");'
RUN Rscript -e 'install.packages("tidyverse");'
RUN Rscript -e 'install.packages("ggplot2");'
RUN Rscript -e 'install.packages("ggforce");'
RUN Rscript -e 'install.packages("heatmaply");'
RUN Rscript -e 'install.packages("R6");'
RUN Rscript -e 'install.packages("magrittr");'
RUN Rscript -e 'install.packages("dplyr");'
RUN Rscript -e 'install.packages("purrr");'
RUN Rscript -e 'install.packages("stringr");'
RUN Rscript -e 'install.packages("scales");'
RUN Rscript -e 'install.packages("httr");'
RUN Rscript -e 'install.packages("openxlsx");'
RUN Rscript -e 'install.packages("reshape2");'
RUN Rscript -e 'install.packages("reactable");'
RUN Rscript -e 'install.packages("msigdbr");'
RUN Rscript -e 'install.packages("kableExtra");'
RUN Rscript -e 'install.packages("rmarkdown");'
RUN Rscript -e 'install.packages("igraph");'
RUN Rscript -e 'install.packages("visNetwork");'
RUN Rscript -e 'install.packages("shiny");'
RUN Rscript -e 'install.packages("shinydashboard");'
RUN Rscript -e 'install.packages("shinydashboardPlus");'
RUN Rscript -e 'install.packages("shinyjs");'
RUN Rscript -e 'install.packages("shinyBS");'
RUN Rscript -e 'install.packages("shinyLP");'
RUN Rscript -e 'install.packages("RMySQL");' 
RUN Rscript -e 'install.packages("V8");'
RUN Rscript -e 'install.packages("shinythemes");'
RUN Rscript -e 'install.packages("plotly");'
RUN Rscript -e 'install.packages("markdown");'
RUN Rscript -e 'install.packages("shinyalert");'
RUN Rscript -e 'install.packages("DT");'
RUN Rscript -e 'install.packages("DBI");'
RUN Rscript -e 'install.packages("ontologyIndex");'
RUN Rscript -e 'install.packages("pool");'
RUN Rscript -e 'install.packages("gridExtra");'
RUN Rscript -e 'install.packages("shinycssloaders");'
RUN Rscript -e 'install.packages("getPass");'
RUN Rscript -e 'install.packages("plyr");' 
RUN Rscript -e 'install.packages("BiocManager");'
RUN Rscript -e 'BiocManager::install("limma");'
RUN Rscript -e 'install.packages("dashboardthemes");'
RUN Rscript -e 'BiocManager::install("org.Hs.eg.db");'
RUN Rscript -e 'devtools::install_github("cran/dqshiny");'
RUN Rscript -e 'devtools::install_github("montilab/hypeR");'
RUN Rscript -e 'devtools::install_github("montilab/OmicSignature");' 

# have storage/memory allocation issue when installing these on
# montilab.bu.edu. Commented this command out for when we do
# have enough space to work with. Be sure to uncomment
# the appropriate lines invoking the signatureSearch abilities
# in ui/ui.R and server/server.R
RUN Rscript -e 'install.packages("HDF5Array");'
RUN Rscript -e 'BiocManager::install("reactome.db");'
RUN Rscript -e 'BiocManager::install("SummarizedExperiment");'
RUN Rscript -e 'BiocManager::install("AnnotationDbi");'
RUN Rscript -e 'BiocManager::install("signatureSearch");'

RUN Rscript -e 'devtools::install_github("montilab/SigRepoR");'
RUN Rscript -e 'install.packages("dbGetQuery");'


RUN ln -s /srv/shiny-server/sigrepo/Shiny /srv/shiny-server/SigRepoApp

EXPOSE 38380
EXPOSE 3306

# RUN chown -R shiny:shiny /srv/shiny-server
# RUN chmod -R 775 /srv/shiny-server
# Copy configuration files to Docker image
# COPY shiny-server.sh /usr/bin/shiny-server.sh

# # Allow permission
# RUN ["chmod", "+rwx", "/srv/shiny-server/"]
# RUN ["chmod", "+x", "/usr/bin/shiny-server.sh"]

# CMD ["/usr/bin/shiny-server.sh"]
# CMD ["/init"]
