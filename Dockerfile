FROM bioconductor/bioconductor_docker:devel
 
#COPY --chown=rstudio:rstudio . /home/rstudio/
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-utils \
    build-essential \
    libglpk-dev \
    libcurl4-openssl-dev \
    libmysqlclient-dev \
    libxml2-dev \
    libssl-dev \
    openssh-server \
    openssh-client && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* &&\
    Rscript -e \
    'install.packages("devtools"); \
    install.packages("R6"); \
    install.packages("magrittr"); \
    install.packages("dplyr"); \
    install.packages("stringr"); \
    install.packages("msigdbr"); \
    install.packages("rmarkdown"); \
    install.packages("RMySQL");\
    install.packages("markdown");\
    install.packages("DBI");\
    install.packages("ontologyIndex");\
    install.packages("pool");\
    install.packages("getPass");\
    install.packages("plyr");\
    if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager");\
    BiocManager::install("limma");\
    devtools::install_github(repo="montilab/OmicSignature");\
    devtools::install_github(repo="montilab/SigRepoR",auth_token="4a30c79965d4df0faf415747531f129b0864b3ce");'

#COPY Shiny /srv/shiny-server/sigrepo/Shiny/
#COPY R /srv/shiny-server/sigrepo/R
#COPY miscellanea /srv/shiny-server/sigrepo/miscellanea
#COPY OmicSignature /srv/shiny-server/sigrepo/OmicSignature
#COPY SigRepoR /srv/shiny-server/sigrepo/SigRepoR
#COPY OmicSig_Shiny.Rproj /srv/shiny-server/sigrepo/
#COPY sigrepo090920_createsOnly.sql /srv/shiny-server/sigrepo/

#RUN ln -s /srv/shiny-server/sigrepo/Shiny /srv/shiny-server/SigRepoApp

# Allow SSH Root Login
#RUN sed -i 's|^#PermitRootLogin.*|PermitRootLogin yes|g' /etc/ssh/sshd_config
# Configure root password
#RUN echo "root:root123" | chpasswd 
