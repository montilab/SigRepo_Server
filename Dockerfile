FROM bioconductor/bioconductor_docker:latest
 
#COPY --chown=rstudio:rstudio . /home/rstudio/
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    apt-utils \
    build-essential \
    libglpk-dev \
    libnode-dev \
    libmysqlclient-dev \
    openssh-server \
    openssh-client && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* &&\
    Rscript -e \
    'install.packages("devtools"); \
     install.packages("tidyverse"); \
     install.packages("ggplot2"); \
     install.packages("ggforce"); \
     install.packages("heatmaply"); \
     install.packages("R6"); \
     install.packages("magrittr"); \
     install.packages("dplyr"); \
     install.packages("purrr"); \
     install.packages("stringr"); \
     install.packages("scales"); \
     install.packages("httr"); \
     install.packages("openxlsx"); \
     install.packages("reshape2"); \
     install.packages("reactable"); \
     install.packages("msigdbr"); \
     install.packages("kableExtra"); \
     install.packages("rmarkdown"); \
     install.packages("igraph"); \
     install.packages("visNetwork"); \
     install.packages("RMySQL");\
     install.packages("V8");\
     install.packages("plotly");\
     install.packages("markdown");\
     install.packages("DT");\
     install.packages("DBI");\
     install.packages("ontologyIndex");\
     install.packages("pool");\
     install.packages("gridExtra");\
     install.packages("getPass");\
     install.packages("plyr");\ 
     devtools::install_github("montilab/hypeR"); \
     devtools::install_github(repo = "Vanessa104/OmicSignature");'
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
