FROM sagebionetworks/shiny-base:release-1.0
ARG INSTALL_DIR=/srv/shiny-server/stopadforms
RUN mkdir $INSTALL_DIR
WORKDIR $INSTALL_DIR
COPY ./ ./
#
# Error: install of package 'waldo' failed
# namespace 'rlang' 0.4.9 is being loaded, but >= 1.0.0 is required
# Address this by installing the latest version up front
RUN Rscript -e "install.packages('rlang', repos='http://cran.rstudio.com/')"
#
RUN Rscript -e "install.packages('renv', repos='http://cran.rstudio.com/'); renv::restore()"
