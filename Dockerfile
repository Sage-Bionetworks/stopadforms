FROM sagebionetworks/shiny-base:release-1.0
ARG INSTALL_DIR=/srv/shiny-server/stopadforms
RUN mkdir $INSTALL_DIR
WORKDIR $INSTALL_DIR
COPY ./ ./
#
# Error: install of package 'waldo' failed
# namespace 'rlang' 0.4.9 is being loaded, but >= 1.0.0 is required
RUN Rscript -e "remove.packages(rlang)"
#
RUN Rscript -e "install.packages('renv', repos='http://cran.rstudio.com/'); renv::restore()"
