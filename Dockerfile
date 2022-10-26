FROM sagebionetworks/shiny-base:release-1.0
# This is the expected application installation folder
WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./
RUN Rscript -e "install.packages(c('renv'), repos='http://cran.rstudio.com/'); renv::restore()"
# RUN Rscript -e "library(reticulate); use_python('/usr/bin/python3'); py_config(); py_install('synapseclient', method='auto')"
