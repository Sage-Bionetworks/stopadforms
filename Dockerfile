FROM sagebionetworks/shiny-base:release-1.0
# This is the expected application installation folder
WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./
RUN Rscript -e "install.packages(c('renv'), repos='http://cran.rstudio.com/'); renv::restore()"
RUN Rscript -e "library(reticulate); install_miniconda(); py_discover_config(); py_install('synapseclient', pip = TRUE, pip_ignore_installed=TRUE)"
