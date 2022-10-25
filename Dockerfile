FROM sagebionetworks/shiny-base:release-1.0
# This is the expected application installation folder
WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./
RUN Rscript -e "install.packages(c('renv', 'rjson'), repos='http://cran.rstudio.com/'); renv::restore(); reticulate::install_miniconda()"
