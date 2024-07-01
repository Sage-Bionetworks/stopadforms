cran <- c(
"remotes",
"dplyr",
"config",
"glue",
"golem",
"httr",
"jsonlite",
"magrittr",
"purrr",
"reactable",
"readr",
"reticulate",
"rlang",
"shiny",
"shinyjs",
"stringr",
"tibble",
"tidyr",
"waiter",
"htmlwidgets",
# "rsconnect", TODO remove this line if install_version solves the deployment issue
"brio",
"callr",
"diffobj",
"praise",
"processx",
"ps",
"rematch2",
"rmarkdown",
"testthat",
"tinytex"
)

# The binary package distributions from R Studio dramatically speed up installation time
# For Ubuntu 18.04 (Bionic) it's https://packagemanager.rstudio.com/all/__linux__/bionic/latest
# For Ubuntu 20.04 (Focal)  it's https://packagemanager.rstudio.com/all/__linux__/focal/latest
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest", getOption("repos")))
install.packages(cran)

# Downgrade rsconnect to a version that is compatible with shinyapps.io
# https://community.rstudio.com/t/unable-to-deploy-on-shinyapp-io/170160
remotes::install_version("rsconnect", "0.8.29")

packages_installed_from_github <- c("dreamRs/shinypop", "Sage-Bionetworks/dccvalidator", "Sage-Bionetworks/synapseforms")
remotes::install_github(packages_installed_from_github)
