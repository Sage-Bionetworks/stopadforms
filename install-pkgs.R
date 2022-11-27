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
"rsconnect"
)

gh <- c("dreamRs/shinypop", "Sage-Bionetworks/dccvalidator", "Sage-Bionetworks/synapseforms")

# The binary package distributions from R Studio dramatically speed up installation time
# For Ubuntu 18.04 (Bionic) it's https://packagemanager.rstudio.com/all/__linux__/bionic/latest
# For Ubuntu 20.04 (Focal)  it's https://packagemanager.rstudio.com/all/__linux__/focal/latest
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest", getOption("repos")))
install.packages(cran)
remotes::install_github(gh)

# Install Python and the Synapse Python client
reticulate::install_miniconda()
reticulate::py_discover_config()
reticulate::py_install('synapseclient', pip = TRUE, pip_ignore_installed=TRUE)

