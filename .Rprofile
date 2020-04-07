.First <- function() {
  options(
    repos = c(
      CRAN = "https://cran.rstudio.com/"
    )
  )
}

if (!identical(Sys.getenv("GITHUB_ACTIONS"), "true") | getRversion() < "4.0.0") {
  source("renv/activate.R")
}
