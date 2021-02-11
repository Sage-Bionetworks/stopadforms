<!-- README.md is generated from README.Rmd. Please edit that file -->

stopadforms
===========

<!-- badges: start -->

[![R build
status](https://github.com/Sage-Bionetworks/stopadforms/workflows/R-CMD-check/badge.svg)](https://github.com/Sage-Bionetworks/stopadforms/actions)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

stopadforms is an R package and Shiny application to allow reviewers to
view and score submissions to the [STOP-AD Compound Submission
Portal](https://stopadportal.synapse.org/#/).

The application is hosted at
<https://shinypro.synapse.org/users/kwoo/stopadforms/>. To use it, you
must be a member of the [STOP-AD\_Reviewers Synapse
team](https://www.synapse.org/#!Team:3403721) and be logged in to
Synapse. To save reviews, you must be a Certified User on Synapse.

Installation
------------

``` r
devtools::install_github("Sage-Bionetworks/stopadforms")
## Or install locally from within this repository:
devtools::install()
```

Running the application locally
-------------------------------

``` r
library("stopadforms")
run_app()
```

See `vignette("deploying-stopadforms", package = "stopadforms")` for
information on how we deploy the app on the Sage Bionetworks Shiny Pro
server.

------------------------------------------------------------------------

Please note that the stopadforms project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
