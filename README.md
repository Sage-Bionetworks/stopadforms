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

Example: calculating scores for a submission
--------------------------------------------

``` r
library("synapseforms")
library("reticulate")
library("tidyverse")
library("stopadforms")

# Optional: use reticulate::use_python() here to set path to Python version you want
# to use
synapse <- import("synapseclient")
syn <- synapse$Synapse()
syn$login() # Authenticate to Synapse

## Get rejected submissions (including several test submissions)
sub_data_files <- get_submissions(
  syn,
  group = 9,
  statuses = "REJECTED"
)

## Convert to data frame
dat_full <- process_submissions(
  sub_data_files,
  lookup_table = stopadforms:::lookup_table,
  complete = TRUE
)

## Filter to one submission of interest -- a test submission I created.
dat <- filter(dat_full, form_data_id == "242")

## Append clinical modifier used in calculations
submissions <- append_clinical_to_submission(dat)

## Get scores
scores <- pull_reviews_table(syn, "syn22014561", submissions)

## Calculate scores
calculate_submission_score(dat, scores)
```

------------------------------------------------------------------------

Please note that the stopadforms project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
