<!-- README.md is generated from README.Rmd. Please edit that file -->

stopadforms
===========

<!-- badges: start -->

[![R build
status](https://github.com/Sage-Bionetworks/stopadforms/workflows/R-CMD-check/badge.svg)](https://github.com/Sage-Bionetworks/stopadforms/actions)
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

Below is an example of how to calculate scores for a submission outside
of the Shiny app (i.e. in an R script).

To run this, you will need to have Python and the
[synapseclient](https://python-docs.synapse.org/build/html/index.html)
Python package installed, as stopadforms uses this package to interact
with [Synapse](https://www.synapse.org).

``` r
library("synapseforms")
library("reticulate")
library("tidyverse")
library("stopadforms")

# Optional: use reticulate::use_python() here to tell R which Python version you
# want to use. Again, the synapseclient Python package must be installed as
# well.

synapse <- import("synapseclient") # load the synapseclient python package
syn <- synapse$Synapse()
syn$login()                        # authenticate to Synapse

## Get rejected submissions (rejected submissions include a test submission I
## created) -- in order to run this you will need to be part of the
## STOP-AD_Reviewers team on Synapse (https://www.synapse.org/#!Team:3403721)
sub_data_files <- get_submissions(
  syn,
  group = 9,
  statuses = "REJECTED"
)

## The above command returns a list of pre-signed URLs to the raw JSON data for
## the submissions. These URLs are ephemeral and expire after a short period of
## time, so if you experience 403 permissions errors trying to use them, it may
## be because they have expired and you'll need to re-run the above command.

## Convert to data frame
dat_full <- process_submissions(
  sub_data_files,
  lookup_table = stopadforms:::lookup_table,
  complete = TRUE
)

## Filter to one submission of interest -- a test submission I created.
dat <- filter(dat_full, form_data_id == "242")

## Append clinical modifier used in calculations
submission <- append_clinical_to_submission(dat)

## Get section scores
scores <- pull_reviews_table(syn, "syn22014561", submission)

## Calculate overall score
calculate_submission_score(submission, scores)
```

------------------------------------------------------------------------

Please note that the stopadforms project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
