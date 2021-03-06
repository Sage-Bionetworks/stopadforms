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

Calculating scores for a submission
-----------------------------------

Below is an example of how to calculate scores for a submission outside
of the Shiny app (i.e. in an R script). This example uses sample data
included in the package.

``` r
library("stopadforms")

## Create a list with the form data id as the name, and the path to the included
## JSON file as the value
sample_sub <- list(
  "242" = system.file("extdata", "testdata.json", package = "stopadforms")
)

## Load the JSON data and convert to a data frame
dat <- process_submissions(sample_sub, lookup_table = lookup_table)

## Append clinical multiplier used in calculations
submission <- append_clinical_to_submission(dat)

## Load weighted scores. The contents of this file correspond to the output of
## the `pull_reviews_table()` function.
scores <- read.csv(
  system.file("extdata", "weighted_scores.csv", package = "stopadforms"),
  stringsAsFactors = FALSE
)

## Calculate overall score
calculate_submission_score(submission, scores)
#> [1] 0.2631988
```

See `vignette("scoring", package = "stopadforms")` for a detailed
description of the scoring process and an example of how to score real
submissions that are stored in Synapse.

Running the application locally
-------------------------------

``` r
library("stopadforms")
run_app()
```

Scores entered while running the application locally will persist on
Synapse.

See `vignette("deploying-stopadforms", package = "stopadforms")` for
information on how we deploy the app on the Sage Bionetworks Shiny Pro
server.

------------------------------------------------------------------------

Please note that the stopadforms project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
