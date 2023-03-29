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
<https://sagebio.shinyapps.io/stopadforms/>. To use it, you
must be a member of the [STOP-AD\_Reviewers Synapse
team](https://www.synapse.org/#!Team:3403721) and be logged in to
Synapse. To save reviews, you must be a Certified User on Synapse.

Installation
------------

``` r
#install latest dccvalidator
install.packages("dccvalidator")
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

Set localDevelopment variable to "TRUE" in "app.R" file of main directory.  Populate the config.yml file in the parent directory with secrets from lastpass for the "testing" profile.  Contact IT if you need access.

app.R
``` r
localDevelopment = TRUE
```
config.yml
``` yaml
testing:
  app_url: http://127.0.0.1:8100
  client_id: ***
  client_secret: ***
  client_name: "local"

default:
  app_url: https://sagebio.shinyapps.io/stopadforms/
  client_id: ***
  client_secret: ***
  client_name: "stopadforms"

```

With the app.R file selected in R Studio, run the shiny app by clicking the button in R Studio.

Scores entered while running the application locally will persist on
Synapse.

See `vignette("deploying-stopadforms", package = "stopadforms")` for
information on how we deploy the app on the Sage Bionetworks Shiny Pro
server.

------------------------------------------------------------------------

Setting up github actions to deploy
-------------------------------

- Enable workflows in the GitHub repository
- Under [secrets](https://github.com/Sage-Bionetworks/stopadforms/settings/secrets/actions) click 'New repository secret'
- Enter secrets for `RSCONNECT_USER`, `RSCONNECT_TOKEN`, and `RSCONNECT_SECRET`, the values for which are saved in Sage's LastPass.
- Enter secrets for `OAUTH_CLIENT_ID`, and `OAUTH_CLIENT_SECRET` for a Synapse OAuth client configured for this application.
- Trigger the GitHub action.
- Check out the app here: https://sagebio.shinyapps.io/stopadforms-staging.
- After verifying correctness, create a Git branch named release*, e.g., `release-1.0`.
- The app' will become available at https://sagebio.shinyapps.io/stopadforms

------------------------------------------------------------------------

Please note that the stopadforms project is released with a [Contributor
Code of Conduct](.github/CODE_OF_CONDUCT.md). By contributing to this
project, you agree to abide by its terms.
