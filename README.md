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
#install app from package
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

## Prerequisites

1. You must be a member of the [STOP-AD_Reviewers](https://www.synapse.org/#!Team:3403721) team in Synapse.

2. Your local .synapseConfig file must be populated with a valid Synapse PAT 
that has both "download" and "update" permissions. If you use the Synapse UI to 
create your PAT, select the "Download" and "Modify" options.

## Configuring the application to run locally

1. Modify 'app.R'
Set the `localDevelopment` variable to `TRUE`. 
```
localDevelopment = TRUE
```

2. Modify `config.yml`
Populate the `testing` profile's `client_secret` with the local client password, 
which is stored in lastpass as "stopadforms synapse client". Contact IT if you 
need access to the shared secret.
```
testing:
  app_url: http://127.0.0.1:8100
  client_id: 100167
  client_secret: ***stored in lastpass***
  client_name: "local"
  preserve_logs: TRUE
```

3. Install R packages
Install packages from `renv.lock` by issuing the following command:
```
renv::restore()
```

4. Create Python virtual environment & install required Python packages
Run the following code (derived from `mod-synapse-oauth.R`) to create the 
required python3 virtual environment and install the required Python packages. 

```
venv_folder<-'./python3_env'
reticulate::virtualenv_create(envname = venv_folder, python = '/usr/bin/python3')
reticulate::virtualenv_install(venv_folder, packages = c('synapseclient<2.8', 'pandas<1.5'))
reticulate::use_virtualenv(venv_folder, required = T)

```

5. Additional steps for OSX
If the app will not run locally after the above steps, try the additional 
steps described in this section.

5.1 Verify required environment variables are set
Check to ensure that the required environment variables are set with the 
expected values:
```
Sys.getenv("R_CONFIG_ACTIVE")
Sys.setenv(app_url)  
Sys.setenv(client_name)
Sys.setenv(client_id)
Sys.setenv(client_secret)
```

If they are not, you can manually set them:
```
Sys.setenv("R_CONFIG_ACTIVE"= "testing")
Sys.setenv(app_url = "http://127.0.0.1:8100")  
Sys.setenv(client_name='local')
Sys.setenv(client_id='100167')
Sys.setenv(client_secret='***stored in lastpass***')
```

5.2 Modify `R/app-server.R'
You may need to add an additional line of code to `R/app-server.R' that 
forces the user-specific Synapse client to log in prior to checking whether it's
logged in:

```
  ## Synapse client for a specific user
  syn <- synapse$Synapse()
  ## Oauth
  syn <- callModule(
    mod_synapse_oauth_server,
    "oauth",
    syn = syn
  )

  # TODO don't forget to remove this local development hack!
  # log into synpase with the user-specific client before the logged_in(syn) check
  attempt_login(syn) 
  
  shiny::req(
    inherits(syn, "synapseclient.client.Synapse"),
    logged_in(syn)
  )
```

## Running the application
With the `app.R` file selected in R Studio, run the shiny app by clicking the "Run App" button in R Studio.

Be aware that scores entered while running the application locally *will* be persisted in
Synapse!!! 

## Running the tests
To run the all the tests in the `tests/testthat` directory:
```
test_dir("tests/testthat/")
```

To run a specific test file from the `tests/testthat` directory:
```
test_file("tests/testthat/<path/to/filename.R>")
```

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
