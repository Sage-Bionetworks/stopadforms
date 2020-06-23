#########################################################
####  Demo of calculating submission score manually  ####
#########################################################

## 2020-06-22

library("synapseforms")
library("reticulate")
library("tidyverse")
pkgload::load_all()

## Replace with your preferred python binary
use_python("~/envs/py3/bin/python", required = TRUE)

## Load synapse client and log in to synapse
synapse <- import("synapseclient")
syn <- synapse$Synapse()
syn$login()

## Get submission data
sub_data_files <- get_submissions(
  syn,
  group = 9,
  statuses = "SUBMITTED_WAITING_FOR_REVIEW"
)

dat_full <- process_submissions(
  sub_data_files,
  lookup_table = lookup_table
)

## Get submission of interest
sub <- "242" # replace this with a form data ID of interest
dat <- filter(dat_full, form_data_id == sub)
submission <- append_clinical_to_submission(dat)

## Get get reviews and calculate weighted section scores
reviews <- pull_reviews_table(syn, "syn22014561", submissions = submission)

## Calculate score for overall submission
calculate_submission_score(submission, reviews)
