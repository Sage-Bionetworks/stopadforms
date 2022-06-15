
synapse <<- reticulate::import("synapseclient", delay_load = TRUE)
syn <- synapse$Synapse()

syn$login()

lookup_table <- readr::read_csv("data-raw/stopad_lookup_table.csv")


source("R/data-gather-clean.R")
source("R/score.R")

library(tidyverse)

sub_data <- get_submissions(
  syn,
  group = 9,
  statuses = "SUBMITTED_WAITING_FOR_REVIEW"
)


sub_data <- process_submissions(sub_data, lookup_table)

## Load submissions and reviews
submissions <- append_clinical_to_submission(sub_data)

reviews_table = "syn22014561"

source("R/utils.R")

load("data/partial_betas.rda")
partial_betas <- readr::read_csv("data-raw/stopad_beta_weights.csv")

reviews <- pull_reviews_table(syn, reviews_table, submissions)

reviews2 <- syn$tableQuery(glue::glue("SELECT * FROM {reviews_table}"))
reviews2 <- readr::read_csv(reviews2$filepath) %>%
  dplyr::mutate(scorer = get_display_name(syn, .data$scorer)) %>%
  dplyr::mutate(form_data_id = as.character(.data$form_data_id)) 

test <- calculate_scores_rowwise(reviews2, submissions)



calculate_scores_rowwise <- function(reviews, submissions) {
  
reviews <- reviews2 
  
  if (nrow(reviews) == 0) {
    return(
      dplyr::mutate(reviews, weighted_score = numeric(0))
    )
  }
  test3 <- reviews %>%
    ## We have both "abstain" and "none" as scoring options, ideally they'd both
    ## represent 0 but shiny won't let two options have the same underlying
    ## value, so "abstain" gets -1 and we convert it back to zero here. This
    ## will result in the Gamma column showing a value of 0 in the app.
    dplyr::mutate(
      score = dplyr::case_when(.data$score < 0 ~ 0, TRUE ~ .data$score)
    ) %>%
    dplyr::inner_join(submissions, by = c("submission", "step", "form_data_id")) %>%
    dplyr::mutate(step2 = .data$step) %>%
    tidyr::nest(
      data = c(
        .data$section,
        .data$variable,
        .data$response,
        .data$exp_num,
        .data$label,
        .data$step
      )
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      weighted_score = calculate_section_score(
        data = .data$data,
        lookup = partial_betas,
        score = .data$score,
        species = switch(.data$species,
                         within = 0.67,
                         across = 0.33,
                         1
        ),
        clinical = .data$clinical
      )
    ) %>%
    dplyr::rename(step = .data$step2) %>%
    dplyr::select(-.data$data)
}