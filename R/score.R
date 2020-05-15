#' Score an entire submission
#'
#' Given a data frame containing a submission and a table of reviews, calculate
#' the score for a submission. First, the score for each section is calculated
#' by multiplying the reviewer's score by the appropriate multipliers (see
#' [calculate_section_score()]). If multiple reviewers scored the section, the
#' section score is calculated for each and averaged.
#'
#' Then, the scores across all sections are added and divided by the denominator
#' (see [calculate_denominator()]).
#'
#' @seealso [calculate_section_score()]
#' @seealso [calculate_denominator()]
#' @param data Data frame containining the submission. Should have columns
#'   `section`, `step`, `variable`, and `response` at a minimum.
#' @param reviews Data frame of scores submitted by reviewers.
#' @inheritParams calculate_section_score
#' @return The score for the submission
calculate_submission_score <- function(data, reviews, lookup) {
  off_label <- data[data$variable == "is_off_label", "response", drop = TRUE]
  clinical <- ifelse(off_label == "Yes", .67, .33)
  if (length(clinical) == 0) {
    warning("Data does not indicate whether compound is available for off-label use. Clinical multiplier will be set to 1 instead of 0.67 (clinical) or 0.33 (preclinical).") # nolint
    clinical <- 1
  }

  sect_scores_split <- split(reviews, reviews$step)
  section_scores_averaged <- purrr::map_dbl(
    sect_scores_split,
    function(x) {
      scores <- purrr::pmap_dbl(x, function(step, score, species, ...) {
        species <- switch(species, within = 0.67, across = 0.33, 1)
        calculate_section_score(
          data = dplyr::filter(data, .data$step == {{ step }}),
          lookup = lookup,
          score = score,
          species = species,
          clinical = clinical
        )
      })
      mean(scores)
    }
  )
  sum(section_scores_averaged) / calculate_denominator(data)
}

#' Calculate the score for a section
#'
#' Given data for a single section of a submission, calculate that section's
#' score based on multiplication of the clinical vs. preclinical factor, within-
#' vs. across-species factor, section factor, partial beta weight, and score
#' given by the reviewer (or average score across multiple reviewers).
#'
#' @param data Data frame containing the section data. Should have columns
#'   `section`, `variable`, and `response` at a minimum.
#' @param lookup Lookup table that contains the partial beta weights for
#'   applicable variables.
#' @param clinical Clinical (usually 0.67) or preclinical (usually 0.33)
#'   multiplication factor. Not used for sections `naming`, `basic`,
#'   `pk_in_silico` or `pk_in_vivo`.
#' @param species Within-species (usually 0.67) or across-species (usually 0.33)
#'   multiplication factor. Not used for sections `basic`, `pk_in_silico` or
#'   `pk_in_vivo`.
#' @param score Average score given by reviewers.
#' @return A numeric value indicating the score for the section
#' @export
calculate_section_score <- function(data, lookup, score = 1, species = 1,
                                    clinical = 1) {
  if (nrow(data) == 0) {
    return(0)
  }
  section_name <- unique(data$section)
  if (length(section_name) > 1) {
    stop("Can only calculate score for one section at a time.")
  }
  ## Clinical section is not scored
  if (section_name == "clinical") {
    return(0)
  }
  ## PK sections have an additional multiplier
  section <- switch(
    section_name,
    "pk_in_silico" = 0.17,
    "pk_in_vitro" = 0.33,
    "pk_in_vivo" = 0.5,
    1
  )
  ## Basic data, PK in silico, and in PK in vitro do not use clinical multiplier
  if (section_name %in% c("naming", "basic", "pk_in_silico", "pk_in_vitro")) {
    clinical <- 1
    species <- 1
  }
  ## Some sections have no specific fields with partial beta values, but rather
  ## get scored on the presence/absence of the data overall.
  if (section_name %in% c("acute_dosing", "chronic_dosing", "teratogenicity")) {
    return(section * clinical * species * score)
  }
  ## Some fields are scored based on the values the submitter provided
  if (section_name == "basic") {
    ap <- data[data$variable == "therapeutic_approach", "response", drop = TRUE]
    lookup <- dplyr::mutate(
      lookup,
      partial_beta = dplyr::case_when(
        variable == "therapeutic_approach" ~ approach_beta(ap),
        TRUE ~ partial_beta
      )
    )
  }
  if (section_name == "efficacy") {
    ef <- data[data$variable == "cell_line_efficacy.efficacy_measure_type", "response", drop = TRUE] # nolint
    lookup <- dplyr::mutate(
      lookup,
      partial_beta = dplyr::case_when(
        variable == "cell_line_efficacy.efficacy_measure_type" ~ efficacy_beta(ef), # nolint
        TRUE ~ partial_beta
      )
    )
  }
  section_multiplier <- section * clinical * species * score
  partial_betas <- dplyr::inner_join(data, lookup, by = c("section", "variable"))
  sum(section_multiplier * partial_betas$partial_beta, na.rm = TRUE)
}

## Betas for therapeutic approach
approach_beta <- function(approach) {
  switch(
    approach,
    prophylactic = 0.4,
    symptomatic = 0.2,
    "prophylactic, symptomatic" = 0.3,
    both = 0.3, # just in case "both" is still present for some reason
    unknown = 0.1
  )
}

## Betas of efficacy measure
efficacy_beta <- function(efficacy_measure) {
  switch(
    efficacy_measure,
    EC50 = .67,
    IC50 = .33,
    0
  )
}

#' Calculate denominator
#'
#' Calculates the denominator by which the score should be divided
#'
#' @param data Data frame containing the submission data
#' @return A number representing the denominator by which the score should be
#'   divided
#' @export
calculate_denominator <- function(data) {
  base_points <- tibble::tribble(
            ~section, ~points,
    "naming",               1,
    "basic",                2,
    "binding",              1,
    "efficacy",             1,
    "in_vivo_data",         1,
    "pk",                   1,
    "ld50",                 1,
    "acute_dosing",         1,
    "chronic_dosing",       1,
    "teratogenicity",       1
  )

  points <- data %>%
    dplyr::select(.data$section, .data$exp_num) %>%
    ## For the purposes of scoring, exp_num == `NA` and exp_num == 1 are
    ## equivalent. Filling this in avoids having mix of NA and 1 for experiment
    ## numbers in the PK section(s) (in silico and in vitro would have NA, but
    ## in vivo would have 1).
    dplyr::mutate(
      exp_num = dplyr::case_when(is.na(.data$exp_num) ~ 1L, TRUE ~ .data$exp_num)
    ) %>%
    dplyr::filter(!.data$section %in% c("measurements", "clinical_data")) %>%
    ## Combine PK sections -- together they get one point
    dplyr::mutate(
      section = dplyr::case_when(
        .data$section %in% c("pk_in_vitro", "pk_in_vivo", "pk_in_silico") ~ "pk",
        TRUE ~ .data$section
      )
    ) %>%
    unique() %>%
    dplyr::full_join(base_points, by = "section")

  sum(points$points)
}
