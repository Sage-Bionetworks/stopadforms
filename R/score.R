

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
#' @seealso [calculate_scores_rowwise()]
#' @seealso [pull_reviews_table()]
#' @seealso [calculate_denominator()]
#' @param submission Data frame containining the submission. Should have columns
#'   `section`, `step`, `variable`, and `response` at a minimum.
#' @param reviews Data frame containing weighted scores (output of
#'   [calculate_scores_rowwise()], or [pull_reviews_table()]).
#' @return The score for the submission
#' @export

##########################################################################

calculate_submission_score <- function(submission, reviews) {
  if (nrow(reviews) == 0) {
    return(0)
  }
  section_scores_averaged <- reviews %>%
    dplyr::group_by(.data$step) %>%
    dplyr::summarize(weighted_score = geom_mean_score(.data$weighted_score))
  total <- sum(section_scores_averaged$weighted_score, na.rm = TRUE)
  total / calculate_denominator(submission)
}

#additional function to recreate pivot_longer legacy

#' A legacy pivot_longer function
#' 
#' @param clinicals The list of clinicals
transformLegacyPivotLonger <- function(clinicals) {
  clinicalsT <- clinicals %>%
    t()
  
  form_data_ids <- rownames(clinicalsT)
  
  clinicals <- clinicalsT %>%
    as.data.frame() %>%
    dplyr::mutate(form_data_id = form_data_ids) %>%
    dplyr::rename(clinical = 'V1')
  
  return(clinicals)
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
  section <- switch(section_name,
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
    ef <- data[data$variable == "efficacy_measure_type", "response", drop = TRUE] # nolint
    lookup <- dplyr::mutate(
      lookup,
      partial_beta = dplyr::case_when(
        variable == "efficacy_measure_type" ~ efficacy_beta(ef), # nolint
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
  switch(approach,
    prophylactic = 0.4,
    symptomatic = 0.2,
    "prophylactic, symptomatic" = 0.3,
    both = 0.3, # just in case "both" is still present for some reason
    unknown = 0.1
  )
}

## Betas of efficacy measure
efficacy_beta <- function(efficacy_measure) {
  switch(efficacy_measure,
    EC50 = .67,
    IC50 = .33,
    0
  )
}

## Get clinical multiplier
get_clinical <- function(data) {
  off_label <- data[data$variable == "is_off_label", "response", drop = TRUE]
  clinical <- ifelse(off_label == "Yes", .67, .33)
  if (length(clinical) == 0) {
    warning("Data does not indicate whether compound is available for off-label use. Clinical multiplier will be set to 1 instead of 0.67 (clinical) or 0.33 (preclinical).") # nolint
    clinical <- 1
  }
  clinical
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
  if (is.null(data)) {
    return(NULL)
  }
  base_points <- tibble::tribble(
    ~section, ~points,
    "naming", 1,
    "basic", 2,
    "binding", 1,
    "efficacy", 1,
    "in_vivo_data", 1,
    "pk_in_silico", 0.17,
    "pk_in_vitro", 0.33,
    "pk_in_vivo", 0.5,
    "ld50", 1,
    "acute_dosing", 1,
    "chronic_dosing", 1,
    "teratogenicity", 1
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
    unique() %>%
    dplyr::full_join(base_points, by = "section")

  sum(points$points)
}

#' Append clinical multiplier to submissions
#'
#' Given a data frome of one or more submissions, finds the clinical value and
#' appends it to the data in a new column.
#'
#' @param submissions Data containing one or more submissiosn
#' @return Data from `submissions` with an added `clinical` column containing
#'   the multiplier
#' @export
append_clinical_to_submission <- function(submissions) {
  clinicals <- purrr::map_dfc(
    split(submissions, submissions$form_data_id),
    get_clinical
  ) %>%
    transformLegacyPivotLonger
  
  with_clinical <- submissions %>%
    dplyr::left_join(clinicals, by = "form_data_id")
  with_clinical
}

#' Calculate scores rowwise
#'
#' Given a data frame of scores, adds a column with the weighted score
#' calculated.
#'
#' @inheritParams show_review_table
#' @param submissions Data frame of submissions *including* clinical multiplier
#'   (i.e. the output from [append_clinical_to_submission()]).
#' @export
calculate_scores_rowwise <- function(reviews, submissions) {
  if (nrow(reviews) == 0) {
    return(
      dplyr::mutate(reviews, weighted_score = numeric(0))
    )
  }
  reviews %>%
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
      section_flag = dplyr::case_when(
        section %in% c("binding", "efficacy", "in_vivo_data", "pk_in_vivo", 
                       "acute_dosing", "chronic_dosing", "teratogenicity",
                       "toxicology") ~ 0,
        TRUE ~ 1
      )
    ) %>%
    dplyr::mutate(
      weighted_score = calculate_section_score(
        data = .data$data,
        lookup = partial_betas,
        score = .data$score,
        species = switch(.data$species,
          within = 0.67,
          across = 0.33,
          section_flag
        ),
        clinical = .data$clinical
      )
    ) %>%
    dplyr::rename(step = .data$step2) %>%
    dplyr::select(-.data$data)
}

#' Calculate geometric mean of non-zero scores
#'
#' Discards any zero or NA scores, then calculates the geometric mean of the
#' remaining scores. If all scores are zero, returns 0.
#'
#' @param values Values to average
geom_mean_score <- function(values) {
  ## Only take non-NA values > 0. We have both "abstain" and "none" as scoring
  ## options and ideally they'd both be zero, but shiny won't let both options
  ## have the same value. Saving NAs to the Synapse table doesn't appear to be
  ## working so we might have to be hacky and make "abstain" == -1.
  values <- stats::na.omit(values[values > 0])
  if (length(values) == 0) {
    return(0)
  } else {
    return(prod(values)^(1 / length(values)))
  }
}

#' Pull latest review table
#'
#' Pull latest review table from Synapse and calculate weighted scores based on
#' the reviewers' scores, clinical/preclinical modifiers, partial beta weights,
#' species, etc.
#'
#' @seealso [calculate_scores_rowwise()]
#' @inheritParams mod_panel_section_server
#' @return Data frame containing the reviewers' scores, comments, and calculated
#'   weighted score (columns will be "ROW_ID", "ROW_VERSION", "form_data_id",
#'   "submission", "scorer", "score", "comments", "species", "clinical", "step",
#'   "weighted_score").
#' @export
pull_reviews_table <- function(syn, reviews_table, submissions) {
  reviews <- syn$tableQuery(glue::glue("SELECT * FROM {reviews_table}"))
  reviews <- readr::read_csv(reviews$filepath) %>%
    dplyr::mutate(scorer = get_display_name(syn, .data$scorer)) %>%
    dplyr::mutate(form_data_id = as.character(.data$form_data_id)) %>%
    calculate_scores_rowwise(submissions)
}