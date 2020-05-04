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
#' @param section Section multiplication factor. Usually 1 except for sections
#'   `pk_in_silico`, `pk_in_vitro`, and `pk_in_vivo`.
#' @param clinical Clinical (usually 0.67) or preclinical (usually 0.33)
#'   multiplication factor.
#' @param species Within-species (usually 0.67) or across-species (usually 0.33)
#'   multiplication factor.
#' @param score Average score given by reviewers.
#' @return A numeric value indicating the score for the section
#' @export
calculate_section_score <- function(data, lookup, section = 1,
                                    clinical = 1, species = 1, score = 1) {
  section_name <- unique(data$section)
  print(section_name)
  if (length(section_name) != 1) {
    stop("Can only calculate score for one section at a time.")
  }
  ## Clinical section is not scored
  if (section_name == "clinical") {
    return(0)
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

efficacy_beta <- function(efficacy_measure) {
  switch(
    efficacy_measure,
    EC50 = .67,
    IC50 = .33
  )
}
