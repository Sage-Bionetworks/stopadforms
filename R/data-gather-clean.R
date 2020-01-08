#' Get the submissions based on status
#'
#' Get the submissions based on status.
#'
#' @inheritParams mod_review_section_server
#' @param statuses A character vector of statuses to
#'   include from the set: "Accepted", "Rejected",
#'   "In Review".
#' @param group The number for a specific Synapse forms group.
get_submissions <- function(syn, group, statuses,
                            section_lookup_table, variable_lookup_table) {
  if (is.null(statuses)) {
    return(NULL)
  }
  submissions <- purrr::map(statuses, function(x) {
    synapseforms::download_all_and_get_table(
      syn = syn,
      state_filter = x,
      group = group
    )
  }) %>%
    purrr::compact() %>% # Removes NAs
    purrr::reduce(dplyr::full_join, by = "variables")

  if (!is.null(submissions)) {
    submissions <- make_clean_table(
      submissions,
      section_lookup_table,
      variable_lookup_table
    )
  }
  submissions
}

#' Make clean submissions table
#'
#' Make submissions table clean by fixing experiment
#' numbering and adding in user-friendly step and label
#' columns instead of json variable styled section and
#' variable columns.
#'
#' @keywords internal
#' @inheritParams get_submissions
#' @inheritParams clean_experiment_variables
make_clean_table <- function(data, section_lookup_table,
                             variable_lookup_table) {
  data <- synapseforms::make_tidier_table(data)
  data <- data[!is.na(data$response), ]
  data <- dplyr::left_join(data, section_lookup_table, by = "section")
  data <- clean_experiment_variables(data)
  data <- dplyr::left_join(data, variable_lookup_table, by = "variable")
  # Some labels/steps will be blank if form changes or not mapped
  na_labels <- which(is.na(data$label))
  data$label[na_labels] <- data$variable[na_labels]
  na_steps <- which(is.na(data$step))
  data$step[na_steps] <- data$section[na_steps]
  # For now, it would be easier to put a "fake" column for
  # submission name until we get user-friendly names.
  # Currently considering form_data_id the name.
  data <- tibble::add_column(data, submission = data$form_data_id)
  data <- data[c(
    "form_data_id",
    "submission",
    "step",
    "label",
    "response"
  )]
  data
}

#' Clean experiment variables
#'
#' Multiple experiments lead to `variable`s with names of the
#' form "age_range1", "age_range2". This will remove the number at
#' the end of `variable` and append to the `step` name.
#' Note that this only happens if the number is > 0. This is due
#' to having one known `variable`, ID50, that ends in a number,
#' but is not related to the multiple experiment issue.
#'
#' @param data The submission data in the form given by
#'   [synapseforms::make_tidier_table], plus columns step and section.
clean_experiment_variables <- function(data) {
  # Basic section can have multiple routes, but if split first,
  # these ones won't get messed up due to not being tertiary to section.
  data <- tidyr::separate(
    data,
    col = "variable",
    sep = "[.]",
    into = c("main_variable", "sub_variable"),
    remove = FALSE,
    extra = "merge",
    fill = "right"
  )
  num_list <- purrr::map(
    data$sub_variable,
    function(x) {
      last_num <- unlist(stringr::str_extract_all(x, "[:digit:]+"))
      if (length(last_num) > 0 && !is.na(last_num)) {
        # If the variable contains "d50", then is most likely ld50 or ed50
        if (stringr::str_detect(x, "(ld|ed)50")) {
          if (as.numeric(last_num) > 50) {
            # Must be multiple experiments
            last_num <- stringr::str_replace(last_num, "^50", "")
          } else {
            # Single experiment; signal no name change
            last_num <- NA
          }
        }
      } else {
        # Must not have digits
        last_num <- NA
      }
      last_num
    }
  )
  # Append the experiment number to the step
  data$step <- purrr::map2(data$step, num_list, function(x, y) {
    if (!is.na(y)) {
      glue::glue("{x} [{y}]")
    } else {
      x
    }
  })
  # Remove the experiment number from the variable
  data$variable <- purrr::map2(data$variable, num_list, function(x, y) {
    if (!is.na(y)) {
      stringr::str_replace(x, glue::glue("{y}$"), "")
    } else {
      x
    }
  })
  # Fix list column problem
  data$step <- as.character(data$step)
  data$variable <- as.character(data$variable)
  # No longer need the main_variable and sub_variable columns
  data <- data[
    c("form_data_id", "step", "section", "variable", "response")
  ]
  data
}
