#' Get the submissions based on status
#'
#' Get the submissions based on status.
#'
#' @inheritParams mod_review_section_server
#' @param statuses A character vector of statuses to
#'   include from the set: "Accepted", "Rejected",
#'   "In Review".
get_submissions <- function(syn, group, statuses,
                            section_lookup_table, variable_lookup_table) {
  if (is.null(statuses)) {
    return(NULL)
  }
  statuses <- purrr::map(
    statuses,
    function(x) {
      switch(
        x,
        Accepted = "ACCEPTED",
        Rejected = "REJECTED",
        `In Review` = "SUBMITTED_WAITING_FOR_REVIEW"
      )
    }
  )
  submissions <- synapseforms::download_all_and_get_table(
    syn,
    state_filter = statuses[[1]], group = group
  )
  if (length(statuses) > 1) {
    for (status in 2:length(statuses)) {
      temp_subs <- synapseforms::download_all_and_get_table(
        syn,
        state_filter = statuses[[status]], group = group
      )
      if (is.null(submissions)) {
        submissions <- temp_subs
      } else if (!is.null(temp_subs)) {
        submissions <- dplyr::full_join(
          submissions,
          temp_subs,
          by = "variables"
        )
      }
    }
  }
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
  data <- dplyr::full_join(data, add_friendly_names(data), by = "form_data_id")
  data <- dplyr::full_join(data, section_lookup_table, by = "section")
  data <- clean_experiment_variables(data)
  data <- dplyr::full_join(data, variable_lookup_table, by = "variable")
  data <- data[!is.na(data$response), ]
  # Some labels/steps will be blank if form changes or not mapped
  na_labels <- which(is.na(data$label))
  data$label[na_labels] <- data$variable[na_labels]
  na_steps <- which(is.na(data$step))
  data$step[na_steps] <- data$section[na_steps]
  # Only need nice, curated table
  data <- data[c(
    "form_data_id",
    "submission",
    "step",
    "label",
    "response"
  )]
  data <- change_logical_responses(data)
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
#'   [synapseforms::make_tidier_table].
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
      suppressWarnings(as.numeric(substr(x, nchar(x), nchar(x))))
    }
  )
  data$step <- purrr::map2(data$step, num_list, function(x, y) {
    if (!is.na(y) && y > 0) {
      glue::glue("{x} [{y}]")
    } else {
      x
    }
  })
  data$variable <- purrr::map2(data$variable, num_list, function(x, y) {
    if (!is.na(y) && y > 0) {
      substr(x, 1, nchar(x) - 1)
    } else {
      x
    }
  })
  # Fix list column problem
  data$step <- as.character(data$step)
  data$variable <- as.character(data$variable)
  data <- data[, -which(names(data) %in% c("main_variable", "sub_variable"))]
  data
}

#' Change logical responses to yes/no
#'
#' Change TRUE/FALSE responses to be yes/no.
#'
#' @inheritParams clean_experiment_variables
change_logical_responses <- function(data) {
  true_indices <- which(data$response == "TRUE")
  false_indices <- which(data$response == "FALSE")
  data$response[true_indices] <- "Yes"
  data$response[false_indices] <- "No"
  data
}

#' Add user-friendly submission name
#'
#' Add a user-friendly submission name column in the
#' form of the submitter's last name - the
#' compound. Note that these are not guaranteed to be
#' unique and submissions should be referred to by their
#' form_data_id.
#'
#' @inheritParams clean_experiment_variables
#' @return List of submission names
add_friendly_names <- function(data) {
  submission_ids <- synapseforms::get_submission_ids(data)
  friendly_names <- purrr::map(
    submission_ids,
    function(x) {
      last_name <- data$response[intersect(
        which(data$form_data_id == x),
        which(data$variable == "last_name")
      )]
      compound_name <- data$response[intersect(
        which(data$form_data_id == x),
        which(data$variable == "compound_name")
      )]
      name <- glue::glue("{last_name} - {compound_name}")
      name
    }
  )
  names_df <- tibble::tibble(
    submission = as.character(friendly_names),
    form_data_id = submission_ids
  )
  names_df
}
