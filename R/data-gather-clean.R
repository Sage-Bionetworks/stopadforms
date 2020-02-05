#' Get the submissions based on status
#'
#' Get the submissions based on status.
#'
#' @inheritParams mod_review_section_server
#' @param statuses A character vector of statuses to
#'   include from the set: `SUBMITTED_WAITING_FOR_REVIEW`,
#'   `ACCEPTED`, `REJECTED`.
#' @param group The number for a specific Synapse forms group.
get_submissions <- function(syn, group, statuses, lookup_table) {
  if (is.null(statuses)) {
    return(NULL)
  }
  sub_list <- purrr::flatten(purrr::map(statuses, function(x) {
    synapseforms::download_all_submissions_temp(
      syn = syn,
      state_filter = x,
      group = group
    )
  }))
  if (all(is.null(unlist(sub_list)))) {
    return(NULL)
  }

  ## Main table creation, along with submission name
  all_subs <- purrr::map2_dfr(
    sub_list,
    names(sub_list),
    function(filename, data_id) {
      data <- jsonlite::fromJSON(filename, simplifyVector = FALSE)
      sub <- purrr::imap_dfr(data, create_table_by_sections)
      sub <- tibble::add_column(sub, form_data_id = data_id)
      # Form submission name
      user_name <- sub$response[which(sub$variable == "last_name")]
      compound_name <- sub$response[which(sub$variable == "compound_name")]
      sub <- tibble::add_column(
        sub,
        submission = glue::glue("{user_name} - {compound_name}")
      )
      sub
    }
  )
  ## Remove metadata section
  metadata_indices <- which(all_subs$section == "metadata")
  if (length(metadata_indices) > 0) {
    all_subs <- all_subs[-metadata_indices, ]
  }
  ## Add columns 'step' and 'label', which contain user-friendly display names
  ## (including the experiment number) for the sections and variables
  all_subs <- map_sections_variables(all_subs, lookup_table)
  ## Fix logical responses
  all_subs <- change_logical_responses(all_subs)
  ## Don't need all the columns
  all_subs <- all_subs[, c(
    "submission",
    "form_data_id",
    "step",
    "label",
    "response"
  )]

  all_subs
}

#' Create table for a section
#'
#' Create table for a section. Nested lists in the data will
#' be unnested into separate rows.
#'
#' @param data A list containing data from one section of a submission
#' @param section The section name
create_table_by_sections <- function(data, section) {
  # If no data, return NULL
  if (length(data) == 0) {
    return(NULL)
  } else if (length(names(data)) == 1 && names(data) == "experiments") {
    # If "experiments" is the only element, we need to go deepr to extract
    # the info for each experiment separately. The section name needs to
    # have a number to differentiate.
    dat <- purrr::imap_dfr(
      data[["experiments"]],
      function(data, index) {
        create_table_from_values(
          data = data,
          section = section,
          exp_num = index
        )
      }
    )
  } else {
    # If the section does not contain separate experiments, return the data
    # from the section
    dat <- create_table_from_values(
      data = data,
      section = section
    )
  }
  dat
}

#' Create a tibble from the values in section
#'
#' Create a tibble with section name, experiment number, variables, and
#' response values.
#'
#' @inheritParams create_table_by_sections
#' @param exp_num Numeric experiment number
create_table_from_values <- function(data, section, exp_num = NA) {
  tibble::tibble(
    section = section,
    exp_num = exp_num,
    variable = names(unlist(data)),
    response = as.character(unlist(data))
  )
}

#' Change logical responses
#'
#' Change logical responses TRUE/FALSE to Yes/No. Additionally, need to handle
#' the variable "is_solution" which sometimes has 0/1 instead of TRUE/FALSE.
#'
#' @param data Dataframe with response column and variable column.
change_logical_responses <- function(data) {
  false_responses <- c(
    which(data$response == "FALSE"),
    which(data$response[which(data$variable == "is_solution")] == "0")
  )
  true_responses <- c(
    which(data$response == "TRUE"),
    which(data$response[which(data$variable == "is_solution")] == "1")
  )
  if (length(false_responses) > 0) {
    data$response[false_responses] <- "No"
  }
  if (length(true_responses) > 0) {
    data$response[true_responses] <- "Yes"
  }
  data
}

#' Append user-friendly section and variable names
#'
#' Appends columns "step" and "label", which corresponds with "section" and
#' "variable". Map via lookup_table and fix missing step/label. Appends
#' the experiment number to the correct step.
#'
#' @param data Dataframe with columns "section", "variable", and "exp_num".
#' @inheritParams get_submissions
map_sections_variables <- function(data, lookup_table) {
  ## Add on the user-friendly names for variables/sections
  data <- dplyr::left_join(
    data,
    lookup_table,
    by = c("variable", "section")
  )
  ## Fix variables/sections that don't have mapping
  ## Use variables, as is
  na_labels <- is.na(data$label)
  data$label[na_labels] <- data$variable[na_labels]
  ## Figure out section name and use corresponding name from lookup_table
  na_steps <- which(is.na(data$step))
  for (index in na_steps) {
    section_name <- data$section[index]
    step_name <- unique(
      lookup_table$step[which(lookup_table$section == section_name)]
    )
    data$step[index] <- step_name
  }
  ## Append experiment numbers on step names
  appended_steps <- purrr::map2(
    data$step,
    data$exp_num,
    function(name, num) {
      if (!is.na(num)) {
        name <- glue::glue("{name} [{num}]")
      }
      name
    }
  )
  data$step <- unlist(appended_steps)
  data
}
