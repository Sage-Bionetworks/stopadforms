#' Get the submissions based on status
#'
#' Get the submissions based on status.
#'
#' @inheritParams mod_review_section_server
#' @param statuses A character vector of statuses to
#'   include from the set: `SUBMITTED_WAITING_FOR_REVIEW`,
#'   `ACCEPTED`, `REJECTED`.
#' @param group The number for a specific Synapse forms group.
#' @importFrom rlang .data
get_submissions <- function(syn, group, statuses, lookup_table) {
  if (is.null(statuses)) {
    return(NULL)
  }
  json_file_paths <- purrr::flatten(
    purrr::map(statuses, function(x) {
      synapseforms::download_all_submissions_temp(
        syn = syn,
        state_filter = x,
        group = group
      )
    })
  )
  if (all(is.null(unlist(json_file_paths)))) {
    return(NULL)
  }

  ## Main table creation, along with submission name. Suppress warnings about
  ## vectorizing 'glue' attributes.
  suppressWarnings(
    all_subs <- purrr::map2_dfr(
      json_file_paths,
      names(json_file_paths), # this is the form data ID
      ~ create_table_from_json_file(.x, .y)
    )
  )
  ## Remove metadata section
  all_subs <- dplyr::filter(all_subs, .data$section != "metadata")
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

#' Create table from JSON file
#'
#' Convert a JSON file containing submission data to a data frame.
#'
#' @param filename Path to JSON file
#' @param data_id Data file handle ID
create_table_from_json_file <- function(filename, data_id) {
  ## Load JSON
  data <- jsonlite::fromJSON(filename, simplifyVector = FALSE)

  ## Iterate over list of sections to create data frame
  sub <- purrr::imap_dfr(data, create_section_table)

  ## Add form data ID and sub name
  user_name <- sub[sub$variable == "last_name", "response", drop = TRUE]
  compound_name <- sub[sub$variable == "compound_name", "response", drop = TRUE]
  sub %>%
    dplyr::mutate(form_data_id = data_id) %>%
    dplyr::mutate(submission = glue::glue("{user_name} - {compound_name}"))
}

#' Create table for a section
#'
#' Create table for a section of a submission. Some sections contain multiple
#' experiments nested within them; if this is the case, they will be unnested
#' and given an experiment number.
#'
#' @param data A list containing data from one section of a submission
#' @param section The section name
create_section_table <- function(data, section) {
  # If no data, return NULL
  if (length(data) == 0) {
    return(NULL)
  } else if (length(names(data)) == 1 && names(data) == "experiments") {
    # If "experiments" is the only element, we need to go deeper to extract the
    # info for each experiment separately. The section name needs to have a
    # number to differentiate.
    dat <- purrr::imap_dfr(
      data[["experiments"]],
      function(data, index) {
        create_values_table(
          data = data,
          section = section,
          exp_num = index
        )
      }
    )
  } else {
    # If the section does not contain separate experiments, return the data
    # from the section
    dat <- create_values_table(
      data = data,
      section = section,
      exp_num = NA
    )
  }
  dat
}

#' Create a tibble from the values within section
#'
#' Create a tibble with section name, experiment number, variables, and
#' response values.
#'
#' @inheritParams create_section_table
#' @param exp_num Numeric experiment number
create_values_table <- function(data, section, exp_num = NA) {
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
#' @importFrom rlang .data
change_logical_responses <- function(data) {
  dplyr::mutate(
    data,
    response = dplyr::case_when(
      .data$response == "FALSE" ~ "No",
      .data$response == "TRUE" ~ "Yes",
      .data$variable == "is_solution" & .data$response %in% c("0", "FALSE") ~ "No", # nolint
      .data$variable == "is_solution" & .data$response %in% c("1", "TRUE") ~ "Yes", # nolint
      TRUE ~ response
    )
  )
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
  ## First join in section names. This join is done in 2 steps because the
  ## variables sometimes are missing from the lookup table (due to having
  ## numbers appended to them -- e.g. route1, route2). If we join all at once,
  ## then both step & label are NA and we have to go back and get step labels
  data <- dplyr::left_join(
    data,
    unique(lookup_table[, c("section", "step")]),
    by = "section"
  ) %>%
    ## Then add labels
    dplyr::left_join(
      lookup_table[, c("variable", "section", "label")],
      by = c("variable", "section")
    )
  ## Fix variables/sections that don't have mapping
  ## Use variables, as is
  data <- data %>%
    dplyr::mutate(
      label = dplyr::case_when(
        is.na(label) ~ variable,
        TRUE ~ label
      )
    )
  ## Append experiment numbers on step names
  data <- data %>%
    dplyr::mutate(
      step = dplyr::case_when(
        !is.na(exp_num) ~ as.character(glue::glue("{step} [{exp_num}]")),
        TRUE ~ step
      )
    )
  data
}
