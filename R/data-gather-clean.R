#' Get the submissions based on status
#'
#' Get the submissions based on status. JSON files are downloaded to a temp
#' directory whose path is returned, along with the submission's form data ID.
#'
#' @param statuses A character vector of statuses to include from the set:
#'   `SUBMITTED_WAITING_FOR_REVIEW`, `ACCEPTED`, `REJECTED`.
#' @param group The number for a specific Synapse forms group.
#' @inheritParams mod_review_section_server
#' @return A list of file paths to JSON files containing the submissions that
#'   have the requested status.
#' @importFrom rlang .data
#' @export
get_submissions <- function(syn, group, statuses) {
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
  } else {
    return(json_file_paths)
  }
}

#' Process submissions
#'
#' Process JSON files into a single table containing all submissions. Cleans up
#' the data to provide user-friendly variable and section names, and remove the
#' `metadata` section.
#'
#' @param submissions A named list of paths to JSON files, i.e. the output of
#'   [get_submissions()]. The name of each element should be its form data ID.
#' @param lookup_table Dataframe with columns "section",
#'   "step", "variable" , and "label" used for user-friendly section and
#'   variable display. "step" maps desired "section" names. "label" maps
#'   desired "variable" names.
#' @param complete If `TRUE`, will join in all section and variable names that
#'   were not provided as part of the submission. If `FALSE`, will only return
#'   the data that was present in the JSON file.
#' @return A data frame containing the combined responses for all submissions
#'   provided to the `submissions` argument
#' @export
#' @importFrom rlang .data
process_submissions <- function(submissions, lookup_table, complete = TRUE) {
  if (is.null(submissions)) {
    stop("No submissions to process", call. = FALSE)
  }
  ## Main table creation, along with submission name. Suppress warnings about
  ## vectorizing 'glue' attributes.
  suppressWarnings(
    all_subs <- purrr::map2_dfr(
      submissions,
      names(submissions), # this is the form data ID
      ~ create_table_from_json_file(
        .x,
        .y,
        lookup_table = lookup_table,
        complete = complete
      )
    )
  )

  ## Remove metadata section
  all_subs <- dplyr::filter(all_subs, .data$section != "metadata") %>%
    ## Fix display of some responses
    change_logical_responses() %>%
    therapeutic_approach_response()
  all_subs
}


#' Create table from JSON file
#'
#' Convert a JSON file containing submission data to a data frame.
#'
#' @param filename Path to JSON file
#' @param data_id Data file handle ID
#' @inheritParams process_submissions
#' @export
create_table_from_json_file <- function(filename, data_id, lookup_table,
                                        complete = TRUE) {
  ## Load JSON
  data <- jsonlite::fromJSON(filename, simplifyVector = FALSE)

  ## Iterate over list of sections to create data frame
  sub <- purrr::imap_dfr(
    data,
    create_section_table,
    lookup_table = lookup_table,
    complete = complete
  )

  ## Add unanswered sections, append experiment numbers to section names
  sub <- map_names(sub, lookup_table = lookup_table, complete = complete) %>%
    append_exp_nums()

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
#' @inheritParams process_submissions
create_section_table <- function(data, section, lookup_table, complete = TRUE) {
  # If no data, return NULL
  if (length(data) == 0) {
    return(NULL)
  } else if (length(names(data)) == 1 && names(data) %in% c("experiments", "cell_line_efficacy", "cell_line_binding")) { # nolint
    # If "experiments" is the only element, we need to go deeper to extract the
    # info for each experiment separately. The section name needs to have a
    # number to differentiate.
    dat <- purrr::imap_dfr(
      data[[1]],
      function(data, index) {
        create_values_table(
          data = data,
          section = section,
          exp_num = index,
          lookup_table = lookup_table,
          complete = complete
        )
      }
    )
  } else {
    # If the section does not contain separate experiments, return the data
    # from the section
    dat <- create_values_table(
      data = data,
      section = section,
      exp_num = NA,
      lookup_table = lookup_table,
      complete = complete
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
create_values_table <- function(data, section, lookup_table,
                                complete = TRUE, exp_num = NA) {
  ## Combine multiple routes into comma-separated single response so we can
  ## later join in variable names
  data <- combine_route_responses(data)
  dat <- tibble::tibble(
    section = section,
    variable = names(unlist(data)),
    response = as.character(unlist(data))
  )
  if (isTRUE(complete)) {
    dat <- add_section_variables(dat, lookup_table = lookup_table)
  }
  ## Add experiment number
  dplyr::mutate(dat, exp_num = exp_num)
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

#' Add unanswered questions within a section.
#'
#' Uses `lookup_table` data to add in questions within a section that were
#' unanswered (and therefore missing from the original JSON data).
#'
#' @param data Dataframe with columns "section", "variable", and "exp_num".
#' @inheritParams process_submissions
add_section_variables <- function(data, lookup_table) {
  ## Filter lookup table to current section
  lookup <- dplyr::filter(
    lookup_table,
    .data$section %in% data$section
  ) %>%
    dplyr::select(.data$variable, .data$section)
  dplyr::full_join(data, lookup, by = c("variable", "section"))
}

#' Append user-friendly section and variable names
#'
#' Appends columns "step" and "label", which corresponds with "section" and
#' "variable". Map via lookup_table and fix missing step/label.
#'
#' @param data Dataframe with columns "section", "variable", and "exp_num".
#' @inheritParams process_submissions
map_names <- function(data, lookup_table, complete = TRUE) {
  join_to_use <- ifelse(complete, dplyr::full_join, dplyr::left_join)
  ## First join in section names. This join is done in 2 steps because the
  ## variables sometimes are missing from the lookup table (due to having
  ## numbers appended to them -- e.g. route1, route2). If we join all at once,
  ## then both step & label are NA and we have to go back and get step labels.
  dat <- dplyr::left_join(
    data,
    unique(lookup_table[, c("section", "step")]),
    by = "section"
  )
  dat <- join_to_use(
    dat,
    lookup_table,
    by = c("section", "variable")
  )
  ## Keep original section/variable names if there's no mapping
  dat %>%
    dplyr::mutate(
      step = dplyr::coalesce(.data$step.x, .data$step.y, .data$section),
      label = dplyr::coalesce(.data$label, .data$variable)
    ) %>%
    dplyr::select(-.data$step.x, -.data$step.y)
}

#' Append experiment numbers to step name
#'
#' When data has multiple experiments, appends the experiment number to each
#' section, e.g. `LD50 [1]`, `LD50 [2]`, etc.
#'
#' @param data Data frame containing submission data
append_exp_nums <- function(data) {
  dplyr::mutate(
    data,
    step = dplyr::case_when(
      !is.na(exp_num) ~ as.character(glue::glue("{step} [{exp_num}]")),
      TRUE ~ step
    )
  )
}

#' Rename response "both" to "prophylactic, symptomatic" in therapeutic approach
#'
#' @inheritParams append_exp_nums
therapeutic_approach_response <- function(data) {
  dplyr::mutate(
    data,
    response = dplyr::case_when(
      variable == "therapeutic_approach" & response == "both" ~
      "prophylactic, symptomatic",
      TRUE ~ response
    )
  )
}

#' Combine multiple routes into one comma-separated response
#'
#' @param data List containing route data
combine_route_responses <- function(data) {
  if ("route" %in% names(data)) {
    data$route <- paste(data$route, collapse = ", ")
  }
  data
}
