#' Add user-friendly submission name
#'
#' Add a user-friendly submission name column in the
#' form of the submitter's last name, the
#' compound, and the formDataId to ensure uniqueness.
#' Setup as "lastname - compound (formDataId)".
#'
#' @inheritParams clean_experiment_variables
#' @return Original data with a submission column
#'   in the second column index, with submission names.
add_friendly_names <- function(data) {
  submission_ids <- synapseforms::get_submission_ids(data)
  friendly_names <- purrr::map(
    submission_ids,
    function (x) {
      last_name <- data$response[intersect(
        which(data$form_data_id == x),
        which(data$variable == "last_name")
      )]
      compound_name <- data$response[intersect(
        which(data$form_data_id == x),
        which(data$variable == "compound_name")
      )]
      name <- glue::glue("{last_name} - {compound_name} ({x})")
      name
    }
  )
  names_df <- tibble::tibble(
    submission = friendly_names,
    form_data_id = submission_ids
  )
  data <- dplyr::full_join(data, names_df, by = "form_data_id")
  # Reorder columns
  data <- data[, c(
    "form_data_id",
    "submission",
    "section",
    "variable",
    "sub_variable",
    "response"
  )]
  data
}