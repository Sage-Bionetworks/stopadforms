#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

get_display_name <- function(syn, id) {
  purrr::map_chr(
    id,
    function(x) {
      display_name <- tryCatch({ syn$getUserProfile(x)$displayName },
        error = function(err) {})
      user_name <- tryCatch({ syn$getUserProfile(x)$userName },
        error = function(err) {})
      display_name %||% user_name
    }
  )
}

#' Clean experiment variables
#'
#' Multiple experiments lead to `sub_variable`s with names of the
#' form "age_range1", "age_range2". This will remove the number at
#' the end of `sub_variable` and append to the `section` name.
#' Note that this only happens if the number is > 0. This is due
#' to having one known `sub_variable`, ID50, that ends in a number,
#' but is not related to the multiple experiment issue.
#'
#' @param data The submission data in the form given by
#'   [synapseforms::make_tidier_table].
clean_experiment_variables <- function(data) {
  num_list <- purrr::map(
    data$sub_variable,
    function(x) {
      suppressWarnings(as.numeric(substr(x, nchar(x), nchar(x))))
    }
  )
  data$section <- purrr::map2(data$section, num_list, function(x, y) {
    if (!is.na(y) && y > 0) {
      glue::glue("{x}_{y}")
    } else {
      x
    }
  })
  data$sub_variable <- purrr::map2(
    data$sub_variable,
    num_list,
    function (x, y) {
      if (!is.na(y) && y > 0) {
        substr(x, 1, nchar(x) - 1)
      } else {
        x
      }
    }
  )
  data
}

#' Format submissions table
#'
#' Format the submissions table to be
#' tidier, have genrated submission names,
#' and cleaned up variables.
#'
#' @param data Submissions table as given by
#'   [synapseforms::download_all_and_get_table()]
format_submissions_table <- function(data) {
  data <- synapseforms::make_tidier_table(data)
  data <- clean_experiment_variables
  data
}
