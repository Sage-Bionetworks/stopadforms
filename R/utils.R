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
      display_name <- tryCatch(
        {
          syn$getUserProfile(x)$displayName
        },
        error = function(err) {}
      )
      user_name <- tryCatch(
        {
          syn$getUserProfile(x)$userName
        },
        error = function(err) {}
      )
      display_name %||% user_name
    }
  )
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