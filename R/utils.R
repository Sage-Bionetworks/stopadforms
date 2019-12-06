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
