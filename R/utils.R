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
      syn$getUserProfile(x)$displayName %||% syn$getUserProfile(x)$userName
    }
  )
}
