default_order <- c(
  "Naming", "Measurements", "Basic Data",
  "Binding", "Efficacy", "In Vivo Data",
  "PK In Silico", "PK In Vitro",
  "PK In Vivo", "LD50", "Acute Dosing",
  "Chronic Dosing", "Teratogenicity",
  "Clinical Data"
)

#' Reorder steps
#'
#' Reorder the steps so the appear in the same order as sections in the original
#' form.
#'
#' @param steps Sections in the data; may include numeric suffixes e.g. ` [1]`,
#'   ` [2]` etc. for multiple experiments within a section
#' @param full_order Correct full order of sections (without numeric suffixes)
#' @return A reordered version of `steps` that matches the original order of
#'   sections in the form
#' @noRd
reorder_steps <- function(steps, full_order = default_order) {
  ## Indices of elements in full_order that match the elements in steps
  ix <- purrr::map_dbl(steps, function(x) {
    unsuffixed_name <- stringr::str_remove(x, " \\[\\d\\]")
    index <- stringr::str_which(full_order, glue::glue("^{unsuffixed_name}$"))
    ## If section isn't found, return NA
    if (length(index) == 0) {
      return(NA)
    } else {
      return(index)
    }
  })
  ## Reorder steps
  steps[order(ix)]
}

extract_base_category <- function(category) {
  # Remove anything in brackets to get the base category
  base_category <- sub("\\s*\\[.*\\]", "", category)
  
  # Try to extract the number in brackets, if it exists
  suppressWarnings(suffix <- as.numeric(sub(".*\\[(\\d+)\\]", "\\1", category)))
  
  if (is.na(suffix)) {
    # If the suffix is NA after attempting extraction, set it to zero
    suffix <- 0
  }
  
  return(list(base_category = base_category, suffix = suffix))
}


