#' @import shiny
app_server <- function(input, output, session) {
  submission_data <- readr::read_csv(
    system.file("extdata/sample_data.csv", package = "stopadforms")
  )
  callModule(mod_review_section_server, "review_section", submission_data)
}
