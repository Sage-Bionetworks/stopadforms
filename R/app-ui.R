#' @import shiny
app_ui <- function() {

  # Submissions
  submissions <- list("123-ABC", "456-DEF", "789-GHI")

  # Sections
  sections <- list("1a", "1b", "2", "3")

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # List the first level UI elements here
    navbarPage(
      "STOP-AD submission reviewer",
      mod_review_section_ui(
        "review_section"
      ),
      mod_panel_section_ui(
        "panel_section",
        submissions = submissions,
        sections = sections
      )
    )
  )
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "stopadforms")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$script(src = "www/readCookie.js")
  )
}
