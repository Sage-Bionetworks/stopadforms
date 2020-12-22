#' @import shiny
app_ui <- function() {

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Add waiter loading screen
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      html = tagList(
        img(src = "www/loading.gif"),
        h4("Connecting to Synapse...")
      ),
      color = "#424874"
    ),

    # List the first level UI elements here
    navbarPage(
      "STOP-AD submission reviewer",
      mod_review_section_ui(
        "review_section"
      ),
      mod_panel_section_ui(
        "panel_section"
      ),
      mod_view_all_section_ui(
        "view_all_section"
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
