#' @import shiny
app_ui <- function(request) {
  if (interactive()) {
    ## Running locally; skip OAuth
    mod_main_ui("main")
  } else {
    mod_synapse_oauth_ui(id = "oauth", request = request)
  }
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "stopadforms")
  )

  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$script(src = "www/readCookie.js"),
    tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'STOP-AD submission reviewer', window.location.pathname);},2000);"))
  )
}
