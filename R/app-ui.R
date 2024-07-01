#' @import shiny
app_ui <- function(request) {
  mod_synapse_oauth_ui(id = "oauth", request = request)
}

#' @import shiny
golem_add_external_resources <- function() {
  addResourcePath(
    "www", system.file("app/www", package = "stopadforms")
  )

  #hack fix for refresh error
  tags$head(
    golem::activate_js(),
    golem::favicon(),
    tags$script(src = "www/readCookie.js"),
    tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'STOP-AD submission reviewer', window.location.pathname);},2000);"))
  )
}
