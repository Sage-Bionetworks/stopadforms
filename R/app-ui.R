#' @import shiny
app_ui <- function(request) {
  shinyjs::useShinyjs()
  
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
    tags$script(htmlwidgets::JS("setTimeout(function(){history.pushState({}, 'STOP-AD submission reviewer', window.location.pathname);},2000);")),
    
    tags$style(HTML("
      .rt-search {
        align-self: flex-start;
        margin-top: -33px;
      }
    ")),
    
    # Ensure that the user cannot type the letter 'e' in the overall score input
    tags$script(HTML("
      $(document).on('keydown', '#main-panel_section-reviewed_overall_score', function (e) {
        if (e.key === 'e' || e.key === 'E') {
          e.preventDefault();
        }
      });
    "))
  )
}
