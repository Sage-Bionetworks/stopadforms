#' @import shiny
app_server <- function(input, output, session) {
  synapse <- reticulate::import("synapseclient")
  session$sendCustomMessage(type = "readCookie", message = list())
  syn <- synapse$Synapse()

  ## Show message if user is not logged in to synapse
  unauthorized <- observeEvent(input$authorized, {
    showModal(
      modalDialog(
        title = "Not logged in",
        HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.")
      )
    )
  })

  observeEvent(input$cookie, {
    ## Log in to Synapse
    syn$login(sessionToken = input$cookie)

    ## Show submission data
    callModule(
      mod_review_section_server,
      "review_section",
      synapse = synapse,
      syn = syn
    )
  })
}
