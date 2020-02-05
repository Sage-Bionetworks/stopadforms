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
        HTML("You must log in to <a href=\"https://www.synapse.org/\">Synapse</a> to use this application. Please log in, and then refresh this page.") # nolint
      )
    )
  })

  observeEvent(input$cookie, {
    ## Log in to Synapse
    syn$login(sessionToken = input$cookie)

    ## Lookup tables for variables
    ## Used in giving user-friendly labels on variables/sections
    lookup <- syn$get("syn21557275")
    lookup_table <- utils::read.csv(
      lookup$path,
      stringsAsFactors = FALSE
    )

    ## Show submission data
    callModule(
      mod_review_section_server,
      "review_section",
      synapse = synapse,
      syn = syn,
      reviews_table = "syn21314955",
      lookup_table = lookup_table
    )

    callModule(
      mod_panel_section_server,
      "panel_section",
      synapse = synapse,
      syn = syn,
      reviews_table = "syn21314955",
      submissions_table = "syn21447678"
    )

    callModule(
      mod_view_all_section_server,
      "view_all_section",
      synapse = synapse,
      syn = syn,
      group = 9,
      lookup_table = lookup_table
    )
  })
}
