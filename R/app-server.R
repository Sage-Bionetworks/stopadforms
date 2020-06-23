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

    ## Check if user is in STOP-AD_Reviewers team
    team <- "3403721"
    user <- syn$getUserProfile()
    memb <- dccvalidator::check_team_membership(
      teams = team,
      user = user,
      syn = syn
    )
    ## Show message if not in team -- (not using
    ## dccvalidator::report_unsatisfied_requirements() because it might be
    ## usefult to view data even if the user isn't certified)
    if (inherits(memb, "check_fail")) {
      showModal(
        modalDialog(
          title = memb$message,
          tagList(
            p(memb$behavior),
            p("You can request to be added at: "),
            HTML(glue::glue("<a href=\"https://www.synapse.org/#!Team:{team}\">https://www.synapse.org/#!Team:{team}</a>"))
          )
        )
      )
    }

    if (inherits(memb, "check_pass")) {

      ## Get data
      sub_data <- get_submissions(
        syn,
        group = 9,
        statuses = "SUBMITTED_WAITING_FOR_REVIEW"
      )
      sub_data <- process_submissions(sub_data, lookup_table)

      ## Show submission data
      callModule(
        mod_review_section_server,
        "review_section",
        synapse = synapse,
        syn = syn,
        user = user,
        submissions = sub_data,
        reviews_table = "syn22014561"
      )

      callModule(
        mod_panel_section_server,
        "panel_section",
        synapse = synapse,
        syn = syn,
        user = user,
        submissions = sub_data,
        reviews_table = "syn22014561",
        submissions_table = "syn22213241"
      )

      callModule(
        mod_view_all_section_server,
        "view_all_section",
        synapse = synapse,
        syn = syn,
        group = 9,
        lookup_table = lookup_table
      )
    }
  })
}
