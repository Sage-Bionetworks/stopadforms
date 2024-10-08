#' @title Module for main app
#'
#' @noRd
#' @import shiny
mod_main_ui <- function(id) {
  ns <- NS(id)
  
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
      # name for refresh hack
      "STOP-AD submission reviewer",
      mod_review_section_ui(ns("review_section")),
      mod_panel_section_ui(ns("panel_section")),
      mod_view_all_section_ui(ns("view_all_section"))
    )
  )
}

#' @import shiny
mod_main_server <- function(input, output, session, syn) {
  shiny::req(inherits(syn, "synapseclient.client.Synapse") & logged_in(syn))
  
  source('data-raw/lookup_table.R', chdir = TRUE)
  
  # Load in the static data
  load("data/partial_betas.rda")
  load("data/lookup_table.rda")

  tryCatch({
    ## Check if user is in STOP-AD_Reviewers team
    team <- "3403721"
    user <- syn$getUserProfile()
    memb <- check_team_membership(teams = team, user = user, syn = syn)
    
    if (inherits(memb, "check_fail")) {
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          p(memb$behavior),
          p("You can request to be added at: "),
          HTML(glue::glue("<a href=\"https://www.synapse.org/#!Team:{team}\">https://www.synapse.org/#!Team:{team}</a>"))
        )
      )
    } else {
      ### update waiter loading screen once login successful
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3(sprintf("Welcome, %s!", syn$getUserProfile()$userName))
        )
      )
    }
  }, error = function(err) {
    Sys.sleep(2)
    waiter::waiter_update(
      html = tagList(
        img(src = "www/synapse_logo.png", height = "120px"),
        h3("Login error"),
        span(
          "There was an error with the login process. Please refresh your Synapse session by logging out of and back in to",
          a("Synapse", href = "https://www.synapse.org/", target = "_blank"),
          ", then refresh this page. If the problem persists, contact an administrator."
        )
      )
    )
  })
  
  tryCatch({
    ## Get data
    sub_data <- get_submissions(syn, group = 9, statuses = "SUBMITTED_WAITING_FOR_REVIEW")

    Sys.sleep(2)
    waiter::waiter_hide()
  }, error = function(err) {
    Sys.sleep(2)
    waiter::waiter_update(
      html = tagList(
        img(src = "www/synapse_logo.png", height = "120px"),
        h3("Submission Retrieval Error"),
        span(
          paste0("There was an error retrieving submission data: ", err,
                 "\n\n Please refresh this page. If the problem persists, contact an administrator."
          )
        )
      )
    )
  })
  
  tryCatch({
    ## Get data
    sub_data <- process_submissions(sub_data, lookup_table)

    Sys.sleep(2)
    waiter::waiter_hide()
  }, error = function(err) {
    Sys.sleep(2)
    waiter::waiter_update(
      html = tagList(
        img(src = "www/synapse_logo.png", height = "120px"),
        h3("Submission Processing Error"),
        span(
          paste0("There was an error processing submission data: ", err,
                 "\n\n Please refresh this page. If the problem persists, contact an administrator."
          )
        )
      )
    )
  })
  
  tryCatch({
    ## Get data
    sub_metadata <- synapseforms::get_submissions_metadata(
      syn = syn,
      group = 9
    ) %>%
      dplyr::select(
        form_data_id = formDataId,
        submitted_on = submissionStatus_submittedOn
      )
    
    sub_data <- dplyr::left_join(sub_data, sub_metadata)
    
    Sys.sleep(2)
    waiter::waiter_hide()
  }, error = function(err) {
    Sys.sleep(2)
    waiter::waiter_update(
      html = tagList(
        img(src = "www/synapse_logo.png", height = "120px"),
        h3("Submission Metadata Processing Error"),
        span(
          paste0("There was an error processing submission metadata: ", err,
                 "\n\n Please refresh this page. If the problem persists, contact an administrator."
          )
        )
      )
    )
  })
  
  if (inherits(memb, "check_pass")) {
    ## Show submission data
    tryCatch({
      callModule(mod_review_section_server, "review_section",
                 synapse = synapse, syn = syn, user = user, submissions = sub_data, reviews_table = "syn22014561")
    }, error = function(err) {
      Sys.sleep(2)
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3("Submission Display Error"),
          span(
            paste0("There was an error displaying submission data for scoring: ", err,
                   "\n\n Please refresh this page. If the problem persists, contact an administrator."
            )
          )
        )
      )
    })
  }
  
  tryCatch({
    callModule(mod_panel_section_server, "panel_section",
               synapse = synapse, syn = syn, user = user, submissions = sub_data, 
               reviews_table = "syn22014561", submissions_table = "syn22213241",
               partial_betas = partial_betas)
  }, error = function(err) {
    Sys.sleep(2)
    waiter::waiter_update(
      html = tagList(
        img(src = "www/synapse_logo.png", height = "120px"),
        h3("Summarized Scores Display Error"),
        span(
          paste0("There was an error displaying summarized scores data: ", err),
          "\n\n Please refresh this page. If the problem persists, contact an administrator."
        )
      )
    )
  })
  
  tryCatch({
    callModule(mod_view_all_section_server, "view_all_section",
               synapse = synapse, syn = syn, group = 9, lookup_table = lookup_table,
               sub_metadata = sub_metadata)
  }, error = function(err) {
    Sys.sleep(2)
    waiter::waiter_update(
      html = tagList(
        img(src = "www/synapse_logo.png", height = "120px"),
        h3("View All Submissions Display Error"),
        span(
          paste0("There was an error displaying submission data: ", err),
          "\n\n Please refresh this page. If the problem persists, contact an administrator."
        )
      )
    )
  })
}
