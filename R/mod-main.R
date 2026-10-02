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

  memb <- NULL
  user <- NULL
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
    message("Login error: ", conditionMessage(err))
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
  
  ## Each step below returns the error (instead of a result) if it fails, in
  ## which case the error is shown on the loading screen and startup stops, so
  ## that later steps never run on incomplete data.
  sub_files <- tryCatch({
    get_submissions(syn, group = 9, statuses = "SUBMITTED_WAITING_FOR_REVIEW")
  }, error = function(err) {
    show_startup_error(
      "Submission Retrieval Error",
      "There was an error retrieving submission data: ",
      err
    )
    err
  })
  if (inherits(sub_files, "error")) {
    return(invisible())
  }
  
  ## Submissions that can't be processed are skipped; their IDs are kept so
  ## that users can be told which ones are missing.
  sub_data <- tryCatch({
    process_submissions(sub_files, lookup_table)
  }, error = function(err) {
    show_startup_error(
      "Submission Processing Error",
      "There was an error processing submission data: ",
      err
    )
    err
  })
  if (inherits(sub_data, "error")) {
    return(invisible())
  }
  failed_ids <- attr(sub_data, "failed_ids")
  failed_names <- attr(sub_data, "failed_names")
  
  sub_metadata <- tryCatch({
    synapseforms::get_submissions_metadata(
      syn = syn,
      group = 9
    ) %>%
      dplyr::select(
        form_data_id = formDataId,
        submitted_on = submissionStatus_submittedOn
      )
  }, error = function(err) {
    show_startup_error(
      "Submission Metadata Processing Error",
      "There was an error processing submission metadata: ",
      err
    )
    err
  })
  if (inherits(sub_metadata, "error")) {
    return(invisible())
  }
  sub_data <- dplyr::left_join(sub_data, sub_metadata, by = "form_data_id")
  
  if (inherits(memb, "check_pass")) {
    ## Show submission data
    review_ok <- tryCatch({
      callModule(mod_review_section_server, "review_section",
                 synapse = synapse, syn = syn, user = user, submissions = sub_data, reviews_table = "syn22014561")
      TRUE
    }, error = function(err) {
      show_startup_error(
        "Submission Display Error",
        "There was an error displaying submission data for scoring: ",
        err
      )
      FALSE
    })
    if (!review_ok) {
      return(invisible())
    }
  }
  
  panel_ok <- tryCatch({
    callModule(mod_panel_section_server, "panel_section",
               synapse = synapse, syn = syn, user = user, submissions = sub_data, 
               reviews_table = "syn22014561", submissions_table = "syn22213241",
               partial_betas = partial_betas)
    TRUE
  }, error = function(err) {
    show_startup_error(
      "Summarized Scores Display Error",
      "There was an error displaying summarized scores data: ",
      err
    )
    FALSE
  })
  if (!panel_ok) {
    return(invisible())
  }
  
  view_all_ok <- tryCatch({
    callModule(mod_view_all_section_server, "view_all_section",
               synapse = synapse, syn = syn, group = 9, lookup_table = lookup_table,
               sub_metadata = sub_metadata)
    TRUE
  }, error = function(err) {
    show_startup_error(
      "View All Submissions Display Error",
      "There was an error displaying submission data: ",
      err
    )
    FALSE
  })
  if (!view_all_ok) {
    return(invisible())
  }
  
  ## Everything loaded; hide the loading screen and flag any skipped submissions
  Sys.sleep(2)
  waiter::waiter_hide()
  show_failed_submissions_notice(failed_ids, failed_names)
}

#' @title Show a startup error on the loading screen
#'
#' @description Log an error that occurred while the app was starting up, and
#' show it on the (still visible) waiter loading screen.
#'
#' @noRd
#' @param title Heading for the error message
#' @param description Text to show before the error itself
#' @param err The error condition
show_startup_error <- function(title, description, err) {
  message(title, ": ", conditionMessage(err))
  Sys.sleep(2)
  waiter::waiter_update(
    html = tagList(
      img(src = "www/synapse_logo.png", height = "120px"),
      h3(title),
      span(
        paste0(description, conditionMessage(err),
               "\n\n Please refresh this page. If the problem persists, contact an administrator."
        )
      )
    )
  )
}
