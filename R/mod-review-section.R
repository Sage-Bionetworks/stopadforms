#' @title   mod_review_section_ui and mod_review_section_server
#' @description  Review and score the selected section of a submission
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param synapse Synapse client (e.g. output of
#'   `reticulate::import("synapseclient")`)
#' @param syn Synapse client object (e.g. output of `synapse$Synapse()`)
#' @param reviews_table Synapse table that stores the scores and comments
#' @param lookup_table Dataframe with columns "section",
#'   "step", "variable" , and "label" used for user-friendly section and
#'   variable display. "step" maps desired "section" names. "label" maps
#'   desired "variable" names.
#'
#' @rdname mod_review_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
mod_review_section_ui <- function(id) {
  ns <- NS(id)

  tabPanel(
    "Review & score",
    fluidRow(
      column(
        4,
        offset = 1,
        selectInput(
          ns("submission"),
          "Select submission",
          choices = ""
        )
      ),
      column(
        4,
        selectInput(
          ns("section"),
          "Select section",
          choices = ""
        )
      )
    ),

    fluidRow(
      column(
        7,
        offset = 1,
        reactable::reactableOutput(ns("data_section_subset"))
      )
    ),

    fluidRow(
      column(
        4,
        offset = 5,
        selectInput(
          inputId = ns("section_score"),
          label = "Score",
          choices = c(
            "None" = 0,
            "Poor" = 0.1,
            "Fair" = 0.25,
            "Good" = 0.85,
            "Excellent" = 1
          )
        ),
        textAreaInput(
          inputId = ns("section_comments"),
          label = "Comments"
        ),
        dccvalidator::with_busy_indicator_ui(
          actionButton(
            inputId = ns("submit"),
            label = "Submit"
          )
        )
      )
    )
  )
}

#' @rdname mod_review_section
#' @keywords internal
mod_review_section_server <- function(input, output, session, synapse, syn,
                                      user, reviews_table, lookup_table) {
  # Get submission data in nice table for viewing
  sub_data <- get_submissions(
    syn,
    group = 9,
    statuses = "SUBMITTED_WAITING_FOR_REVIEW"
  )
  sub_data <- process_submissions(sub_data, lookup_table)

  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c(
      "",
      get_submission_list(sub_data)
    )
  )

  observeEvent(input$submission, {
    if (input$submission != "") {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "section",
        choices = c(
          "",
          get_sections(
            sub_data,
            input$submission
          )
        )
      )
    }
  })

  submission_id <- reactive({
    input$submission
  })
  submission_name <- reactive({
    sub_data$submission[sub_data$form_data_id == input$submission][1]
  })
  section <- reactive({
    input$section
  })

  ## Show section
  to_show <- reactive({
    sub_section <- dplyr::filter(
      sub_data,
      .data$form_data_id == submission_id() & .data$step == section()
    ) %>%
      dplyr::mutate(
        response = dplyr::case_when(
          label == "What is the therapeutic approach?" & response == "both" ~
            "prophylactic, symptomatic",
          TRUE ~ response
        )
      )
    sub_section[c("label", "response")]
  })

  output$data_section_subset <- reactable::renderReactable({
    reactable::reactable(
      to_show(),
      pagination = FALSE,
      columns = list(
        label = reactable::colDef(name = "Label"),
        response = reactable::colDef(name = "Response")
      )
    )
  })
  certified <- dccvalidator::check_certified_user(user$ownerId, syn = syn)

  ## Save new row to table
  observeEvent(input$submit, {
    dccvalidator::with_busy_indicator_server("submit", {
      validate(
        need(
          inherits(certified, "check_pass"),
          HTML("You must be a Synapse Certified User to save reviews. <a href=\"https://docs.synapse.org/articles/accounts_certified_users_and_profile_validation.html\">Learn more</a>")
        )
      )
      if (input$submission == "" || input$section == "") {
        stop("Please select a submission and section")
      }
      result <- readr::read_csv(
        syn$tableQuery(
          glue::glue(
            "SELECT * FROM {reviews_table} WHERE (scorer = {syn$getUserProfile()$ownerId} AND formDataId = {submission_id()} AND section = '{input$section}')" # nolint
          )
        )$filepath
      )
      if (nrow(result) == 0) {
        new_row <- data.frame(
          formDataId = submission_id(),
          submission = submission_name(),
          section = input$section,
          scorer = syn$getUserProfile()$ownerId,
          score = input$section_score,
          comments = input$section_comments,
          stringsAsFactors = FALSE
        )
      } else if (nrow(result) == 1) {
        new_row <- result
        new_row$score <- input$section_score
        new_row$comments <- input$section_comments
      } else {
        stop("Unable to update score: duplicate scores were found for this section from a single reviewer") # nolint
      }
      syn$store(synapse$Table(reviews_table, new_row))
      shinyjs::reset("section_score")
      shinyjs::reset("section_comments")
    })
  })
}

#' Get list of submissions with ids
#'
#' Get list of submissions with their form_data_id.
#'
#' @inheritParams get_sections
#' @return named list where names are the submission names
#'   and values are their form_data_ids.
get_submission_list <- function(data) {
  sub_ids <- as.list(unique(data$form_data_id))
  sub_names <- purrr::map(
    sub_ids,
    function(x) {
      data$submission[data$form_data_id == x][1]
    }
  )
  names(sub_ids) <- sub_names
  sub_ids
}

#' Get submission sections
#'
#' Get submission steps, which can be thought of
#' as the main sections.
#'
#' @param data The submission data as given by
#'   [get_submissions()].
#' @param submission_id Submission form_data_id.
get_sections <- function(data, submission_id) {
  submission <- data[which(data$form_data_id == submission_id), ]
  steps <- unique(submission$step)
  metadata_index <- which(steps == "metadata")
  if (length(metadata_index) > 0) {
    steps <- steps[-metadata_index]
  }
  reorder_steps(steps)
}
