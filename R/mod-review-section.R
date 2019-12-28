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
#' @param section_lookup_table Dataframe with columns "section" and
#'   "step" used for user-friendly section names
#' @param section_lookup_table Dataframe with columns "variable" and
#'   "label" used for user-friendly variable names
#'
#' @rdname mod_review_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList
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
        numericInput(
          inputId = ns("section_score"),
          label = "Score",
          value = 1
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
                                      reviews_table, section_lookup_table,
                                      variable_lookup_table) {
  # Get submission data in nice table for viewing
  sub_data <- get_submissions(
    syn,
    group = 9,
    statuses = "In Review",
    section_lookup_table = section_lookup_table,
    variable_lookup_table = variable_lookup_table
  )

  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c(
      "",
      unique(sub_data$submission)
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

  submission <- reactive({
    input$submission
  })
  section <- reactive({
    input$section
  })

  ## Show section
  to_show <- reactive({
    sub_section <- dplyr::filter(
      sub_data,
      submission == submission() & step == section() & !is.na(sub_data$response)
    )
    sub_section[c("label", "response")]
  })

  output$data_section_subset <- reactable::renderReactable({
    reactable::reactable(to_show())
  })

  ## Save new row to table
  observeEvent(input$submit, {
    dccvalidator::with_busy_indicator_server("submit", {
      form_data_id <- sub_data$form_data_id[
        which(sub_data$submission == input$submission)
      ]
      result <- readr::read_csv(
        syn$tableQuery(
          glue::glue(
            "SELECT * FROM {reviews_table} WHERE (scorer = {syn$getUserProfile()$ownerId} AND formDataId = {form_data_id} AND section = '{input$section}')" # nolint
          )
        )$filepath
      )
      if (nrow(result) == 0) {
        new_row <- data.frame(
          formDataId = form_data_id,
          submission = input$submission,
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
    })
  })
}

#' Get submission sections
#'
#' Get submission steps, which can be thought of
#' as the main sections.
#'
#' @param data The submission data as given by
#'   [get_submissions()].
#' @param submission_name Submission name.
get_sections <- function(data, submission_name) {
  submission <- data[which(data$submission == submission_name), ]
  steps <- unique(submission$step)
  steps <- steps[-which(steps == "metadata")]
  steps
}
