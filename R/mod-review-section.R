#' @title   mod_review_section_ui and mod_review_section_server
#' @description  Review and score the selected section of a submission
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param submissions Character vector of available submissions
#' @param sections Character vector of sections within submissions
#' @param synapse Synapse client (e.g. output of
#'   `reticulate::import("synapseclient")`)
#' @param syn Synapse client object (e.g. output of `synapse$Synapse()`)
#' @param reviews_table Synapse table that stores the scores and comments
#'
#' @rdname mod_review_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
mod_review_section_ui <- function(id, submissions, sections) {
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
                                      reviews_table) {
  sub_data <- synapseforms::download_all_and_get_table(syn, group = 9)
 
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c("", synapseforms::get_submission_names(sub_data))
  )

  observeEvent(input$submission, {
    if (input$submission != "") {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "section",
        choices = c("", synapseforms::get_main_sections(
          sub_data,
          input$submission
        ))
      )
    }
  })

  submission <- reactive({ input$submission })
  section <- reactive({ input$section })

  ## Show section
  to_show <- reactive({
    dplyr::filter(
      submission_data,
      submission == submission() & section == section()
    )
  })

  output$data_section_subset <- reactable::renderReactable({
    reactable::reactable(to_show())
  })

  ## Save new row to table
  observeEvent(input$submit, {
    dccvalidator::with_busy_indicator_server("submit", {
      result <- readr::read_csv(
        syn$tableQuery(
          glue::glue(
            "SELECT * FROM {reviews_table} WHERE (scorer = {syn$getUserProfile()$ownerId} AND submission = '{input$submission}' AND section = '{input$section}')"
          )
        )$filepath
      )
      if (nrow(result) == 0 ) {
        new_row <- data.frame(
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
        stop("Unable to update score: duplicate scores were found for this section from a single reviewer")
      }
      syn$store(synapse$Table(reviews_table, new_row))
    })
  })
}
