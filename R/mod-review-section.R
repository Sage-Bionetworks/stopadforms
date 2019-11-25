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
#'
#' @rdname mod_review_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList 
mod_review_section_ui <- function(id) {
  ns <- NS(id)

  # Submissions
  submissions <- list("123-ABC", "456-DEF", "789-GHI")

  # Sections
  sections <- list("1a", "1b", "2", "3")

  tabPanel(
    "Review & score",
    fluidRow(
      column(
        4,
        offset = 1,
        selectInput(
          ns("submission"),
          "Select submission",
          choices = submissions,
          selected = "123-ABC"
        )
      ),
      column(
        4,
        selectInput(
          ns("section"),
          "Select section",
          choices = sections,
          selected = "1a"
        )
      )
    ),

    fluidRow(
      column(
        8,
        reactable::reactableOutput(ns("data_section_subset"))
      )
    ),

    fluidRow(
      column(
        4,
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
mod_review_section_server <- function(input, output, session, synapse, syn) {
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
      new_row <- data.frame(
        submission = input$submission,
        section = input$section,
        scorer = syn$getUserProfile()$ownerId,
        score = input$section_score,
        comments = input$section_comments,
        stringsAsFactors = FALSE
      )
      syn$store(synapse$Table("syn21314955", new_row))
    })
  })
}
