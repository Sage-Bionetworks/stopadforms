#' @title   mod_view_all_section_ui and mod_view_all_section_server
#' @description  View and search among all submissions
#'
#' @inheritParams mod_review_section_ui
#' @inheritParams mod_review_section_server
#'
#' @rdname mod_view_all_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
mod_view_all_section_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    "View all submissions",
    fluidRow(
      column(
        6,
        offset = 1,
        checkboxGroupInput(
          ns("status"),
          "Select statuses to include",
          choices = c(
            "In Review" = "SUBMITTED_WAITING_FOR_REVIEW",
            "Accepted" = "ACCEPTED",
            "Rejected" = "REJECTED"
          ),
          selected = "SUBMITTED_WAITING_FOR_REVIEW",
          inline = TRUE
        )
      )
    ),
    fluidRow(
      column(
        4,
        offset = 1,
        dccvalidator::with_busy_indicator_ui(
          actionButton(
            ns("select_status"),
            "Submit Selection"
          )
        )
      )
    ),
    fluidRow(
      column(
        10,
        offset = 1,
        reactable::reactableOutput(ns("submissions"))
      )
    )
  )
}

#' @rdname mod_view_all_section
#' @keywords internal
mod_view_all_section_server <- function(input, output, session, synapse, syn,
                                        group, lookup_table) {

  observeEvent(input$select_status, {
    dccvalidator::with_busy_indicator_server("select_status", {
        submissions <- get_submissions(
          syn,
          group,
          input$status,
          lookup_table
        ) %>%
          ## Make step an ordered factor
          dplyr::mutate(
            step = factor(
              .data$step,
              levels = reorder_steps(unique(.data$step)),
              ordered = TRUE
            )
          ) %>%
          ## Arrange by submission and step
          dplyr::arrange(.data$submission, .data$step)

        if (is.null(submissions)) {
          stop("No submissions found with requested status(es)")
        }
        ## Replace "both" with "prophylactic, symptomatic" in answer to question
        ## "What is the therapeutic approach?"
        submissions <- dplyr::mutate(
          submissions,
          response = dplyr::case_when(
            .data$label == "What is the therapeutic approach?" &
              .data$response == "both" ~
              "prophylactic, symptomatic",
            TRUE ~ response
          )
        )
        output$submissions <- reactable::renderReactable({
          reactable::reactable(
            submissions,
            groupBy = c("submission", "step"),
            searchable = TRUE,
            highlight = TRUE,
            columns = list(
              submission = reactable::colDef(
                name = "Submission",
                aggregate = "unique"
              ),
              form_data_id = reactable::colDef(name = "ID"),
              step = reactable::colDef(
                name = "Section",
                aggregate = reactable::JS(
                  "function(values, rows) { return '...' }"
                )
              ),
              label = reactable::colDef(
                name = "Label",
                aggregate = reactable::JS(
                  "function(values, rows) { return '...' }"
                )
              ),
              response = reactable::colDef(
                name = "Reponse",
                aggregate = reactable::JS(
                  "function(values, rows) { return '...' }"
                )
              )
            )
          )
        })
    })
  })
}
