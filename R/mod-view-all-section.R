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
            "In Review",
            "Accepted",
            "Rejected"
          ),
          selected = "In Review",
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
        6,
        offset = 1,
        reactable::reactableOutput(ns("submissions"))
      )
    )
  )
}

#' @rdname mod_view_all_section
#' @keywords internal
mod_view_all_section_server <- function(input, output, session, synapse, syn,
                                        group, section_lookup_table,
                                        variable_lookup_table) {

  observeEvent(input$select_status, {
    dccvalidator::with_busy_indicator_server("select_status", {
        submissions <- get_submissions(
          syn,
          group,
          input$status,
          section_lookup_table,
          variable_lookup_table
        )
        output$submissions <- reactable::renderReactable({
          reactable::reactable(
            submissions,
            groupBy = c("submission", "form_data_id", "step"),
            searchable = TRUE,
            highlight = TRUE,
            columns = list(
              submission = reactable::colDef(
                name = "Submission",
                aggregate = "unique"
              ),
              form_data_id = reactable::colDef(name = "Submission ID"),
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
