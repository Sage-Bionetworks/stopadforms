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
                                        group) {

  observeEvent(input$select_status, {
    dccvalidator::with_busy_indicator_server("select_status", {
        submissions <- get_submissions(syn, group, input$status)
        output$submissions <- reactable::renderReactable({
          reactable::reactable(
            submissions,
            groupBy = c("form_data_id", "section"),
            searchable = TRUE,
            highlight = TRUE,
            columns = list(
              section = reactable::colDef(
                name = "Section",
                aggregate = reactable::JS(
                  "function(values, rows) { return '...' }"
                )
              ),
              variable = reactable::colDef(
                name = "Variable",
                aggregate = reactable::JS(
                  "function(values, rows) { return '...' }"
                )
              ),
              sub_variable = reactable::colDef(
                name = "Sub_Variable",
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

#' Get the submissions based on status
#'
#' Get the submissions based on status.
#'
#' @inheritParams mod_view_all_section
#' @param statuses A character vector of statuses to
#'   include from the set: "Accepted", "Rejected",
#'   "In Review".
get_submissions <- function(syn, group, statuses) {
  if (is.null(statuses)) {
    return(NULL)
  }
  statuses <- purrr::map(
    statuses,
    function(x) {
      switch(
        x,
        Accepted = "ACCEPTED",
        Rejected = "REJECTED",
        `In Review` = "SUBMITTED_WAITING_FOR_REVIEW"
      )
    }
  )
  submissions <- synapseforms::download_all_and_get_table(
    syn, state_filter = statuses[[1]], group = group
  )
  if (length(statuses) > 1) {
    for (status in 2:length(statuses)) {
      temp_subs <- synapseforms::download_all_and_get_table(
        syn, state_filter = statuses[[status]], group = group
      )
      if (is.null(submissions)) {
        submissions <- temp_subs
      } else if (!is.null(temp_subs)) {
        submissions <- dplyr::full_join(
          submissions,
          temp_subs, by = "variables"
        )
      }
    }
  }
  if (!is.null(submissions)) {
    submissions <- synapseforms::make_tidier_table(submissions)
    submissions <- clean_experiment_variables(submissions)
    submissions <- submissions[!is.na(submissions$response), ]
  }
  submissions
}
