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
        )
      )
    ),
    fluidRow(
      column(
        2,
        offset = 1,
        div(
          style = "display: flex;",
          actionButton(
            ns("select_status"),
            "Get Submissions",
            style = "flex-grow: 1;"
          )
        )
      )
    ),
    br(),
    fluidRow(
      column(2, offset = 1,
             div(
               actionButton(ns("expand_all"), "Expand All"),
               actionButton(ns("collapse_all"), "Collapse All")
             )
      )
    ),
    br(), br(), br(),
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
                                        group, lookup_table, sub_metadata) {
  
  observeEvent(input$expand_all, {
    reactable::updateReactable("submissions", expanded = TRUE, session = getDefaultReactiveDomain())
  })
  
  observeEvent(input$collapse_all, {
    reactable::updateReactable("submissions", expanded = FALSE, session = getDefaultReactiveDomain())
  })
  
  observeEvent(input$select_status, {
    with_busy_indicator_server("select_status", {
      submissions <- get_submissions(
        syn,
        group,
        input$status
      ) %>%
        process_submissions(lookup_table) %>%
        ## Make step an ordered factor
        dplyr::mutate(
          step = factor(
            .data$step,
            levels = reorder_steps(unique(.data$step)),
            ordered = TRUE
          )
        )

      if (is.null(submissions)) {
        stop("No submissions found with requested status(es)")
      }
      
      sub_metadata$submitted_on <- clean_date_strings(sub_metadata$submitted_on)
      
      submissions <- dplyr::left_join(submissions, sub_metadata) %>%
        dplyr::mutate(
          submission = paste0(submitted_on, ": ", submission)
        ) %>%
        ## Arrange by submission and step
        dplyr::arrange(desc(.data$submission), .data$step)
      
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
