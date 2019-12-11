#' @title   mod_panel_section_ui and mod_panel_section_server
#' @description  View summarized scores for a submission
#'
#' @inheritParams mod_review_section_ui
#' @inheritParams mod_review_section_server
#'
#' @rdname mod_panel_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
mod_panel_section_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    "View summarized scores",
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
        dccvalidator::with_busy_indicator_ui(
          actionButton(
            ns("refresh_comments"),
            "Refresh Comments"
          )
        )
      )
    ),
    fluidRow(
      column(
        6,
        offset = 1,
        reactable::reactableOutput(ns("averaged_scores"))
      )
    )
  )
}

#' @rdname mod_panel_section
#' @keywords internal
mod_panel_section_server <- function(input, output, session, synapse, syn,
                                     reviews_table) {
  submission <- reactive({ input$submission })

  ## Load reviews
  reviews <- reactive(pull_reviews_table(syn, reviews_table))
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c("", unique(reviews()$submission))
  )
  show_review_table(input, output, reviews, submission)

  observeEvent(input$refresh_comments, {
    dccvalidator::with_busy_indicator_server("refresh_comments", {
      reviews <<- reactive(pull_reviews_table(syn, reviews_table))
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "submission",
        choices = c("", unique(reviews()$submission)),
        selected = submission()
      )
      show_review_table(input, output, reviews, submission)
    })
  })
}

#' @title Pull latest review table
#' 
#' @description Pull latest review table from Synapse.
#' 
#' @inheritParams mod_panel_section_server
#' @keywords internal
pull_reviews_table <- function(syn, reviews_table) {
  reviews <- syn$tableQuery(glue::glue("SELECT * FROM {reviews_table}"))
  reviews <- readr::read_csv(reviews$filepath) %>%
    dplyr::mutate(scorer = get_display_name(syn, .data$scorer))
  reviews
}

#' @title Show review table
#'
#' @description Show review table.
#'
#' @inheritParams mod_panel_section_server
#' @param reviews Dataframe review table.
#' @param submission Submission name.
#' @keywords internal
show_review_table <- function(input, output, reviews, submission) {
  to_show <- reactive({
    dplyr::filter(reviews(), submission == submission()) %>%
      dplyr::select(.data$section, .data$score, .data$scorer, .data$comments)
  })
  
  output$averaged_scores <- reactable::renderReactable({
    reactable::reactable(
      to_show(),
      groupBy = "section",
      searchable = TRUE,
      columns = list(
        section = reactable::colDef(name = "Section"),
        score = reactable::colDef(name = "Score", aggregate = "mean"),
        scorer = reactable::colDef(name = "Scorer(s)", aggregate = "unique"),
        comments = reactable::colDef(
          name = "Comments",
          aggregate = reactable::JS("function(values, rows) { return '...' }")
        )
      )
    )
  })
}
