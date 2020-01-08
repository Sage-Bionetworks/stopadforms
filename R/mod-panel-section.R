#' @title   mod_panel_section_ui and mod_panel_section_server
#' @description  View summarized scores for a submission
#'
#' @inheritParams mod_review_section_ui
#' @inheritParams mod_review_section_server
#' @param submissions_table Synapse table that stores the
#'   overall submission scores and comments
#'
#' @rdname mod_panel_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
mod_panel_section_ui <- function(id) {
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
    ),
    fluidRow(
      column(
        4,
        offset = 5,
        numericInput(
          inputId = ns("overall_score"),
          label = "Overall Score",
          value = 1
        ),
        textAreaInput(
          inputId = ns("internal_comments"),
          label = "Internal Comments (500 character limit)"
        ),
        textAreaInput(
          inputId = ns("external_comments"),
          label = "External Comments (500 character limit)"
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

#' @rdname mod_panel_section
#' @keywords internal
mod_panel_section_server <- function(input, output, session, synapse, syn,
                                     reviews_table, submissions_table) {
  submission <- reactive({
    input$submission
  })

  ## Load reviews
  reviews <- pull_reviews_table(syn, reviews_table)
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c("", unique(reviews$submission))
  )
  show_review_table(input, output, reviews, submission)

  observeEvent(input$refresh_comments, {
    dccvalidator::with_busy_indicator_server("refresh_comments", {
      reviews <<- pull_reviews_table(syn, reviews_table)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "submission",
        choices = c("", unique(reviews$submission)),
        selected = submission()
      )
      show_review_table(input, output, reviews, submission)
    })
  })

  ## Save new row to table
  observeEvent(input$submit, {
    dccvalidator::with_busy_indicator_server("submit", {
      if (nchar(input$internal_comments) > 500 || nchar(input$external_comments) > 500) { # nolint
        stop("Please limit comments to 500 characters")
      }
      form_data_id <- reviews$formDataId[
        which(reviews$submission == input$submission)[1]
      ]
      result <- readr::read_csv(
        syn$tableQuery(
          glue::glue(
            "SELECT * FROM {submissions_table} WHERE (scorer = {syn$getUserProfile()$ownerId} AND formDataId = {form_data_id})" # nolint
          )
        )$filepath
      )
      if (nrow(result) == 0) {
        new_row <- data.frame(
          formDataId = form_data_id,
          submission = input$submission,
          scorer = syn$getUserProfile()$ownerId,
          overall_score = input$overall_score,
          internal_comment = input$internal_comments,
          external_comment = input$external_comments,
          stringsAsFactors = FALSE
        )
      } else if (nrow(result) == 1) {
        new_row <- result
        new_row$overall_score <- input$overall_score
        new_row$internal_comment <- input$internal_comments
        new_row$external_comment <- input$external_comments
      } else {
        stop("Unable to update score: duplicate scores were found for this section from a single reviewer") # nolint
      }
      syn$store(synapse$Table(submissions_table, new_row))
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
#' @param submission Reactive shiny object containing submission name
#'   accessible via `submission()`.
#' @keywords internal
show_review_table <- function(input, output, reviews, submission) {
  to_show <- reactive({
    dplyr::filter(reviews, submission == submission()) %>%
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
