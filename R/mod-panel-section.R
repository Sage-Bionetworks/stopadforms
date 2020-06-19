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
        7,
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
mod_panel_section_server <- function(input, output, session, synapse, syn, user,
                                     submissions, reviews_table,
                                     submissions_table) {
  ## Load submissions and reviews
  submissions <- append_clinical_to_submission(submissions)
  reviews <- pull_reviews_table(syn, reviews_table, submissions)

  submission_id <- reactive({
    input$submission
  })
  submission_name <- reactive({
    req(reviews)
    if (input$submission != "") {
      reviews$submission[reviews$form_data_id == input$submission][1]
    }
  })
  current_submission <- reactive({
    req(submissions)
    if (input$submission != "") {
      dplyr::filter(submissions, .data$form_data_id == submission_id())
    }
  })

  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c("", get_submission_list(reviews))
  )

  current_reviews <- reactive({
    show_review_table(
      input = input,
      output = output,
      reviews = reviews,
      submission_id = submission_id
    )
  })

  observeEvent(input$submission, {
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "overall_score",
      value = calculate_submission_score(
        current_submission(),
        current_reviews()
      )
    )
  })

  observeEvent(input$refresh_comments, {
    dccvalidator::with_busy_indicator_server("refresh_comments", {
      reviews <<- pull_reviews_table(syn, reviews_table, submissions)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "submission",
        choices = get_submission_list(reviews),
        selected = submission_id()
      )
      show_review_table(
        input = input,
        output = output,
        reviews = reviews,
        submission_id = submission_id
      )
    })
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
      if (nchar(input$internal_comments) > 500 || nchar(input$external_comments) > 500) { # nolint
        stop("Please limit comments to 500 characters")
      }
      if (input$submission == "") {
        stop("Please select a submission")
      }
      result <- readr::read_csv(
        syn$tableQuery(
          glue::glue(
            "SELECT * FROM {submissions_table} WHERE (scorer = {syn$getUserProfile()$ownerId} AND form_data_id = {submission_id()})" # nolint
          )
        )$filepath
      )
      if (nrow(result) == 0) {
        new_row <- data.frame(
          formDataId = submission_id(),
          submission = submission_name(),
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
      shinyjs::reset("internal_comments")
      shinyjs::reset("external_comments")
      shinyjs::reset("overall_score")
    })
  })
}

#' @title Pull latest review table
#'
#' @description Pull latest review table from Synapse.
#'
#' @inheritParams mod_panel_section_server
#' @keywords internal
pull_reviews_table <- function(syn, reviews_table, submissions) {
  reviews <- syn$tableQuery(glue::glue("SELECT * FROM {reviews_table}"))
  reviews <- readr::read_csv(reviews$filepath) %>%
    dplyr::mutate(scorer = get_display_name(syn, .data$scorer)) %>%
    dplyr::mutate(form_data_id = as.character(.data$form_data_id)) %>%
    calculate_scores_rowwise(submissions)
}

#' @title Show review table
#'
#' @description Show review table.
#'
#' @inheritParams mod_panel_section_server
#' @param reviews Dataframe review table.
#' @param submission_id Reactive shiny object containing submission id
#'   accessible via `submission()`.
#' @keywords internal
#' @importFrom rlang .data
show_review_table <- function(input, output, reviews, submission_id) {
  to_show <- reactive({
    dplyr::filter(reviews, .data$form_data_id == submission_id()) %>%
      dplyr::select(
        .data$step,
        .data$score,
        .data$weighted_score,
        .data$scorer,
        .data$comments
      )
  })

  output$averaged_scores <- reactable::renderReactable({
    reactable::reactable(
      to_show(),
      groupBy = "step",
      searchable = TRUE,
      pagination = FALSE,
      columns = list(
        step = reactable::colDef(name = "Section"),
        score = reactable::colDef(name = "Gamma", aggregate = "unique"),
        weighted_score = reactable::colDef(name = "Score", aggregate = "unique"),
        scorer = reactable::colDef(name = "Scorer(s)", aggregate = "unique"),
        comments = reactable::colDef(
          name = "Comments",
          aggregate = reactable::JS("function(values, rows) { return '...' }")
        )
      )
    )
  })
  return(to_show())
}
