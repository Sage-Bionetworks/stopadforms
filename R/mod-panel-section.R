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
          choices = "",
          width = "428px"
        )
      )
    ),
    # Add buttons to expand/collapse all
    fluidRow(
      column(1, offset = 1,
             actionButton(ns("expand_all"), "Expand All", style = "width: 167px;")
      ),
      column(1),
      column(1,
             actionButton(ns("collapse_all"), "Collapse All", style = "width: 167px;")
      )
    ),
    br(),
    fluidRow(
      column(2, offset = 3,
             with_busy_indicator_ui(
               actionButton(
                 ns("refresh_data"),
                 "Refresh Data",
                 style = "width: 167px;"
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
        shinyjs::disabled(numericInput(
          inputId = ns("calculated_overall_score"),
          label = "Calculated Overall Score",
          value = 0,
          min = 0,
          max = 1,
          step = 0.0001
        )),
        numericInput(
          inputId = ns("reviewed_overall_score"),
          label = "Reviewed Overall Score",
          value = 0,
          min = 0,
          max = 1,
          step = 0.0001
        ),
        textAreaInput(
          inputId = ns("internal_comment"),
          label = "Internal Comments (500 character limit)"
        ),
        textAreaInput(
          inputId = ns("external_comment"),
          label = "External Comments (500 character limit)"
        ),
        with_busy_indicator_ui(
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
  submissions <- append_clinical_to_submission(submissions) %>%
    dplyr::mutate(submission = submission %>% trimws())
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

  current_reviews <- show_review_table(
    input = input,
    output = output,
    reviews = reviews,
    submission_id = submission_id
  )
  
  observeEvent(input$expand_all, {
    reactable::updateReactable("averaged_scores", expanded = TRUE, session = getDefaultReactiveDomain())
  })
  
  observeEvent(input$collapse_all, {
    reactable::updateReactable("averaged_scores", expanded = FALSE, session = getDefaultReactiveDomain())
  })

  observeEvent(input$submission, {
    updateNumericInput(
      session = getDefaultReactiveDomain(),
      inputId = "calculated_overall_score",
      value = calculate_submission_score(
        current_submission(),
        current_reviews()
      )
    )
  })
  
  observeEvent(input$reviewed_overall_score, {
    if (!is.na(input$reviewed_overall_score)) {
      if (input$reviewed_overall_score < 0 || input$reviewed_overall_score > 1) {
        shinyjs::runjs("alert('Enter a valid Final Overall Score value ranging from 0 to 1.');")
        updateNumericInput(session, "reviewed_overall_score", value = 0)
      }
    }
  })

  observeEvent(input$refresh_data, {
    with_busy_indicator_server("refresh_data", {
      reviews <<- pull_reviews_table(syn, reviews_table, submissions)
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "submission",
        choices = get_submission_list(reviews),
        selected = submission_id()
      )
      # Something is weird here and I don't know why this is necessary
      current_reviews <<- show_review_table(
        input = input,
        output = output,
        reviews = reviews,
        submission_id = submission_id
      )

      updateNumericInput(
        session = getDefaultReactiveDomain(),
        inputId = "calculated_overall_score",
        value = calculate_submission_score(
          current_submission(),
          current_reviews()
        )
      )
    })
  })
  certified <- check_certified_user(user$ownerId, syn = syn)
  
  observeEvent(input$submission, {
    if (!is.null(input$submission) && nchar(input$submission) > 0) {
      query <- "SELECT * FROM {submissions_table} WHERE form_data_id = {submission_id()}"
      
      result <- readr::read_csv(
        existing_syn_submission()$filepath
      )

      if (nrow(result) > 0) {
        updateNumericInput(session, "reviewed_overall_score", value = result$overall_score[1])
        updateTextAreaInput(session, "internal_comment", value = result$internal_comment[1])
        updateTextAreaInput(session, "external_comment", value = result$external_comment[1])
        
        updateActionButton(session, "submit", label = "Overwrite")
      } else {
        updateNumericInput(session, "reviewed_overall_score", value = 0)
        updateTextAreaInput(session, "internal_comment", value = "")
        updateTextAreaInput(session, "external_comment", value = "")
        
        updateActionButton(session, "submit", label = "Submit")
      }
    }
  })
  
  query_trigger <- reactiveVal(0)
  
  existing_syn_submission <- reactive({
    query_trigger() # If triggered, will automatically re-run the query
    
    syn$tableQuery(
      glue::glue(
        "SELECT * FROM {submissions_table} WHERE form_data_id = {submission_id()}" # nolint
      )
    )
  })

  ## Save new row to table
  observeEvent(input$submit, {
    with_busy_indicator_server("submit", {
      validate(
        need(
          inherits(certified, "check_pass"),
          HTML("You must be a Synapse Certified User to save reviews. <a href=\"https://docs.synapse.org/articles/accounts_certified_users_and_profile_validation.html\">Learn more</a>")
        )
      )
      if (nchar(input$internal_comment) > 500 || nchar(input$external_comment) > 500) { # nolint
        stop("Please limit comments to 500 characters")
      }
      if (input$submission == "") {
        stop("Please select a submission")
      }
      
      syn_result <- existing_syn_submission()
      result <- readr::read_csv(
        syn_result$filepath
      )
      
      if (nrow(result) == 0) {
        new_row <- data.frame(
          form_data_id = submission_id(),
          submission = submission_name(),
          scorer = syn$getUserProfile()$ownerId,
          overall_score = input$reviewed_overall_score,
          internal_comment = input$internal_comment,
          external_comment = input$external_comment,
          stringsAsFactors = FALSE
        )
        
        etag <- NULL
      } else if (nrow(result) == 1) {
        new_row <- result
        new_row$overall_score <- input$reviewed_overall_score
        new_row$internal_comment <- input$internal_comment
        new_row$external_comment <- input$external_comment
        
        etag <- syn_result$etag
      } else {
        stop("Unable to update score: duplicate scores were found for this section from a single reviewer") # nolint
      }
      
      # Create a temporary file path
      temp_file <- tempfile(fileext = ".csv")
      
      # Write the data frame to the temporary CSV file
      write.csv(new_row, temp_file, row.names = FALSE)
      
      # Store into the synapse table
      syn$store(synapse$Table(submissions_table, temp_file, etag=etag))
      
      # Refresh the query now that data has been modified
      query_trigger(query_trigger() + 1)
    })
  })
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
  # TODO: refactor so that returning data and creating output side effect are
  # separate

  to_show <- reactive({
    rollup_scores <- calculate_section_rollup_score(submission_id(), reviews)

    dplyr::filter(reviews, .data$form_data_id == submission_id()) %>%
      dplyr::left_join(rollup_scores) %>%
      dplyr::select(
        .data$step,
        .data$rollup_score,
        .data$score,
        .data$weighted_score,
        .data$scorer,
        .data$comments
      ) %>%
      dplyr::mutate(
        step = factor(.data$step, levels = reorder_steps(.data$step))
      ) %>%
      dplyr::arrange(step)
  })

  output$averaged_scores <- reactable::renderReactable({
    reactable::reactable(
      to_show(),
      groupBy = "step",
      searchable = TRUE,
      pagination = FALSE,
      defaultExpanded = TRUE,
      columns = list(
        step = reactable::colDef(name = "Section"),
        rollup_score = reactable::colDef(name = "Rollup Score", aggregate = "unique"),
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
  return(to_show)
}
