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
#' @param submissions Data frame containing tidied submissions
#' @param reviews_table Synapse table that stores the scores and comments
#'
#' @rdname mod_review_section
#'
#' @keywords internal
#' @importFrom shiny NS tagList
#' @importFrom rlang .data
mod_review_section_ui <- function(id) {
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
          choices = "",
          width = "100%"
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
    br(), br(),
    fluidRow(
      column(
        7,
        offset = 1,
        reactable::reactableOutput(ns("data_section_subset"))
      )
    ),
    br(), br(),
    fluidRow(
      column(
        4,
        offset = 5,
        selectInput(
          inputId = ns("section_score"),
          label = "Score",
          choices = c(
            "Abstain" = -1,
            "None" = 0,
            "Poor" = 0.1,
            "Fair" = 0.25,
            "Good" = 0.85,
            "Excellent" = 1
          )
        ),
        selectInput(
          inputId = ns("section_species"),
          label = "Species",
          choices = c(
            "Not applicable" = "NA",
            "Within species" =  "within",
            "Across species" =  "across"
          )
        ),
        textAreaInput(
          inputId = ns("section_comments"),
          label = "Comments"
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

#' @rdname mod_review_section
#' @keywords internal
mod_review_section_server <- function(input, output, session, synapse, syn,
                                      user, submissions, reviews_table) {
  updateSelectInput(
    session = getDefaultReactiveDomain(),
    "submission",
    choices = c(
      "",
      get_submission_list(submissions)
    )
  )

  observeEvent(input$submission, {
    if (input$submission != "") {
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        "section",
        choices = c(
          "",
          get_sections(
            submissions,
            input$submission
          )
        )
      )
    }
  })

  submission_id <- reactive({
    input$submission
  })
  submission_name <- reactive({
    submissions$submission[submissions$form_data_id == input$submission][1]
  })
  section <- reactive({
    input$section
  })

  ## Show section
  to_show <- reactive({
    sub_section <- dplyr::filter(
      submissions,
      .data$form_data_id == submission_id() & .data$step == section()
    )

    sub_section[c("label", "response")]
  })

  output$data_section_subset <- reactable::renderReactable({
    reactable::reactable(
      to_show(),
      pagination = FALSE,
      columns = list(
        label = reactable::colDef(name = "Label"),
        response = reactable::colDef(name = "Response")
      )
    )
  })
  certified <- check_certified_user(user$ownerId, syn = syn)
  
  query_trigger <- reactiveVal(0)
  
  existing_syn_submission <- reactive({
    req(input$submission)
    req(input$section)
    
    query_trigger() # If triggered, will automatically re-run the query
    
    syn$tableQuery(
      glue::glue(
        "SELECT * FROM {reviews_table} WHERE (scorer = {syn$getUserProfile()$ownerId} AND form_data_id = {submission_id()} AND step = '{input$section}')" # nolint
      )
    )
  })
  
  existing_submission <- reactive({
    req(input$submission)
    req(input$section)
    
    result <- readr::read_csv(
      existing_syn_submission()$filepath,
      col_types = readr::cols(
        ROW_ID = readr::col_double(),
        ROW_VERSION = readr::col_double(),
        form_data_id = readr::col_double(),
        submission = readr::col_character(),
        step = readr::col_character(),
        scorer = readr::col_double(),
        score = readr::col_double(),
        comments = readr::col_character(),
        species = readr::col_character()
      )
    )

    return(result)
  })
  
  observe({
    result <- existing_submission()
    
    selected_species <- result$species[1]
    if (is.na(selected_species)) selected_species <- "NA"
    
    if (nrow(result) > 0) {
      updateSelectInput(session, "section_score", selected = result$score[1])
      updateSelectInput(session, "section_species", selected = selected_species)
      updateTextAreaInput(session, "section_comments", value = result$comments[1])
      updateActionButton(session, "submit", label = "Overwrite")
    } else {
      # Handle automatic default for Species dropdown
      default_species <- "NA"
      label_ind <- grep("What cell line|Species", to_show()$label)

      # If there is a field for cell line or species, parse it
      if (length(label_ind) > 0 && !is.na(to_show()$response[label_ind[1]])) {
        default_species <- "across"
        if (grepl("mouse", to_show()$response[label_ind[1]], ignore.case = TRUE)) {
          default_species <- "within"
        }
      }
      
      # Update all the dropdowns
      updateSelectInput(session, "section_score", selected = -1)
      updateSelectInput(session, "section_species", selected = default_species)
      updateTextAreaInput(session, "section_comments", value = "")
      updateActionButton(session, "submit", label = "Submit")
    }
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
      if (input$submission == "" || input$section == "") {
        stop("Please select a submission and section")
      }

      syn_result <- existing_syn_submission()
      result <- existing_submission()

      if (nrow(result) == 0) {
        new_row <- data.frame(
          form_data_id = submission_id(),
          submission = submission_name(),
          step = input$section,
          scorer = syn$getUserProfile()$ownerId,
          score = input$section_score,
          comments = input$section_comments,
          species = input$section_species,
          stringsAsFactors = FALSE
        )
        
        etag <- NULL
      } else if (nrow(result) == 1) {
        new_row <- result
        new_row$score <- input$section_score
        new_row$comments <- input$section_comments
        new_row$species <- input$section_species
        
        etag <- syn_result$etag
      } else {
        stop("Unable to update score: duplicate scores were found for this section from a single reviewer") # nolint
      }
      
      ## TODO: Re-examine this workaround
      # Package dependencies on shinyapps.io are causing issues with dataframe storing
      # Investigate Python version and environment on server
      
      # Create a temporary file path
      temp_file <- tempfile(fileext = ".csv")
      
      # Write the data frame to the temporary CSV file
      write.csv(new_row, temp_file, row.names = FALSE)
      
      # Store into the synapse table
      syn$store(synapse$Table(reviews_table, temp_file, etag=etag))
      
      # Refresh the query now that data has been modified
      query_trigger(query_trigger() + 1)
      
      ## Update the label of the button now
      updateActionButton(session, "submit", label = "Overwrite")
    })
  })
}

#' Get list of submissions with ids
#'
#' Get list of submissions with their form_data_id.
#'
#' @inheritParams get_sections
#' 
#' @importFrom lubridate ymd_hms with_tz
#' @return named list where names are the submission names
#'   and values are their form_data_ids.
get_submission_list <- function(data) {
  sub_ids <- as.list(unique(data$form_data_id))
  
  sub_names <- purrr::map(
    sub_ids,
    function(x) {
      date_string <- data$submitted_on[data$form_data_id == x][1]
      formatted_date <- clean_date_strings(date_string)
      
      return(paste0(formatted_date, ": ", data$submission[data$form_data_id == x][1]))
    }
  )

  names(sub_ids) <- sub_names
  
  sorted_sub_ids <- sub_ids[order(names(sub_ids), decreasing = TRUE)]
  sorted_sub_ids
}

#' Get submission sections
#'
#' Get submission steps, which can be thought of
#' as the main sections.
#'
#' @param data The submission data as given by
#'   [get_submissions()].
#' @param submission_id Submission form_data_id.
get_sections <- function(data, submission_id) {
  submission <- data[which(data$form_data_id == submission_id), ]
  steps <- unique(submission$step)
  metadata_index <- which(steps == "metadata")
  if (length(metadata_index) > 0) {
    steps <- steps[-metadata_index]
  }
  reorder_steps(steps)
}
