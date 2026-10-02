#' Get the submissions based on status
#'
#' Get the submissions based on status. No files are downloaded and no URLs
#' are generated here: each submission gets a function that requests a fresh
#' pre-signed URL for its JSON file when called. Pre-signed URLs from Synapse
#' are more likely to be refused (HTTP 403) the longer they wait before use, so
#' the URL is generated just before each download attempt instead of up front.
#'
#' @param statuses A character vector of statuses to include from the set:
#'   `SUBMITTED_WAITING_FOR_REVIEW`, `ACCEPTED`, `REJECTED`.
#' @param group The number for a specific Synapse forms group.
#' @inheritParams mod_review_section_server
#' @return A list, named by form data ID, of functions that each return a new
#'   pre-signed URL for the submission's JSON file, for submissions that have
#'   the requested status. Each function has `"form_name"` and `"submitted_on"`
#'   attributes. `NULL` if there are no such submissions.
#' @importFrom rlang .data
#' @export
get_submissions <- function(syn, group, statuses) {

  if (is.null(statuses)) {
    return(NULL)
  }

  url_sources <- purrr::flatten(
    purrr::map(statuses, function(x) {
      metadata <- synapseforms::get_submissions_metadata(
        syn = syn,
        group = group,
        state_filter = x
      )
      if (is.null(metadata)) {
        return(list())
      }
      url_sources <- purrr::pmap(
        list(
          metadata$dataFileHandleId,
          metadata$formDataId,
          metadata$name,
          metadata$submissionStatus_submittedOn
        ),
        function(file_handle_id, form_data_id, name, submitted_on) {
          ## Evaluate now, so each function gets its own submission's IDs
          force(file_handle_id)
          force(form_data_id)
          url_source <- function() {
            get_presigned_url(syn, file_handle_id, form_data_id)
          }
          attr(url_source, "form_name") <- name
          attr(url_source, "submitted_on") <- submitted_on
          url_source
        }
      )
      names(url_sources) <- metadata$formDataId
      url_sources
    })
  )

  if (length(url_sources) == 0) {
    return(NULL)
  } else {
    return(url_sources)
  }
}

#' Get a pre-signed URL for a submission's file
#'
#' Request a new pre-signed URL for a form submission's file from Synapse. This
#' is the same request as `synapseforms:::get_ps_url()` makes.
#'
#' @noRd
#' @inheritParams mod_review_section_server
#' @param file_handle_id The submission's data file handle ID.
#' @param form_data_id The submission's form data ID.
#' @return The pre-signed URL.
get_presigned_url <- function(syn, file_handle_id, form_data_id) {
  body <- glue::glue(
    '{{"requestedFiles": [{{"fileHandleId": "{file_handle_id}", ',
    '"associateObjectId": "{form_data_id}", "associateObjectType": "FormData"}}], ',
    '"includePreSignedURLs": true, "includeFileHandles": false}}'
  )
  response <- synapseforms::rest_post(
    syn = syn,
    uri = "https://repo-prod.prod.sagebase.org/file/v1/fileHandle/batch",
    body = body
  )
  ## An empty requestedFiles list gets the same error as a missing URL
  requested <- if (length(response$requestedFiles) > 0) {
    response$requestedFiles[[1]]
  } else {
    list()
  }
  if (is.null(requested$preSignedURL)) {
    stop(
      "Synapse returned no pre-signed URL for form data ID ", form_data_id,
      " (failure code: ", requested$failureCode %||% "none", ")",
      call. = FALSE
    )
  }
  requested$preSignedURL
}

#' Process submissions
#'
#' Process JSON files into a single table containing all submissions. Cleans up
#' the data to provide user-friendly variable and section names, and remove the
#' `metadata` section.
#'
#' @param submissions A named list, i.e. the output of [get_submissions()].
#'   The name of each element should be its form data ID. Each element is
#'   either a URL or path to a JSON file, or a function that returns one (see
#'   [create_table_from_json_file()]). A function's `"form_name"` and
#'   `"submitted_on"` attributes, if present, are included when a failure is
#'   logged.
#' @param lookup_table Dataframe with columns "section",
#'   "step", "variable" , and "label" used for user-friendly section and
#'   variable display. "step" maps desired "section" names. "label" maps
#'   desired "variable" names.
#' @param complete If `TRUE`, will join in all section and variable names that
#'   were not provided as part of the submission. If `FALSE`, will only return
#'   the data that was present in the JSON file.
#' @return A data frame containing the combined responses for all submissions
#'   provided to the `submissions` argument that could be processed. Any
#'   submission that fails to download or parse is skipped and logged, and its
#'   form data ID is recorded in the `"failed_ids"` attribute of the result
#'   (a character vector, empty if none failed). The `"failed_names"` attribute
#'   holds the matching submission names, `NA` where the name is not known. If
#'   no submissions can be processed, an error is thrown.
#' @export
#' @importFrom rlang .data
process_submissions <- function(submissions, lookup_table, complete = TRUE) {
  if (is.null(submissions)) {
    stop("No submissions to process", call. = FALSE)
  }

  ## Main table creation, along with submission name. Suppress warnings about
  ## vectorizing 'glue' attributes. Each submission is processed separately so
  ## that one bad submission does not prevent the others from loading.
  sub_tables <- purrr::imap(
    submissions, # names are the form data IDs
    function(filename, data_id) {
      ## Warnings are muffled, but kept so they can be logged if processing
      ## fails; e.g. download.file() reports the HTTP status as a warning
      warns <- character(0)
      withCallingHandlers(
        tryCatch(
          create_table_from_json_file(
            filename,
            data_id,
            lookup_table = lookup_table,
            complete = complete
          ),
          error = function(err) {
            message(
              "Failed to process form data ID ", data_id,
              describe_submission(filename), ": ",
              conditionMessage(err),
              if (length(warns) > 0) {
                paste0(" [warnings: ", paste(unique(warns), collapse = "; "), "]")
              }
            )
            NULL
          }
        ),
        warning = function(w) {
          warns <<- c(warns, conditionMessage(w))
          invokeRestart("muffleWarning")
        }
      )
    }
  )
  failed_ids <- names(sub_tables)[purrr::map_lgl(sub_tables, is.null)]

  if (length(failed_ids) == length(sub_tables)) {
    stop(
      "Could not process any submissions (form data IDs: ",
      paste(failed_ids, collapse = ", "), ")",
      call. = FALSE
    )
  }

  ## Remove metadata section
  all_subs <- dplyr::bind_rows(sub_tables) %>%
    dplyr::filter(.data$section != "metadata") %>%
    ## Fix display of some responses
    change_logical_responses() %>%
    therapeutic_approach_response()

  attr(all_subs, "failed_ids") <- failed_ids
  attr(all_subs, "failed_names") <- vapply(
    submissions[failed_ids],
    function(x) attr(x, "form_name") %||% NA_character_,
    character(1),
    USE.NAMES = FALSE
  )
  all_subs
}

#' Describe a submission for logging
#'
#' @noRd
#' @param source An element of the `submissions` list given to
#'   [process_submissions()].
#' @return `" (<name>, submitted <date>)"` from the source's `"form_name"` and
#'   `"submitted_on"` attributes, or `""` if it has neither.
describe_submission <- function(source) {
  details <- c(
    attr(source, "form_name"),
    if (!is.null(attr(source, "submitted_on"))) {
      paste("submitted", attr(source, "submitted_on"))
    }
  )
  if (length(details) == 0) {
    return("")
  }
  paste0(" (", paste(details, collapse = ", "), ")")
}


#' Create table from JSON file
#'
#' Convert a JSON file containing submission data to a data frame.
#'
#' @param filename URL or path to the JSON file, or a function that returns
#'   one, such as an element of [get_submissions()]. A function is called again
#'   for each download attempt, so every attempt uses a fresh URL.
#' @param data_id Form data ID
#' @inheritParams process_submissions
#' @export
create_table_from_json_file <- function(filename, data_id, lookup_table,
                                        complete = TRUE) {
  
  # Log the data id
  cat("\n")  # Handles newlines properly
  print(paste0("Form Data ID: ", data_id))

  # Download file first to avoid parsing error from Amazon tokens
  # ALZ-88: never pass a URL straight to jsonlite::fromJSON(). It only treats a
  # string as a URL if it is shorter than 2084 bytes, and parses longer ones as
  # JSON text, which fails. Pre-signed URLs can be longer than that.
  R_string <- MHmakeRandomString(length = 10)
  newFilename <- paste0(R_string, ".json")

  ## Always clean up the downloaded file, even if downloading or parsing fails
  on.exit(unlink(newFilename), add = TRUE)
  download_with_retry(filename, newFilename, label = data_id)

  ## Load JSON
  data <- jsonlite::fromJSON(newFilename, simplifyVector = FALSE)
  
  ## Iterate over list of sections to create data frame
  sub <- purrr::imap_dfr(
    data,
    create_section_table,
    lookup_table = lookup_table,
    complete = complete
  )
  
  ## Add unanswered sections, append experiment numbers to section names
  sub <- map_names(sub, lookup_table = lookup_table, complete = complete) %>%
    append_exp_nums()
  
  ## Add form data ID and sub name
  user_name <- sub[sub$variable == "last_name", "response", drop = TRUE]
  compound_name <- sub[sub$variable == "compound_name", "response", drop = TRUE]
  sub %>%
    dplyr::mutate(form_data_id = data_id) %>%
    dplyr::mutate(submission = glue::glue("{user_name} - {compound_name}"))
}

#' Download a file, retrying on failure
#'
#' Synapse's download service sometimes refuses valid pre-signed URLs (HTTP
#' 403), so failed downloads are retried. If `source` is a function, it is
#' called on each attempt to get a fresh URL, which is much more likely to
#' succeed than retrying the same URL.
#'
#' @noRd
#' @param source URL or path to download, or a function that returns one.
#' @param destfile Where to save the file.
#' @param label Form data ID, used in log messages.
#' @param attempts Number of attempts to make.
#' @param wait Seconds to wait before each retry; recycled as needed.
#' @param download Function used to download the file, with the arguments of
#'   [utils::download.file()].
#' @return `destfile`, invisibly. Errors if every attempt fails.
download_with_retry <- function(source, destfile, label, attempts = 3,
                                wait = getOption("stopadforms.download_retry_wait", c(1, 2)),
                                download = utils::download.file) {
  wait <- rep_len(wait, max(attempts - 1, 1))
  for (i in seq_len(attempts)) {
    ## download.file() reports the HTTP status of a failed download as a
    ## warning, so keep each attempt's warnings for the log
    warns <- character(0)
    result <- withCallingHandlers(
      tryCatch(
        {
          url <- if (is.function(source)) source() else source
          ## quiet = TRUE also keeps pre-signed URLs out of the logs
          download(url, destfile, quiet = TRUE)
          NULL
        },
        error = function(err) err
      ),
      warning = function(w) {
        warns <<- c(warns, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    if (is.null(result)) {
      return(invisible(destfile))
    }

    ## Report the error and warnings, but not the URL they may contain
    problem <- paste0(
      redact_urls(conditionMessage(result)),
      if (length(warns) > 0) {
        paste0(" [", redact_urls(paste(unique(warns), collapse = "; ")), "]")
      }
    )
    if (i < attempts) {
      message(
        "Download attempt ", i, " of ", attempts, " failed for form data ID ",
        label, ": ", problem, "; retrying in ", wait[i], " s"
      )
      Sys.sleep(wait[i])
    }
  }
  stop("download failed after ", attempts, " attempts: ", problem, call. = FALSE)
}

#' Remove URLs from a message
#'
#' @noRd
#' @param x Character vector.
#' @return `x` with any http(s) URLs replaced by `<URL>`.
redact_urls <- function(x) {
  gsub("https?://[^ '\"]+", "<URL>", x)
}

#' Create table for a section
#'
#' Create table for a section of a submission. Some sections contain multiple
#' experiments nested within them; if this is the case, they will be unnested
#' and given an experiment number.
#'
#' @param data A list containing data from one section of a submission
#' @param section The section name
#' @inheritParams process_submissions
create_section_table <- function(data, section, lookup_table, complete = TRUE) {
  
    # Log the section
    print(paste0("Section: ", section))

    # ALZ-157: remove empty objects from inner lists
    if (length(names(data)) == 1 && names(data) %in% c("experiments", "cell_line_efficacy", "cell_line_binding")) {
      data <- remove_empty_objects(data)
    }
    # If no data, return NULL
    if (length(data) == 0) {
      return(NULL)
    } else if (length(names(data)) == 1 && names(data) %in% c("experiments", "cell_line_efficacy", "cell_line_binding")) { # nolint
    # If "experiments" is the only element, we need to go deeper to extract the
    # info for each experiment separately. The section name needs to have a
    # number to differentiate.
    dat <- purrr::imap_dfr(
      data[[1]],
      function(data, index) {
        create_values_table(
          data = data,
          section = section,
          exp_num = index,
          lookup_table = lookup_table,
          complete = complete
        )
      }
    )
  } else {
    # If the section does not contain separate experiments, return the data
    # from the section
    dat <- create_values_table(
      data = data,
      section = section,
      exp_num = NA,
      lookup_table = lookup_table,
      complete = complete
    )
  }
  dat
}

#' Create a tibble from the values within section
#'
#' Create a tibble with section name, experiment number, variables, and
#' response values.
#'
#' @inheritParams create_section_table
#' @param exp_num Numeric experiment number
create_values_table <- function(data, section, lookup_table,
                                complete = TRUE, exp_num = NA) {
  ## Combine multiple routes into comma-separated single response so we can
  ## later join in variable names
  data <- combine_route_responses(data)
  dat <- tibble::tibble(
    section = section,
    variable = names(unlist(data)),
    response = as.character(unlist(data))
  )
  if (isTRUE(complete)) {
    dat <- add_section_variables(dat, lookup_table = lookup_table)
  }
  ## Add experiment number
  dplyr::mutate(dat, exp_num = exp_num)
}

#' Change logical responses
#'
#' Change logical responses TRUE/FALSE to Yes/No. Additionally, need to handle
#' the variable "is_solution" which sometimes has 0/1 instead of TRUE/FALSE.
#'
#' @param data Dataframe with response column and variable column.
#' @importFrom rlang .data
change_logical_responses <- function(data) {
  dplyr::mutate(
    data,
    response = dplyr::case_when(
      .data$response == "FALSE" ~ "No",
      .data$response == "TRUE" ~ "Yes",
      .data$variable == "is_solution" & .data$response %in% c("0", "FALSE") ~ "No", # nolint
      .data$variable == "is_solution" & .data$response %in% c("1", "TRUE") ~ "Yes", # nolint
      TRUE ~ response
    )
  )
}

#' Add unanswered questions within a section.
#'
#' Uses `lookup_table` data to add in questions within a section that were
#' unanswered (and therefore missing from the original JSON data).
#'
#' @param data Dataframe with columns "section", "variable", and "exp_num".
#' @inheritParams process_submissions
add_section_variables <- function(data, lookup_table) {
  ## Filter lookup table to current section
  lookup <- dplyr::filter(
    lookup_table,
    .data$section %in% data$section
  ) %>%
    dplyr::select(.data$variable, .data$section)
  dplyr::full_join(data, lookup, by = c("variable", "section"))
}

#' Append user-friendly section and variable names
#'
#' Appends columns "step" and "label", which corresponds with "section" and
#' "variable". Map via lookup_table and fix missing step/label.
#'
#' @param data Dataframe with columns "section", "variable", and "exp_num".
#' @inheritParams process_submissions
map_names <- function(data, lookup_table, complete = TRUE) {
  join_to_use <- ifelse(complete, dplyr::full_join, dplyr::left_join)
  ## First join in section names. This join is done in 2 steps because the
  ## variables sometimes are missing from the lookup table (due to having
  ## numbers appended to them -- e.g. route1, route2). If we join all at once,
  ## then both step & label are NA and we have to go back and get step labels.
  dat <- dplyr::left_join(
    data,
    unique(lookup_table[, c("section", "step")]),
    by = "section"
  )
  dat <- join_to_use(
    dat,
    lookup_table,
    by = c("section", "variable")
  )
  ## Keep original section/variable names if there's no mapping
  dat %>%
    dplyr::mutate(
      step = dplyr::coalesce(.data$step.x, .data$step.y, .data$section),
      label = dplyr::coalesce(.data$label, .data$variable)
    ) %>%
    dplyr::select(-.data$step.x, -.data$step.y)
}

#' Append experiment numbers to step name
#'
#' When data has multiple experiments, appends the experiment number to each
#' section, e.g. `LD50 [1]`, `LD50 [2]`, etc.
#'
#' @param data Data frame containing submission data
append_exp_nums <- function(data) {
  rel_sections <- c("binding", "efficacy", "ld50", "acute_dosing", "chronic_dosing",
                    "teratogenicity", "in_vivo_data", "pk_in_vivo")
  
  dplyr::mutate(
    data,
    step = dplyr::case_when(
      !is.na(exp_num) ~ as.character(glue::glue("{step} [{exp_num}]")),
      section %in% rel_sections ~ as.character(glue::glue("{step} [1]")),
      TRUE ~ as.character(glue::glue("{step}"))
    )
  )
}

#' Rename response "both" to "prophylactic, symptomatic" in therapeutic approach
#'
#' @inheritParams append_exp_nums
therapeutic_approach_response <- function(data) {
  dplyr::mutate(
    data,
    response = dplyr::case_when(
      variable == "therapeutic_approach" & response == "both" ~
        "prophylactic, symptomatic",
      TRUE ~ response
    )
  )
}

#' Combine multiple routes into one comma-separated response
#'
#' @param data List containing route data
combine_route_responses <- function(data) {
  if ("route" %in% names(data)) {
    data$route <- paste(data$route, collapse = ", ")
  }
  data
}

#' ALZ-157: Remove empty objects from inner lists for legacy submissions.
#'
#' @param data_list List containing data
remove_empty_objects <- function(data_list) {
  if (is(data_list, "list")) {
    if (all(lengths(data_list) == 0)) {
      return(NULL)
    }
    data_list <- lapply(data_list, remove_empty_objects)
    keep <- lengths(data_list) > 0
    data_list <- data_list[keep]
    return(data_list)
  }
  return(data_list)
}

clean_date_strings <- function(date_string) {
  # Step 1: Parse the string to a datetime object and convert to Eastern Time
  datetime_utc <- lubridate::ymd_hms(date_string, tz = "UTC")  # Parse as UTC
  datetime_et <- lubridate::with_tz(datetime_utc, tzone = "America/New_York")  # Convert to Eastern Time
  
  # Step 2: Extract just the date in the desired format (YYYY-MM-DD)
  formatted_date <- format(datetime_et, "%Y-%m-%d")
  
  return(formatted_date)
}