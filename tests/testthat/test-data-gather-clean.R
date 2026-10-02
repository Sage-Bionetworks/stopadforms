context("data-gather-clean.R")

library(stopadforms)

## Don't wait between download retries in tests
old_options <- options(stopadforms.download_retry_wait = 0)

## Base URL for downloading local json files
download_path <- paste('file://', getwd(), sep = "")

## Sample JSON data to test with
json <- '
{
  "pk_in_vitro": {
    "permeability": "super permeable"
  },
  "binding": null,
  "naming": {
    "compound_name": "test",
    "first_name": "Kara",
    "last_name": "Woo"
  },
  "chronic_dosing": {
    "experiments": [
      {
        "age_range": null,
        "dose_range": null,
        "name": "my experiment 1",
        "species": "mouse",
        "strain": "APP/PS1",
        "sex": "both",
        "route": [
          "sublingual",
          "injection",
          "transdermal"
        ]
      },
      {
        "age_range": null,
        "dose_range": null,
        "name": "my experiment 2",
        "species": "mouse",
        "strain": "APP/PS1",
        "sex": "both",
        "route": [
          "oral",
          "injection",
          "transdermal",
          "formulated_in_food"
        ]
      }
    ]
  }
}
'

# write to file to allow data-gather-clean.R
# create_table_from_json_file to operate
write(json, "test1.json")
json1_download_path <- paste(download_path, "/test1.json", sep = "")

# create_table_from_json_file() ------------------------------------------------

test_that("create_table_from_json_file creates (at least) one row per row in lookup table", { # nolint
  dat <- create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = TRUE
  )
  expect_true(nrow(dat) >= nrow(lookup_table))
})

test_that("All sections are represented if complete = TRUE", {
  dat <- create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = TRUE
  )
  expect_true(all(lookup_table$section %in% dat$section))
})

test_that("create_table_from_json_file creates one row per response if complete = FALSE", { # nolint
  dat <- create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_equal(nrow(dat), 14)
})

test_that("Submission is named by user name and compound name", {
  dat <- create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_true(all(dat$submission == "Woo - test"))
})

test_that("Submission's data ID is added to data", {
  dat <- create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_true(all(dat$form_data_id == "1"))
})

test_that("create_table_from_json_file gets missing sections added to each experiment", { # nolint
  lookup_table <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50", "LD50"),
    variable = c("reference", "duration"),
    label = c("Provide a reference", "Duration")
  )
  
  json <- '
{
  "naming": {
    "compound_name": "test",
    "first_name": "Kara",
    "last_name": "Woo"
  },
  "ld50": {
    "experiments": [
      {
        "duration": 10
      },
      {
        "duration": 15
      }
    ]
  }
}
'
  write(json, "test2.json")
  json2_download_path <- paste(download_path, "/test2.json", sep = "")
  
  res <- create_table_from_json_file(
    json2_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = TRUE
  )
  
  ## # clean up test2.json file
  file.remove("test2.json")
  
  ## "reference" should appear twice
  expect_equal(sum(res$variable == "reference"), 2)
})

test_that("create_table_from_json_file returns correct columns", {
  correct <- c("section", "variable", "response", "label", "exp_num", "step",
               "form_data_id", "submission")
  res <- create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table,
    complete = TRUE
  )
  expect_equal(setdiff(correct, names(res)), character(0))
})

test_that("create_table_from_json_file logs only the form data ID", {
  ## One log line per submission, not one per section, to keep logs short
  out <- capture.output(invisible(create_table_from_json_file(
    json1_download_path,
    data_id = "1",
    lookup_table = lookup_table
  )))
  expect_equal(out, '[1] "Form Data ID: 1"')
})

# process_submissions() --------------------------------------------------------

write("{ this is not valid json", "malformed.json")
malformed_download_path <- paste(download_path, "/malformed.json", sep = "")
missing_download_path <- paste(download_path, "/does-not-exist.json", sep = "")

test_that("process_submissions skips submissions that fail and records their IDs", { # nolint
  submissions <- list(
    "1" = json1_download_path,
    "2" = missing_download_path,
    "3" = malformed_download_path
  )
  expect_no_warning(
    msgs <- capture_messages(
      res <- process_submissions(submissions, lookup_table)
    )
  )
  expect_true(any(grepl("Failed to process form data ID 2", msgs)))
  expect_true(any(grepl("Failed to process form data ID 3", msgs)))
  expect_equal(unique(res$form_data_id), "1")
  expect_equal(attr(res, "failed_ids"), c("2", "3"))
})

test_that("process_submissions records no failed IDs if all succeed", {
  res <- process_submissions(list("1" = json1_download_path), lookup_table)
  expect_equal(attr(res, "failed_ids"), character(0))
})

test_that("process_submissions errors if no submissions can be processed", {
  submissions <- list(
    "2" = missing_download_path,
    "3" = malformed_download_path
  )
  expect_error(
    suppressMessages(suppressWarnings(
      process_submissions(submissions, lookup_table)
    )),
    "Could not process any submissions \\(form data IDs: 2, 3\\)"
  )
})

test_that("A failed parse does not leave downloaded files behind", {
  json_files_before <- list.files(pattern = "\\.json$")
  suppressMessages(suppressWarnings(
    process_submissions(
      list("1" = json1_download_path, "3" = malformed_download_path),
      lookup_table
    )
  ))
  expect_equal(list.files(pattern = "\\.json$"), json_files_before)
})

test_that("process_submissions accepts functions that return URLs", {
  res <- process_submissions(
    list("1" = function() json1_download_path),
    lookup_table
  )
  expect_equal(unique(res$form_data_id), "1")
  expect_equal(attr(res, "failed_ids"), character(0))
  expect_equal(attr(res, "failed_names"), character(0))
})

test_that("process_submissions logs and records the names of failed submissions", { # nolint
  named_source <- function() missing_download_path
  attr(named_source, "form_name") <- "CNS4.json"
  attr(named_source, "submitted_on") <- "2026-09-09T18:39:03.067Z"
  submissions <- list(
    "1" = json1_download_path,
    "774" = named_source,
    "3" = malformed_download_path
  )
  msgs <- capture_messages(
    res <- process_submissions(submissions, lookup_table)
  )
  expect_true(any(grepl(
    "Failed to process form data ID 774 (CNS4.json, submitted 2026-09-09T18:39:03.067Z): ", # nolint
    msgs,
    fixed = TRUE
  )))
  ## A plain path has no name, so its log line has no details
  expect_true(any(grepl("Failed to process form data ID 3: ", msgs, fixed = TRUE)))
  expect_equal(attr(res, "failed_ids"), c("774", "3"))
  expect_equal(attr(res, "failed_names"), c("CNS4.json", NA_character_))
})

# download_with_retry() --------------------------------------------------------

## A fake download.file() that fails (as an HTTP 403 does) a set number of
## times, then writes a file. Records the URL used for each call.
fake_download <- function(failures) {
  calls <- character(0)
  download <- function(url, destfile, quiet) {
    calls <<- c(calls, url)
    if (length(calls) <= failures) {
      warning("cannot open URL '", url, "': HTTP status was '403 Forbidden'")
      stop("cannot open URL '", url, "'")
    }
    writeLines("{}", destfile)
  }
  list(download = download, calls = function() calls)
}

test_that("download_with_retry retries a failed download", {
  fake <- fake_download(failures = 2)
  destfile <- tempfile()
  msgs <- capture_messages(
    stopadforms:::download_with_retry(
      "https://example.org/file.json", destfile, label = "1",
      download = fake$download
    )
  )
  expect_true(file.exists(destfile))
  expect_length(fake$calls(), 3)
  expect_length(msgs, 2)
  expect_true(all(grepl("failed for form data ID 1: .*403 Forbidden", msgs)))
  ## The URL itself is not logged
  expect_false(any(grepl("example.org", msgs)))
})

test_that("download_with_retry gets a fresh URL from a function for each attempt", { # nolint
  fake <- fake_download(failures = 2)
  n <- 0
  source <- function() {
    n <<- n + 1
    paste0("https://example.org/file.json?attempt=", n)
  }
  suppressMessages(
    stopadforms:::download_with_retry(
      source, tempfile(), label = "1", download = fake$download
    )
  )
  expect_equal(
    fake$calls(),
    paste0("https://example.org/file.json?attempt=", 1:3)
  )
})

test_that("download_with_retry counts a failure to get a URL as a failed attempt", { # nolint
  fake <- fake_download(failures = 0)
  n <- 0
  source <- function() {
    n <<- n + 1
    if (n == 1) stop("Synapse returned no pre-signed URL")
    "https://example.org/file.json"
  }
  msgs <- capture_messages(
    stopadforms:::download_with_retry(
      source, tempfile(), label = "1", download = fake$download
    )
  )
  expect_length(fake$calls(), 1)
  expect_true(grepl("no pre-signed URL", msgs[1]))
})

test_that("download_with_retry errors after the last failed attempt", {
  fake <- fake_download(failures = Inf)
  expect_error(
    suppressMessages(
      stopadforms:::download_with_retry(
        "https://example.org/file.json", tempfile(), label = "1",
        download = fake$download
      )
    ),
    "download failed after 3 attempts: .*403 Forbidden"
  )
  expect_length(fake$calls(), 3)
})

test_that("download_with_retry makes one attempt if the first succeeds", {
  fake <- fake_download(failures = 0)
  expect_silent(
    stopadforms:::download_with_retry(
      "https://example.org/file.json", tempfile(), label = "1",
      download = fake$download
    )
  )
  expect_length(fake$calls(), 1)
})

test_that("download_with_retry keeps the end of long download.file() messages", { # nolint
  ## Pre-signed URLs are long, and download.file() puts the useful part of its
  ## message (the HTTP status, or here the reason) after the URL. R's default
  ## 1000-byte message limit cut that part off.
  long_url <- paste0("file:///nonexistent/", strrep("x", 1500), ".json")
  expect_error(
    suppressMessages(stopadforms:::download_with_retry(
      long_url, tempfile(), label = "1", attempts = 1
    )),
    "No such file or directory"
  )
})

test_that("download_with_retry restores the message length limit", {
  before <- getOption("warning.length")
  during <- NULL
  fake <- function(url, destfile, quiet) {
    during <<- getOption("warning.length")
    writeLines("{}", destfile)
  }
  stopadforms:::download_with_retry(
    "https://example.org/file.json", tempfile(), label = "1", download = fake
  )
  expect_equal(during, 8170)
  expect_equal(getOption("warning.length"), before)
})

# get_presigned_url() ----------------------------------------------------------

## A fake Synapse client whose restPOST() returns `response` and records the
## request body
fake_syn <- function(response) {
  bodies <- character(0)
  list(
    restPOST = function(uri, body) {
      bodies <<- c(bodies, body)
      response
    },
    bodies = function() bodies
  )
}

test_that("get_presigned_url returns the pre-signed URL", {
  syn <- fake_syn(list(requestedFiles = list(list(
    preSignedURL = "https://data.prod.sagebase.org/file.json"
  ))))
  url <- stopadforms:::get_presigned_url(syn, "94297170", "41")
  expect_equal(url, "https://data.prod.sagebase.org/file.json")
  body <- jsonlite::fromJSON(syn$bodies())
  expect_equal(body$requestedFiles$fileHandleId, "94297170")
  expect_equal(body$requestedFiles$associateObjectId, "41")
  expect_equal(body$requestedFiles$associateObjectType, "FormData")
  expect_true(body$includePreSignedURLs)
})

test_that("get_presigned_url errors with Synapse's failure code if there is no URL", { # nolint
  syn <- fake_syn(list(requestedFiles = list(list(
    fileHandleId = "94297170", failureCode = "UNAUTHORIZED"
  ))))
  expect_error(
    stopadforms:::get_presigned_url(syn, "94297170", "41"),
    "no pre-signed URL for form data ID 41 \\(failure code: UNAUTHORIZED\\)"
  )
})

test_that("get_presigned_url sends valid JSON with the IDs as strings", {
  syn <- fake_syn(list(requestedFiles = list(list(
    preSignedURL = "https://data.prod.sagebase.org/file.json"
  ))))
  ## Numbers are sent as strings
  stopadforms:::get_presigned_url(syn, 94297170, 41)
  ## Characters that would break hand-built JSON are escaped
  stopadforms:::get_presigned_url(syn, 'a"b}', "c\\d")
  bodies <- lapply(syn$bodies(), jsonlite::fromJSON)
  expect_identical(bodies[[1]]$requestedFiles$fileHandleId, "94297170")
  expect_identical(bodies[[1]]$requestedFiles$associateObjectId, "41")
  expect_identical(bodies[[2]]$requestedFiles$fileHandleId, 'a"b}')
  expect_identical(bodies[[2]]$requestedFiles$associateObjectId, "c\\d")
  ## requestedFiles is a JSON array, and the flags are booleans
  expect_true(grepl('"requestedFiles":\\[\\{', syn$bodies()[[1]]))
  expect_identical(bodies[[1]]$includePreSignedURLs, TRUE)
  expect_identical(bodies[[1]]$includeFileHandles, FALSE)
})

test_that("get_presigned_url errors helpfully if Synapse returns no requested files", { # nolint
  syn <- fake_syn(list(requestedFiles = list()))
  expect_error(
    stopadforms:::get_presigned_url(syn, "94297170", "41"),
    "no pre-signed URL for form data ID 41 \\(failure code: none\\)"
  )
})

# get_submissions() ------------------------------------------------------------

## A fake Synapse client for the forms service. restPOST() returns
## `pages_by_state[[state]]` for submission list requests, and a pre-signed URL
## of "https://example.org/<file handle ID>/<form data ID>" for file requests.
## Records every request.
fake_forms_syn <- function(pages_by_state) {
  requests <- list()
  list(
    restPOST = function(uri, body) {
      request <- jsonlite::fromJSON(body)
      requests[[length(requests) + 1]] <<- list(uri = uri, request = request)
      if (grepl("/form/data/list", uri)) {
        page <- pages_by_state[[request$filterByState]]
        list(page = if (is.null(page)) list() else page)
      } else {
        files <- request$requestedFiles
        list(requestedFiles = list(list(preSignedURL = paste0(
          "https://example.org/", files$fileHandleId, "/", files$associateObjectId
        ))))
      }
    },
    requests = function() requests
  )
}

## A submission's metadata as the forms service lists it
fake_submission <- function(form_data_id, file_handle_id, name, submitted_on,
                            state = "SUBMITTED_WAITING_FOR_REVIEW") {
  list(
    formDataId = form_data_id,
    name = name,
    dataFileHandleId = file_handle_id,
    submissionStatus = list(submittedOn = submitted_on, state = state)
  )
}

file_requests <- function(syn) {
  Filter(function(r) grepl("/fileHandle/batch", r$uri), syn$requests())
}

test_that("get_submissions gives each submission a URL source for its own IDs", { # nolint
  syn <- fake_forms_syn(list(SUBMITTED_WAITING_FOR_REVIEW = list(
    fake_submission("774", "177144258", "CNS4.json", "2026-09-09T18:39:03.067Z"),
    fake_submission("481", "160000001", "EPGN2742.json", "2024-08-01T21:41:15.412Z"),
    fake_submission("503", "160000002", "Nezavist.json", "2025-01-13T23:03:34.906Z")
  )))
  sources <- get_submissions(syn, group = 9, statuses = "SUBMITTED_WAITING_FOR_REVIEW")

  expect_equal(names(sources), c("774", "481", "503"))
  expect_true(all(vapply(sources, is.function, logical(1))))
  ## No URLs are requested until they're needed
  expect_length(file_requests(syn), 0)

  ## Call them in reverse order, so mixed-up IDs can't pass by coincidence
  expect_equal(sources[["503"]](), "https://example.org/160000002/503")
  expect_equal(sources[["481"]](), "https://example.org/160000001/481")
  expect_equal(sources[["774"]](), "https://example.org/177144258/774")

  expect_equal(attr(sources[["774"]], "form_name"), "CNS4.json")
  expect_equal(attr(sources[["774"]], "submitted_on"), "2026-09-09T18:39:03.067Z")
  expect_equal(attr(sources[["481"]], "form_name"), "EPGN2742.json")
  expect_equal(attr(sources[["481"]], "submitted_on"), "2024-08-01T21:41:15.412Z")
  expect_equal(attr(sources[["503"]], "form_name"), "Nezavist.json")
  expect_equal(attr(sources[["503"]], "submitted_on"), "2025-01-13T23:03:34.906Z")

  ## Each call requests a fresh URL
  sources[["774"]]()
  expect_length(file_requests(syn), 4)
})

test_that("get_submissions combines submissions from several states", {
  syn <- fake_forms_syn(list(
    SUBMITTED_WAITING_FOR_REVIEW = list(
      fake_submission("774", "177144258", "CNS4.json", "2026-09-09T18:39:03.067Z")
    ),
    REJECTED = list(
      fake_submission("468", "150000000", "Notum.json", "2024-07-01T12:46:28.774Z",
                      state = "REJECTED")
    )
  ))
  sources <- get_submissions(
    syn, group = 9, statuses = c("SUBMITTED_WAITING_FOR_REVIEW", "REJECTED")
  )

  expect_equal(names(sources), c("774", "468"))
  expect_equal(sources[["468"]](), "https://example.org/150000000/468")
  list_states <- vapply(
    Filter(function(r) grepl("/form/data/list", r$uri), syn$requests()),
    function(r) r$request$filterByState,
    character(1)
  )
  expect_equal(list_states, c("SUBMITTED_WAITING_FOR_REVIEW", "REJECTED"))
})

test_that("get_submissions returns NULL if there are no submissions", {
  syn <- fake_forms_syn(list())
  expect_null(get_submissions(
    syn, group = 9, statuses = c("SUBMITTED_WAITING_FOR_REVIEW", "REJECTED")
  ))
  expect_null(get_submissions(syn, group = 9, statuses = NULL))
})

# format_failed_submissions() --------------------------------------------------

test_that("format_failed_submissions labels submissions with their names where known", { # nolint
  expect_equal(
    stopadforms:::format_failed_submissions(
      c("774", "3"), c("CNS4.json", NA_character_)
    ),
    c("Title: CNS4, ID: 774", "ID: 3")
  )
  ## Only a trailing ".json" is removed (in any case); a bare ".json" leaves
  ## no name, so just the ID is shown
  expect_equal(
    stopadforms:::format_failed_submissions(
      c("1", "2", "3"), c("Pagano Lab Inhibitors.JSON", "data.json.backup", ".json")
    ),
    c("Title: Pagano Lab Inhibitors, ID: 1", "Title: data.json.backup, ID: 2",
      "ID: 3")
  )
  expect_equal(
    stopadforms:::format_failed_submissions("774"),
    "ID: 774"
  )
})

test_that("format_failed_submissions ignores names that don't match the IDs", { # nolint
  ## Too few names would otherwise be recycled onto the wrong IDs
  expect_warning(
    labels <- stopadforms:::format_failed_submissions(
      c("774", "3", "468"), c("CNS4.json", "other.json")
    ),
    "got 2 names for 3 form data IDs"
  )
  expect_equal(labels, c("ID: 774", "ID: 3", "ID: 468"))
  expect_warning(
    labels <- stopadforms:::format_failed_submissions("774", c("a.json", "b.json")),
    "got 2 names for 1 form data IDs"
  )
  expect_equal(labels, "ID: 774")
})

file.remove("malformed.json")

# clean up test1.json file
file.remove("test1.json")

# create_section_table() -------------------------------------------------------

# Convert sample JSON to list
dat_list <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

test_that("create_section_table doesn't print anything", {
  ## Only printed output matters here, not (e.g. deprecation) warnings
  out <- capture.output(invisible(suppressWarnings(
    stopadforms:::create_section_table(
      dat_list[["pk_in_vitro"]],
      "pk_in_vitro",
      lookup_table = lookup_table
    )
  )))
  expect_length(out, 0)
})

test_that("create_section_table creates rows for each response", {
  res <- stopadforms:::create_section_table(
    dat_list[["pk_in_vitro"]],
    names(dat_list[["pk_in_vitro"]]),
    lookup_table = lookup_table
  )
  expect_true(inherits(res, "data.frame"))
  expect_true(nrow(res) == 1)
})

test_that("create_section_table returns NULL if no data", {
  res <- stopadforms:::create_section_table(
    dat_list[["binding"]],
    names(dat_list[["binding"]]),
    lookup_table = lookup_table
  )
  expect_null(res)
})

test_that("create_section_table gives experiments a number", {
  res <- stopadforms:::create_section_table(
    dat_list[["chronic_dosing"]],
    names(dat_list[["chronic_dosing"]]),
    lookup_table = lookup_table
  )
  expect_equal(range(res$exp_num), c(1, 2))
})


test_that("create_section_table does not give experiment number if no experiments", {
  res <- stopadforms:::create_section_table(
    dat_list[["pk_in_vitro"]],
    names(dat_list[["pk_in_vitro"]]),
    lookup_table = lookup_table
  )
  expect_true(is.na(res$exp_num))
})

test_that("create_section_table returns multiple selections from responses", {
  res <- stopadforms:::create_section_table(
    dat_list[["chronic_dosing"]],
    names(dat_list[["chronic_dosing"]]),
    lookup_table = lookup_table
  )
  routes <- res %>%
    dplyr::filter(variable ==  "route")
  
  expect_equal(
    routes[routes$exp_num == 1, "response", drop = TRUE],
    "sublingual, injection, transdermal"
  )
  expect_equal(
    routes[routes$exp_num == 2, "response", drop = TRUE],
    "oral, injection, transdermal, formulated_in_food"
  )
})

test_that("create_section_table finds experiments in binding and efficacy", {
  json <- '
{
  "naming": {
    "compound_name": "test",
    "first_name": "Kara",
    "last_name": "Woo"
  },
  "binding": {
    "cell_line_binding": [
      {
        "name": "binding experiment 1",
        "cell_line": "iPSCs",
        "assay_description": "receptor binding",
        "binding_affinity": "10",
        "binding_affinity_constant": "Ki"
      },
      {},
      {
        "name": "binding experiment 2",
        "cell_line": "CHO cells",
        "assay_description": "ligand binding",
        "binding_affinity": "20",
        "binding_affinity_constant": "Km"
      }
    ]
  },
  "efficacy": {
    "cell_line_efficacy": [
      {
        "name": "efficacy experiment 1",
        "cell_line": "iPSC",
        "outcome_measures": "none",
        "efficacy_measure": "10",
        "efficacy_measure_type": "EC50"
      },
      {},
      {}
    ]
  }
}
'
  dat_list <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  res1 <- stopadforms:::create_section_table(
    dat_list[["binding"]],
    names(dat_list[["binding"]]),
    lookup_table = lookup_table
  )
  res2 <- stopadforms:::create_section_table(
    dat_list[["efficacy"]],
    names(dat_list[["efficacy"]]),
    lookup_table = lookup_table
  )
  expect_equal(unique(res1$exp_num), c(1, 2))
  expect_equal(unique(res2$exp_num), c(1))
})

# create_values_table() --------------------------------------------------------



test_that("create_values_table turns sub-list into tibble", {
  lookup_table <- tibble::tibble(
    section = "pk_in_vitro",
    step = "PK In Vitro",
    variable = "permeability",
    label = "Permeability"
  )
  res <- stopadforms:::create_values_table(
    dat_list[[1]],
    section = names(dat_list[1]),
    lookup_table = lookup_table
  )
  expected <- tibble::tibble(
    section = "pk_in_vitro",
    variable = "permeability",
    response = "super permeable",
    exp_num = NA
  )
  expect_identical(res, expected)
})

test_that("create_values_table doesn't add extra fields if complete = FALSE", {
  lookup_table <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50", "LD50"),
    variable = c("reference", "duration"),
    label = c("Provide a reference", "Duration")
  )
  res <- stopadforms:::create_values_table(
    list(duration = 10),
    section = "ld50",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_equal(nrow(res), 1)
  expect_equal(res$variable, "duration")
})

test_that("create_values_table combines routes", {
  dat <- list(
    drug_formulation = "foo",
    route = list("oral", "sublingual")
  )
  lookup_table <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50", "LD50"),
    variable = c("drug_formulation", "route"),
    label = c("Drug Formulation", "What was the route of administration?")
  )
  res <- stopadforms:::create_values_table(dat, section = "ld50", lookup_table = lookup_table)
  expect_true("route" %in% res$variable)
  expect_false("route1" %in% res$variable)
  expect_true("oral, sublingual" %in% res$response)
})

test_that("create_values_table doesn't combine other nested things like age range", {
  dat <- list(
    name = "My cool experiment",
    age_range = list(age_range_min = 18, age_range_max = 90)
  )
  lookup_table <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50", "LD50"),
    variable = c("drug_formulation", "name"),
    label = c("Drug Formulation", "Experiment Name")
  )
  res <- stopadforms:::create_values_table(dat, section = "ld50", lookup_table = lookup_table)
  expect_true("age_range.age_range_max" %in% res$variable)
  expect_true("age_range.age_range_min" %in% res$variable)
  expect_false("age_range" %in% res$variable)
})

# change_logical_responses() ---------------------------------------------------

test_that("change_logical_responses() fixes responses to yes/no", {
  data <- tibble::tibble(
    variable = c("is_solution", "is_solution", "is_solution", "is_compound"),
    response = c("0", "1", "TRUE", "FALSE")
  )
  res <- stopadforms:::change_logical_responses(data)
  expect_equal(res$response, c("No", "Yes", "Yes", "No"))
})

test_that("change_logical_responses() changes correct rows", {
  data <- tibble::tibble(
    variable = c("name", "species", "is_solution", "is_solution"),
    response = c("foo", "mouse", "0", "1")
  )
  res <- stopadforms:::change_logical_responses(data)
  expect_equal(res$response, c("foo", "mouse", "No", "Yes"))
})


# add_section_variables() ------------------------------------------------------

test_that("add_section_variables() adds extra sections", {
  lookup_table <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50", "LD50"),
    variable = c("reference", "duration"),
    label = c("Provide a reference", "Duration")
  )
  dat <- tibble::tibble(section = "ld50", variable = "duration", response = 10)
  res <- stopadforms:::add_section_variables(dat, lookup_table)
  expect_equal(res$variable, c("duration", "reference"))
})

# map_names() ------------------------------------------------------------------

lookup_table <- tibble::tibble(
  section = c("pk_in_vitro", "naming"),
  step = c("PK In Vitro", "Naming"),
  variable = c("permeability", "first_name"),
  label = c("Permeability", "First Name")
)

test_that("map_names() maps correct fields", {
  dat <- tibble::tibble(
    section = "pk_in_vitro",
    variable = "permeability",
    response = "super permeable"
  )
  res <- stopadforms:::map_names(dat, lookup_table = lookup_table, complete = TRUE)
  expect_equal(res$label, c("Permeability", "First Name"))
})

test_that("map_names() maps only given rows if complete = FALSE", {
  dat <- tibble::tibble(
    section = "pk_in_vitro",
    variable = "permeability",
    response = "super permeable"
  )
  res <- stopadforms:::map_names(dat, lookup_table = lookup_table, complete = FALSE)
  expect_equal(nrow(res), 1)
  expect_equal(res$label, "Permeability")
})

test_that("map_names() leaves sections and variables that don't map intact", {
  dat <- tibble::tibble(
    section = "foo",
    variable = "bar",
    exp_num = NA,
    response = "baz"
  )
  res <- stopadforms:::map_names(dat, lookup_table, complete = FALSE)
  expect_equal(res$step, "foo")
  expect_equal(res$label, "bar")
})

test_that("Step is added even if variable isn't in lookup table", {
  dat <- tibble::tibble(
    section = "ld50",
    variable = "other_species",
    response = "gremlins"
  )
  lookup_table <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50", "LD50"),
    variable = c("reference", "duration"),
    label = c("Provide a reference", "Duration")
  )
  res <- stopadforms:::map_names(dat, lookup_table, complete = FALSE)
  expect_equal(res$step, "LD50")
})

# append_exp_nums() ------------------------------------------------------------

test_that("append_exp_nums() adds number to step column", {
  dat <- tibble::tibble(section = c("ld50", "ld50"), 
                        step = c("LD50", "LD50"),
                        exp_num = c(1, 2))
  res <- stopadforms:::append_exp_nums(dat)
  expected <- tibble::tibble(
    section = c("ld50", "ld50"),
    step = c("LD50 [1]", "LD50 [2]"),
    exp_num = c(1, 2)
  )
  expect_equal(res, expected)
})

# therapeutic_approach_response() ----------------------------------------------

test_that("therapeutic_approach_response() renames 'both'", {
  dat <- tibble::tibble(variable = "therapeutic_approach", response = "both")
  res <- stopadforms:::therapeutic_approach_response(dat)
  expect_equal(
    res,
    tibble::tibble(
      variable = "therapeutic_approach",
      response = "prophylactic, symptomatic"
    )
  )
})

# combine_route_responses() ----------------------------------------------------

test_that("combine_route_responses combines routes if present", {
  dat1 <- list(route = "a")
  dat2 <- list(route = list("a"))
  dat3 <- list(route = list("a", "b"))
  expect_equal(stopadforms:::combine_route_responses(dat1), list(route = "a"))
  expect_equal(stopadforms:::combine_route_responses(dat2), list(route = "a"))
  expect_equal(stopadforms:::combine_route_responses(dat3), list(route = "a, b"))
})

test_that("combine_route_responses returns orig. data if no route present ", {
  dat <- list(not_a_route = list("a", "b"))
  expect_equal(stopadforms:::combine_route_responses(dat), list(not_a_route = list("a", "b")))
})


# remove_empty_objects() ----------------------------------------------
test_that("remove_empty_objects removes all empty inner cell_line_efficacy objects", {
  json <- '
{
  "efficacy": {
    "cell_line_efficacy": [
        {},
        {},
        {}
    ]
  }
}
'
  
  data_in <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  data_out <- stopadforms:::remove_empty_objects(data_in)
  
  expect_equal(length(data_in$efficacy[[1]]), 3)
  expect_equal(length(data_out$efficacy[[1]]), 0)
  
})

test_that("remove_empty_objects removes all empty inner cell_line_binding objects", {
  json <- '
{
   "binding": {
    "cell_line_binding": [
      {},
      {}, 
      {},
      {}
    ]
   }
}
'
  
  data_in <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  data_out <- stopadforms:::remove_empty_objects(data_in)
  
  expect_equal(length(data_in$binding[[1]]), 4)
  expect_equal(length(data_out$binding[[1]]), 0)
})

test_that("remove_empty_objects removes all empty inner age_range objects", {
  json <- '
{
  "pk_in_vivo": {
    "experiments": [
      {
        "age_range": {}
      },
      {
        "age_range": {}
      },
      {
        "age_range": {}
      },
      {
        "age_range": {}
      },
      {
        "age_range": {}
      }
    ]
  }
}
'
  
  data_in <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  data_out <- stopadforms:::remove_empty_objects(data_in)
  
  expect_equal(length(data_in$pk_in_vivo[[1]]), 5)
  expect_equal(length(data_out$pk_in_vivo[[1]]), 0)
})



test_that("remove_empty_objects removes only empty inner cell_line_efficacy objects", {
  json <- '
{
  "efficacy": {
    "cell_line_efficacy": [
        {},
        {
          "name": "efficacy experiment 1",
          "cell_line": "iPSC",
          "outcome_measures": "none",
          "efficacy_measure": "10",
          "efficacy_measure_type": "EC50"
        },
        {}
    ]
  }
}
'
  
  data_in <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  data_out <- stopadforms:::remove_empty_objects(data_in)
  
  expect_equal(length(data_in$efficacy[[1]]), 3)
  expect_equal(length(data_out$efficacy[[1]]), 1)

})

test_that("remove_empty_objects removes only empty inner cell_line_binding objects", {
  json <- '
{
   "binding": {
      "cell_line_binding": [
        {},
        {
          "name": "binding experiment 2",
          "cell_line": "CHO cells",
          "assay_description": "ligand binding",
          "binding_affinity": "20",
          "binding_affinity_constant": "Km"
        }, 
        {},
        {}
      ]
   }
}
' 
  
  data_in <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  data_out <- stopadforms:::remove_empty_objects(data_in)
  
  expect_equal(length(data_in$binding[[1]]), 4)
  expect_equal(length(data_out$binding[[1]]), 1)
})

test_that("remove_empty_objects removes only empty inner age_range objects", {
  json <- '
{
  "pk_in_vivo": {
    "experiments": [
      {
        "age_range": {}
      },
      {
          "age_range":{
            "age_range_min":6,
            "age_range_max":24
        }
      },
      {
        "age_range": {}
      },
      {
        "age_range": {}
      },
      {
          "age_range":{
            "age_range_min":1,
            "age_range_max":6
          }
      }
    ]
  }
}
'
  
  data_in <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)
  data_out <- stopadforms:::remove_empty_objects(data_in)
  
  expect_equal(length(data_in$pk_in_vivo[[1]]), 5)
  expect_equal(length(data_out$pk_in_vivo[[1]]), 2)
})

options(old_options)
