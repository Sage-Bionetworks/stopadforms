context("data-gather-clean.R")

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

# create_table_from_json_file() ------------------------------------------------

test_that("create_table_from_json_file creates (at least) one row per row in lookup table", { # nolint
  dat <- create_table_from_json_file(
    json,
    data_id = "1",
    lookup_table = lookup_table,
    complete = TRUE
  )
  expect_true(nrow(dat) >= nrow(lookup_table))
})

test_that("All sections are represented if complete = TRUE", {
  dat <- create_table_from_json_file(
    json,
    data_id = "1",
    lookup_table = lookup_table,
    complete = TRUE
  )
  expect_true(all(lookup_table$section %in% dat$section))
})

test_that("create_table_from_json_file creates one row per response if complete = FALSE", { # nolint
  dat <- create_table_from_json_file(
    json,
    data_id = "1",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_equal(nrow(dat), 19)
})

test_that("Submission is named by user name and compound name", {
  dat <- create_table_from_json_file(
    json,
    data_id = "1",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_true(all(dat$submission == "Woo - test"))
})

test_that("Submission's data ID is added to data", {
  dat <- create_table_from_json_file(
    json,
    data_id = "1",
    lookup_table = lookup_table,
    complete = FALSE
  )
  expect_true(all(dat$form_data_id == "1"))
})

# create_section_table() -------------------------------------------------------

# Convert sample JSON to list
dat_list <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

test_that("create_section_table creates rows for each response", {
  res <- create_section_table(
    dat_list[["pk_in_vitro"]],
    names(dat_list[["pk_in_vitro"]])
  )
  expect_true(inherits(res, "data.frame"))
  expect_true(nrow(res) == 1)
})

test_that("create_section_table returns NULL if no data", {
  res <- create_section_table(
    dat_list[["binding"]],
    names(dat_list[["binding"]])
  )
  expect_null(res)
})

test_that("create_section_table gives experiments a number", {
  res <- create_section_table(
    dat_list[["chronic_dosing"]],
    names(dat_list[["chronic_dosing"]])
  )
  expect_equal(range(res$exp_num), c(1, 2))
})

test_that("create_section_table returns multiple selections from responses", {
  res <- create_section_table(
    dat_list[["chronic_dosing"]],
    names(dat_list[["chronic_dosing"]])
  )

  routes <- res %>%
    dplyr::filter(stringr::str_detect(variable, "route"))

  expect_equal(
    routes[routes$exp_num == 1, "response", drop = TRUE],
    c("sublingual", "injection", "transdermal")
  )
  expect_equal(
    routes[routes$exp_num == 2, "response", drop = TRUE],
    c("oral", "injection", "transdermal", "formulated_in_food")
  )
})

# create_values_table() --------------------------------------------------------

test_that("create_values_table turns sub-list into tibble", {
  res <- create_values_table(dat_list[[1]], section = names(dat_list[1]))
  expected <- tibble::tibble(
    section = "pk_in_vitro",
    exp_num = NA,
    variable = "permeability",
    response = "super permeable"
  )
  expect_identical(res, expected)
})

# change_logical_responses() ---------------------------------------------------

test_that("change_logical_responses() fixes responses to yes/no", {
  data <- tibble::tibble(
    variable = c("is_solution", "is_solution", "is_solution", "is_compound"),
    response = c("0", "1", "TRUE", "FALSE")
  )
  res <- change_logical_responses(data)
  expect_equal(res$response, c("No", "Yes", "Yes", "No"))
})

test_that("change_logical_responses() changes correct rows", {
  data <- tibble::tibble(
    variable = c("name", "species", "is_solution", "is_solution"),
    response = c("foo", "mouse", "0", "1")
  )
  res <- change_logical_responses(data)
  expect_equal(res$response, c("foo", "mouse", "No", "Yes"))
})


# map_sections_variables() -----------------------------------------------------

test_that("map_sections_variables() maps correctly", {
  dat <- tibble::tibble(
    section = c("naming", "chronic_dosing", "chronic_dosing"),
    variable = c("first_name", "name", "species"),
    exp_num = c(NA, 1, 1),
    response = c("Kara", "my experiment 1", "mouse")
  )
  expected <- dplyr::mutate(
    dat,
    step = c("Naming", "Chronic Dosing [1]", "Chronic Dosing [1]"),
    label = c("First Name", "Experiment Name", "Species")
    )

  res <- map_sections_variables(dat, lookup_table, complete = FALSE)
  expect_equal(res, expected)
})

test_that("map_sections_variables() leaves labels that don't map intact", {
  dat <- tibble::tibble(
    section = "naming",
    variable = "foo",
    exp_num = NA,
    response = "bar"
  )
  res <- map_sections_variables(dat, lookup_table, complete = FALSE)
  expect_equal(res$label, "foo")
})

