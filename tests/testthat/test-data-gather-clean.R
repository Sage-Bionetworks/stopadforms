context("data-gather-clean.R")

lookup_table <- tibble::tribble(
  ~section, ~step, ~variable, ~label,
  "basic", "Basic Data", "age_range_min", "Min of age range",
  "basic", "Basic Data", "age_range_max", "Max of age range",
  "naming", "Naming", "last_name", "Last name",
  "naming", "Naming", "first_name", "First name",
  "naming", "Naming", "compound_name", "Compound name",
  "pk_in_vivo", "Pk in Vivo", "name", "Experiment Name",
  "pk_in_vivo", "Pk in Vivo", "is_solution", "Is compound a solution?"
)

data1 <- tibble::tibble(
  section = c("basic", "naming", "pk_in_vivo", "pk_in_vivo", "pk_in_vivo"),
  variable = c("age_range_min", "last_name", "name", "route1", "is_solution"),
  exp_num = c(NA, NA, 1, 2, 1),
  response = c("3", "Smith", "Exp 1", "TRUE", "0")
)

test_that("map_sections_variables() maps correctly", {
  expected <- tibble::tibble(
    section = c("basic", "naming", "pk_in_vivo", "pk_in_vivo", "pk_in_vivo"),
    variable = c("age_range_min", "last_name", "name", "route1", "is_solution"),
    exp_num = c(NA, NA, 1, 2, 1),
    response = c("3", "Smith", "Exp 1", "TRUE", "0"),
    step = c("Basic Data", "Naming", "Pk in Vivo [1]", "Pk in Vivo [2]",
             "Pk in Vivo [1]"),
    label = c("Min of age range", "Last name", "Experiment Name",
              "route1", "Is compound a solution?")
  )
  res <- map_sections_variables(data1, lookup_table)
  expect_equal(res, expected)
})

test_that("change_logical_responses() fixes responses to yes/no", {
  data <- tibble::tibble(
    variable = c("is_solution", "is_solution", "is_solution", "is_compound"),
    response = c("0", "1", "TRUE", "FALSE")
  )
  res <- change_logical_responses(data)
  expect_equal(res$response, c("No", "Yes", "Yes", "No"))
})

## Sample JSON data to test with
json <- '
{
  "pk_in_vitro": {
    "permeability": "super permeable"
  },
  "binding": null,
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
## Convert to list
dat_list <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

test_that("create_table_by_sections creates rows for each response", {
  res <- create_table_by_sections(
    dat_list[["pk_in_vitro"]],
    names(dat_list[["pk_in_vitro"]])
  )
  expect_true(inherits(res, "data.frame"))
  expect_true(nrow(res) == 1)
})

test_that("create_table_by_sections returns NULL if no data", {
  res <- create_table_by_sections(
    dat_list[["binding"]],
    names(dat_list[["binding"]])
  )
  expect_null(res)
})

test_that("create_table_by_sections gives experiments a number", {
  res <- create_table_by_sections(
    dat_list[["chronic_dosing"]],
    names(dat_list[["chronic_dosing"]])
  )
  expect_equal(range(res$exp_num), c(1, 2))
})

test_that("create_table_by_sections returns multiple selections from responses", {
  res <- create_table_by_sections(
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
