context("data-gather-clean.R")

library(stopadforms)

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

# clean up test1.json file
file.remove("test1.json")

# create_section_table() -------------------------------------------------------

# Convert sample JSON to list
dat_list <- jsonlite::fromJSON(json, simplifyDataFrame = FALSE)

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
