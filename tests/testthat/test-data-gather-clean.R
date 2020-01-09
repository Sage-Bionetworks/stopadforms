context("data-gather-clean.R")

data <- tibble::tribble(
  ~form_data_id, ~step, ~section, ~variable, ~response,
  1, "LD50", "ld50", "experiments.name", "exp",
  1, "LD50", "ld50", "experiments.name1", "exp1",
  1, "LD50", "ld50", "experiments.name10", "exp10",
  1, "LD50", "ld50", "experiments.name50", "exp50",
  1, "LD50", "ld50", "experiments.ed50", 1,
  1, "LD50", "ld50", "experiments.ed501", 1,
  1, "LD50", "ld50", "experiments.ed5010", 1,
  1, "LD50", "ld50", "experiments.ed5050", 1,
  1, "LD50", "ld50", "experiments.ed5051", 1,
  1, "LD50", "ld50", "experiments.ld50", 1,
  1, "LD50", "ld50", "experiments.ld5051", 1,
  1, "Basic Data", "basic", "name", "myname",
  1, "Basic Data", "basic", "route", "route1",
  1, "Basic Data", "basic", "route2", "route2"
)

test_that("clean_experiment_variables returns correct steps and variables", {
  cleaned_data <- tibble::tribble(
    ~form_data_id, ~step, ~section, ~variable, ~response,
    1, "LD50", "ld50", "experiments.name", "exp",
    1, "LD50 [1]", "ld50", "experiments.name", "exp1",
    1, "LD50 [10]", "ld50", "experiments.name", "exp10",
    1, "LD50 [50]", "ld50", "experiments.name", "exp50",
    1, "LD50", "ld50", "experiments.ed50", 1,
    1, "LD50 [1]", "ld50", "experiments.ed50", 1,
    1, "LD50 [10]", "ld50", "experiments.ed50", 1,
    1, "LD50 [50]", "ld50", "experiments.ed50", 1,
    1, "LD50 [51]", "ld50", "experiments.ed50", 1,
    1, "LD50", "ld50", "experiments.ld50", 1,
    1, "LD50 [51]", "ld50", "experiments.ld50", 1,
    1, "Basic Data", "basic", "name", "myname",
    1, "Basic Data", "basic", "route", "route1",
    1, "Basic Data", "basic", "route2", "route2"
  )
  res <- clean_experiment_variables(data)
  expect_equal(res, cleaned_data)
})
