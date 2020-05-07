context("score.R")

# calculate_denominator() ------------------------------------------------------

test_that("calculate_denominator returns greater of points in data or 11", {
  dat <- tibble::tibble(
    section = c("binding", "efficacy"),
    exp_num = c(1, 1)
  )
  expect_equal(calculate_denominator(dat), 11)
})

test_that("calculate_denominator adds 1 for additional experiments", {
  dat <- tibble::tibble(
    section = c("ld50", "ld50"),
    exp_num = c(1, 2)
  )
  expect_equal(calculate_denominator(dat), 12)
})
