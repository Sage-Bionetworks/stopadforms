context("mod-review-section.R")

test_that("reorder_steps puts steps in correct order", {
  test <- c(
    "Efficacy",
    "Naming",
    "LD50 [1]",
    "LD50 [2]",
    "Clinical Data [1]",
    "Measurements",
    "Acute Dosing"
  )
  expect_identical(
    stopadforms:::reorder_steps(test),
    c(
      "Naming",
      "Measurements",
      "Efficacy",
      "LD50 [1]",
      "LD50 [2]",
      "Acute Dosing",
      "Clinical Data [1]"
    )
  )
})

test_that("reorder_steps puts any unexpected sections at the end", {
  test <- c("nope", "LD50", "also no")
  expect_identical(
    stopadforms:::reorder_steps(test),
    c("LD50", "nope", "also no")
  )
})

test_that("reorder_steps doesn't fail even if no sections match", {
  expect_identical(
    stopadforms:::reorder_steps(c("a", "b")),
    c("a", "b")
  )
})
