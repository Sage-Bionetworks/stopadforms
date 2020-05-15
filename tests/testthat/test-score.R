context("score.R")

# calculate_submission_score() -------------------------------------------------

test_that("calculate_submission_score returns expected values", {
  reviews_table <- tibble::tribble(
           ~step, ~score, ~species,
    "Basic Data",   0.85,       NA
  )
  dat <- tibble::tribble(
    ~section,        ~step,   ~exp_num,              ~variable,      ~response,
    "naming",    "Naming", NA_integer_,         "is_off_label",          "Yes",
    "basic", "Basic Data", NA_integer_, "therapeutic_approach", "prophylactic",
    "basic", "Basic Data", NA_integer_,      "moa_description",         "test"
  )
  expect_equal(
    calculate_submission_score(
      data = dat,
      reviews = reviews_table,
      lookup = partial_betas
    ),
    0.108181818181818
  )
})

test_that("If no reviews, calculate_submission_score returns 0", {
  reviews_table <- tibble::tibble(
    step = character(0),
    score = character(0),
    species = character(0)
  )
  dat <- tibble::tribble(
    ~section,        ~step,   ~exp_num,              ~variable,     ~response,
    "naming",    "Naming", NA_integer_,         "is_off_label",          "Yes",
    "basic", "Basic Data", NA_integer_, "therapeutic_approach", "prophylactic",
    "basic", "Basic Data", NA_integer_,      "moa_description",         "test"
  )
  expect_equal(
    calculate_submission_score(
      data = dat,
      reviews = reviews_table,
      lookup = partial_betas
    ),
    0
  )
})

test_that("clinical variable is identified and used" , {
  dat1 <- tibble::tribble(
    ~section,      ~step, ~exp_num,      ~variable, ~response,
    "naming",   "Naming",       NA, "is_off_label",      "No",
    "ld50",   "LD50 [1]",       1L,         "ld50",      "10"
  )
  dat2 <- dplyr::mutate(
    dat1,
    response = dplyr::case_when(
      variable == "is_off_label" ~ "Yes",
      TRUE ~ response
    )
  )
  reviews_table <- tibble::tribble(
         ~step, ~score, ~species,
    "LD50 [1]",    0.1, "within"
  )
  preclinical <- calculate_submission_score(
    data = dat1,
    reviews = reviews_table,
    lookup = partial_betas
  )
  clinical <- calculate_submission_score(
    data = dat2,
    reviews = reviews_table,
    lookup = partial_betas
  )
  expect_equal(preclinical, 0.00201)
  expect_equal(clinical, 0.0040809)
})

test_that("calculate_submission_score warns if data lacks clinical information", {
  ## This should be a required field in the form, but just in case
  dat <- tibble::tribble(
    ~section,      ~step, ~exp_num,      ~variable, ~response,
    "ld50",   "LD50 [1]",       1L,         "ld50",      "10"
  )
  reviews_table <- tibble::tribble(
    ~step, ~score, ~species,
    "LD50 [1]",    0.1, "within"
  )
  expect_warning(
    calculate_submission_score(
      data = dat,
      reviews = reviews_table,
      lookup = partial_betas
    )
  )
})

# calculate_section_score() ----------------------------------------------------

test_that("calculate_section_score returns 0 if data has 0 rows", {
  dat <- tibble::tibble(
    section = character(0),
    variable = character(0),
    response = character(0),
    exp_num = character(0),
    label = character(0),
    step = character(0),
    form_data_id = character(0),
    submission = character(0)
  )
  expect_equal(calculate_section_score(dat, lookup = partial_betas), 0)
})

test_that("calculate_section_score errors if given more than 1 section", {
  dat <- tibble::tibble(
    section = c("naming", "basic"),
    variable = c("first_name", "route")
  )
  expect_error(calculate_section_score(dat, lookup = partial_betas))
})

test_that("clinical section is not scored", {
  dat <- tibble::tibble(
    section = "clinical",
    variable = "foo",
    response = "bar"
  )
  expect_equal(calculate_section_score(dat, lookup = partial_betas), 0)
})

test_that("PK sections have additional section multiplier", {
  pksil <- tibble::tibble(
    section = c("pk_in_silico", "pk_in_silico", "pk_in_silico"),
    variable = c(
      "partition_coefficient",
      "dissociation_constant",
      "molecular_weight"
    ),
    response = c("foo", "bar", "baz")
  )
  pkvit <- tibble::tibble(
    section = "pk_in_vitro",
    variable = "lipophilicity",
    response = "foo"
  )
  expect_equal(
    calculate_section_score(pksil, partial_betas, score = 0.25),
    0.0425 # 0.17 section multiplier * 0.25 score
  )
  expect_equal(
    calculate_section_score(pkvit, partial_betas, score = 0.1),
    0.033 # .33 section multiplier * .1 score
  )
})

test_that("certain sections don't use clinical/preclinical multiplier", {
  nam <- tibble::tibble(section = "naming", variable = "data_sharing")
  bas <- tibble::tibble(
    section = c("basic", "basic"),
    variable = c("moa_description", "therapeutic_approach"),
    response = c("foo", "prophylactic")
  )
  expect_equal(
    calculate_section_score(nam, partial_betas, score = 0.85, clinical = 0.67),
    0.85
  )
  expect_equal(
    calculate_section_score(bas, partial_betas, score = 1, clinical = 0.67),
    1.4 # 0.4 for prophylactic + 1 for well-defined mechanism of action
  )
})

test_that("therapeutic approach values are scored", {
  dat1 <- tibble::tibble(
    section = "basic",
    variable = "therapeutic_approach",
    response = "prophylactic"
  )
  dat2 <- tibble::tibble(
    section = "basic",
    variable = "therapeutic_approach",
    response = "unknown"
  )
  expect_equal(calculate_section_score(dat1, partial_betas, score = 1), 0.4)
  expect_equal(calculate_section_score(dat2, partial_betas, score = 1), 0.1)
})

test_that("efficacy measure type values are scored", {
  dat1 <- tibble::tibble(
    section = "efficacy",
    variable = "cell_line_efficacy.efficacy_measure_type",
    response = "IC50"
  )
  dat2 <- tibble::tibble(
    section = "efficacy",
    variable = "cell_line_efficacy.efficacy_measure_type",
    response = "EC50"
  )
  expect_equal(calculate_section_score(dat1, partial_betas, score = 1), 0.33)
  expect_equal(calculate_section_score(dat2, partial_betas, score = 1), 0.67)
})

test_that("the many PK in vivo betas are summed", {
  pkv <- tibble::tribble(
      ~section,                    ~variable,                 ~response,
  "pk_in_vivo",                       "name", "PK in vivo experiment 1",
  "pk_in_vivo",                    "species",                  "rabbit",
  "pk_in_vivo",                     "strain",      "C57BL/6J substrain",
  "pk_in_vivo",                        "sex",                    "both",
  "pk_in_vivo",             "clearance_rate",                      "17",
  "pk_in_vivo",       "tissue_concentration",                       "1",
  "pk_in_vivo",     "plasma_protein_binding",                     "95%",
  "pk_in_vivo", "fractional_bioavailability",                     "80%",
  "pk_in_vivo",    "age_range.age_range_min",                        NA,
  "pk_in_vivo",    "age_range.age_range_max",                        NA,
  "pk_in_vivo",                  "reference",                        NA,
  "pk_in_vivo",                  "half_life",                        NA,
  "pk_in_vivo",           "liver_metabolism",                        NA,
  "pk_in_vivo",   "plasma_max_concentration",                        NA,
  "pk_in_vivo",                     "volume",                        NA
  )
  expect_equal(calculate_section_score(pkv, partial_betas, score = 1), 0.5)
  expect_equal(calculate_section_score(pkv, partial_betas, score = .85), .425)
  expect_equal(calculate_section_score(pkv, partial_betas, score = .1), .05)
})

# approach_beta() --------------------------------------------------------------

test_that("approach_beta returns correct values", {
  expect_equal(approach_beta("prophylactic"), 0.4)
  expect_equal(approach_beta("symptomatic"), 0.2)
  expect_equal(approach_beta("prophylactic, symptomatic"), 0.3)
  expect_equal(approach_beta("both"), 0.3)
  expect_equal(approach_beta("unknown"), 0.1)
  expect_null(approach_beta(NA))
})

# efficacy_beta() --------------------------------------------------------------

test_that("efficacy_beta returns correct values", {
  expect_equal(efficacy_beta("EC50"), 0.67)
  expect_equal(efficacy_beta("IC50"), 0.33)
  expect_equal(efficacy_beta("anything else"), 0)
  expect_null(efficacy_beta(NA))
})

# calculate_denominator() ------------------------------------------------------

test_that("calculate_denominator returns greater of points in data or 11", {
  dat <- tibble::tibble(
    section = c("binding", "efficacy"),
    exp_num = c(1L, 1L)
  )
  expect_equal(calculate_denominator(dat), 11)
})

test_that("calculate_denominator adds 1 for additional experiments", {
  dat <- tibble::tibble(
    section = c("ld50", "ld50"),
    exp_num = c(1L, 2L)
  )
  expect_equal(calculate_denominator(dat), 12)
})
