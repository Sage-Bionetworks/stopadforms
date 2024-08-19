context("score.R")

# calculate_submission_score() -------------------------------------------------

test_that("calculate_submission_score returns expected values", {
  reviews_table <- tibble::tibble(
    step = c("Basic Data"),
    score = c(0.85),
    species = c(NA),
    submission = c("foo"),
    form_data_id = c("1")
  )
  dat <- tibble::tibble(
    section = c("naming", "basic", "basic"),
    step = c("Naming", "Basic Data", "Basic Data"),
    exp_num = c(NA_integer_, NA_integer_, NA_integer_),
    variable = c("is_off_label", "therapeutic_approach","moa_description"),
    response = c("Yes", "prophylactic", "test"),
    form_data_id = c("1", "1", "1"),
    submission = c("foo", "foo", "foo"),
    label = c("doesn't matter", "doesn't matter", "doesn't matter")
  )
  reviews <- calculate_scores_rowwise(
    reviews_table,
    submissions = append_clinical_to_submission(dat)
  )
  expect_equal(
    stopadforms:::calculate_submission_score(
      submission = dat,
      reviews = reviews
    ),
    0.0309,
    tolerance = 0.0001
  )
})

test_that("calculate_submission_score uses geometric mean", {
  reviews_table <- tibble::tibble(
    step = c("Basic Data", "Basic Data"),
    score = c(0.85, 1),
    species = c(NA, NA),
    submission = c("foo", "foo"),
    form_data_id = c("1", "1")
  )
  dat <- tibble::tibble(
    section = c("naming", "basic", "basic"),
    step = c("Naming", "Basic Data", "Basic Data"),
    exp_num = c(NA_integer_, NA_integer_, NA_integer_),
    variable = c("is_off_label", "therapeutic_approach","moa_description"),
    response = c("Yes", "prophylactic", "test"),
    form_data_id = c("1", "1", "1"),
    submission = c("foo", "foo", "foo"),
    label = c("doesn't matter", "doesn't matter", "doesn't matter")
  )
  reviews <- calculate_scores_rowwise(
    reviews_table,
    submissions = append_clinical_to_submission(dat)
  )
  expect_equal(
    stopadforms:::calculate_submission_score(
      submission = dat,
      reviews = reviews
    ),
    0.0335,
    tolerance = 0.0001
  )
})

test_that("If no reviews, calculate_submission_score returns 0", {
  reviews_table <- tibble::tibble(
    step = character(0),
    score = character(0),
    species = character(0),
    submission = character(0),
    form_data_id = character(0)
  )
  dat <- tibble::tibble(
    section = c("naming", "basic", "basic"),
    step = c("Naming", "Basic Data", "Basic Data"),
    exp_num = c(NA_integer_, NA_integer_, NA_integer_),
    variable = c("is_off_label", "therapeutic_approach","moa_description"),
    response = c("Yes", "prophylactic", "test"),
    form_data_id = c("1", "1", "1"),
    submission = c("foo", "foo", "foo"),
    label = c("doesn't matter", "doesn't matter", "doesn't matter")
  )
  reviews <- calculate_scores_rowwise(
    reviews_table,
    submissions = append_clinical_to_submission(dat)
  )
  expect_equal(
    stopadforms:::calculate_submission_score(
      submission = dat,
      reviews = reviews_table
    ),
    0
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
    0.4 # TODO: Address: 0.4 for prophylactic + 1 for well-defined mechanism of action
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
    variable = "efficacy_measure_type",
    response = "IC50"
  )
  dat2 <- tibble::tibble(
    section = "efficacy",
    variable = "efficacy_measure_type",
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
  expect_equal(stopadforms:::approach_beta("prophylactic"), 0.4)
  expect_equal(stopadforms:::approach_beta("symptomatic"), 0.2)
  expect_equal(stopadforms:::approach_beta("prophylactic, symptomatic"), 0.3)
  expect_equal(stopadforms:::approach_beta("both"), 0.3)
  expect_equal(stopadforms:::approach_beta("unknown"), 0.1)
  expect_null(stopadforms:::approach_beta(NA))
})

# efficacy_beta() --------------------------------------------------------------

test_that("efficacy_beta returns correct values", {
  expect_equal(stopadforms:::efficacy_beta("EC50"), 0.67)
  expect_equal(stopadforms:::efficacy_beta("IC50"), 0.33)
  expect_equal(stopadforms:::efficacy_beta("anything else"), 0)
  expect_null(stopadforms:::efficacy_beta(NA))
})

# get_clinical() ---------------------------------------------------------------

test_that("get_clinical warns if data lacks clinical information", {
  ## This should be a required field in the form, but just in case
  dat <- tibble::tribble(
    ~section,      ~step, ~exp_num,      ~variable, ~response,
    "ld50",   "LD50 [1]",       1L,         "ld50",      "10"
  )
  reviews_table <- tibble::tribble(
    ~step, ~score, ~species,
    "LD50 [1]",    0.1, "within"
  )
  expect_warning(stopadforms:::get_clinical(data = dat))
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

test_that("calculate_denominator adds 0.5 for extra pk_in_vivo data", {
  dat <- tibble::tibble(
    section = c("pk_in_vivo", "pk_in_vivo"),
    exp_num = c(1L, 2L)
  )
  expect_equal(calculate_denominator(dat), 11.5)
})

# calculate_scores_rowwise() ---------------------------------------------------

test_that("if no scores, calculate_scores_rowwise() returns empty df", {
  reviews_table <- tibble::tibble(
    step = character(0),
    score = character(0),
    species = character(0),
    submission = character(0),
    form_data_id = character(0)
  )
  dat <- tibble::tibble(
    section = c("naming", "basic", "basic"),
    step = c("Naming", "Basic Data", "Basic Data"),
    exp_num = c(NA_integer_, NA_integer_, NA_integer_),
    variable = c("is_off_label",
                 "therapeutic_approach","moa_description"),
    response = c("Yes", "prophylactic", "test"),
    form_data_id = c("1", "1", "1"),
    submission = c("foo", "foo", "foo"),
    label = c("doesn't matter", "doesn't matter", "doesn't matter")
  )
  reviews <- calculate_scores_rowwise(
    reviews_table,
    submissions = append_clinical_to_submission(dat)
  )
  expect_equal(nrow(reviews), 0)
  expect_equal(reviews$weighted_score, numeric(0))
})

test_that("clinical variable is identified and used" , {
  dat1 <- tibble::tibble(
    section = c("naming", "ld50"),
    step = c("Naming", "LD50 [1]"),
    exp_num = c(NA, 1L),
    variable = c("is_off_label", "ld50"),
    response = c("No", "10"),
    label = c("doesn't matter", "doesn't matter"),
    form_data_id = c("1", "1"),
    submission = c("my submission", "my submission")
  )
  dat2 <- dplyr::mutate(
    dat1,
    response = dplyr::case_when(
      variable == "is_off_label" ~ "Yes",
      TRUE ~ response
    )
  )
  reviews_table <- tibble::tibble(
    step = c("LD50 [1]"),
    score = c(0.1),
    species = c("within"),
    form_data_id = c("1"),
    submission = c("my submission")
  )
  reviews1 <- calculate_scores_rowwise(
    reviews_table,
    submissions = append_clinical_to_submission(dat1)
  )
  reviews2 <- calculate_scores_rowwise(
    reviews_table,
    submissions = append_clinical_to_submission(dat2)
  )
  expect_equal(reviews1$weighted_score, 0.02211)
  expect_equal(reviews2$weighted_score, 0.04489)
})

# geom_mean_score() ------------------------------------------------------------

test_that("geom_mean_score calculates the geometric mean", {
  expect_equal(stopadforms:::geom_mean_score(0.85), 0.85)
  expect_equal(stopadforms:::geom_mean_score(c(1, 1, 1)), 1)
  expect_equal(stopadforms:::geom_mean_score(c(1, 2, 3)), 1.81712059283214)
  expect_equal(stopadforms:::geom_mean_score(0.85), 0.85)
})

test_that("geom_mean_score can handle zeroes", {
  expect_equal(stopadforms:::geom_mean_score(0), 0)
  expect_equal(
    stopadforms:::geom_mean_score(c(1, 2, 3, 0, 0, 0)),
    stopadforms:::geom_mean_score(c(1, 2, 3))
  )
})

test_that("geom_mean_score can handle NAs", {
  expect_equal(
    stopadforms:::geom_mean_score(c(1, 2, 3, NA, NA, NA)),
    stopadforms:::geom_mean_score(c(1, 2, 3))
  )
})
