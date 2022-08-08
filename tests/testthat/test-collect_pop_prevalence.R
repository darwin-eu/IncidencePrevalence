
test_that("mock db: check output format", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop
  )

  expect_true(all(c(
    "incidence_analysis_id",
    "numerator", "denominator",
    "prev",
    "prev_low",
    "prev_high",
    "calendar_month", "calendar_year",
    "required_days_prior_history",
    "age_strata", "sex_strata",
    "period",
    "time_interval",
    "confidence_interval",
    "cohort_id_outcome",
    "cohort_id_denominator_pop",
    "minimum_representative_proportion",
    "minimum_event_count",
    "result_obscured"
  ) %in%
    names(prev)))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: checks on working example", {
  library(DBI)
  library(dplyr)
  library(tibble)

  person <- tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2008-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    ),
    cohort_end_date = c(
      as.Date("2008-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    )
  )

  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop
  )
  expect_true(nrow(prev)>=1)

  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check minimum counts", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop,
    minimum_event_count = NULL
  )
  expect_true(any(c(0:4) %in% prev$numerator))

  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop,
    minimum_event_count = 5
  )
  expect_true(!any(c(0:4) %in% prev$numerator))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check conversion of user inputs", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  prev<-collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = 1,
    cohort_ids_denominator_pops = 1,
    study_denominator_pop = dpop
  )
  expect_true(nrow(prev)>=1)


  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check expected errors", {
  library(DBI)
  library(dplyr)
  library(tibble)

  person <- tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    )
  )
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  expect_error(collect_pop_prevalence(
    db = "a",
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = 1,
    cohort_ids_denominator_pops = 1,
    study_denominator_pop = dpop
  ))




  DBI::dbDisconnect(db, shutdown=TRUE)
})

