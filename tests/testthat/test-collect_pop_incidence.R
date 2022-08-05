test_that("checks on mock working example", {
  library(DBI)
  library(dplyr)
  library(tibble)

  # duckdb mock database
  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
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
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome", outcome,
      overwrite = TRUE
    )
  })
  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  inc <- collect_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    outcome_washout_windows = 0,
    repetitive_events = FALSE,
    study_denominator_pop = dpop,
    time_intervals = c("Months"),
    verbose = TRUE
  )

  expect_true(all(c(
    "cohort_definition_id",
    "n_persons", "person_days",
    "person_months", "person_years", "n_events",
    "ir_100000_pys",
    "ir_100000_pys_low",
    "ir_100000_pys_high",
    "calendar_month", "calendar_year", "required_days_prior_history",
    "age_strata", "sex_strata", "outcome_washout_window",
    "repetitive_events", "time_interval", "confidence_interval",
    "cohort_id_outcome", "cohort_id_denominator_pop"
  ) %in%
    names(inc)))



  inc <- collect_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = 1,
    cohort_ids_denominator_pops = 1,
    outcome_washout_windows = NULL,
    study_denominator_pop = dpop
  )


  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("expected errors with mock", {
  library(DBI)
  library(dplyr)
  library(tibble)

  # duckdb mock database
  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
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
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome", outcome,
      overwrite = TRUE
    )
  })
  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  expect_error(collect_pop_incidence(
    db = "a",
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop
  ))




  DBI::dbDisconnect(db, shutdown=TRUE)
})
