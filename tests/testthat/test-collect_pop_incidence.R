
test_that("mock db: check output format", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  dpop<-dpop$denominator_populations

  inc <- collect_pop_incidence(
    db = db,
    results_schema_outcomes = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    outcome_washout_windows = 0,
    repetitive_events = FALSE,
    study_denominator_pop = dpop,
    time_intervals = c("months"),
    confidence_interval = "none",
    verbose = TRUE
  )

  expect_true(class(inc) == "list")
  expect_true(all(names(inc) %in%
                    c("incidence_estimates",
                    "analysis_settings",  "attrition" )))

  # check analysis settings tibble
  expect_true(all(c(
    "incidence_analysis_id",
    "cohort_id_outcome",
    "cohort_id_denominator_pop",
    "outcome_washout_window",
    "repetitive_events",
    "time_interval",
    "confidence_interval",
    "minimum_cell_count"
  ) %in%
    names(inc[["analysis_settings"]])))

  # check estimates tibble
  expect_true(all(c(
    "incidence_analysis_id",
    "n_persons",
    "person_days",
    "person_years",
    "n_events",
    "ir_100000_pys",
    "ir_100000_pys_low",
    "ir_100000_pys_high",
    "calendar_month", "calendar_year",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(inc[["incidence_estimates"]])))

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
  dpop<-dpop$denominator_populations

  inc <- collect_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    outcome_washout_windows = 0,
    repetitive_events = FALSE,
    study_denominator_pop = dpop,
    time_intervals = c("months"),
    verbose = TRUE
  )
  expect_true(nrow(inc[["incidence_estimates"]])>=1)


  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check minimum counts", {
  library(DBI)
  library(dplyr)
  library(tibble)

  #20 people
  person <- tibble(
    person_id = as.character(c(1:20)),
    gender_concept_id = rep("8507",20),
    year_of_birth =  rep(2000,20),
    month_of_birth =  rep(01,20),
    day_of_birth =  rep(01,20)
  )
  observation_period <- tibble(
    observation_period_id = as.character(c(1:20)),
    person_id = as.character(c(1:20)),
    observation_period_start_date = rep(as.Date("2000-01-01"),20),
    observation_period_end_date = rep(as.Date("2012-06-01"),20)
  )
  outcome <-
    bind_rows(
      # 17 in first period
    tibble(
    cohort_definition_id = rep("1",17),
    subject_id = as.character(c(1:17)),
    cohort_start_date = rep(
      as.Date("2000-01-02"),17),
    cohort_end_date = rep(
      as.Date("2000-01-03"),17)
  ),
  # three in second
  tibble(
    cohort_definition_id = rep("1",3),
    subject_id = as.character(c(18:20)),
    cohort_start_date = rep(
      as.Date("2000-02-02"),3),
    cohort_end_date = rep(
      as.Date("2000-02-03"),3)
  )
    )

  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)
  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  dpop<-dpop$denominator_populations
  inc <- collect_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = FALSE,
    study_denominator_pop = dpop,
    minimum_cell_count = NULL
  )
  expect_true(inc[["incidence_estimates"]]$n_persons[1] == 20)
  expect_true(inc[["incidence_estimates"]]$n_persons[2] == 3)
  expect_true(inc[["incidence_estimates"]]$n_persons[3] == 0)
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[3]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[3]))
  expect_true(inc[["incidence_estimates"]]$n_events[1] == 17)
  expect_true(inc[["incidence_estimates"]]$n_events[2] == 3)
  expect_true(inc[["incidence_estimates"]]$n_events[3] == 0)
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[2]))

  inc <- collect_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = FALSE,
    study_denominator_pop = dpop,
    minimum_cell_count = 5
  )
  expect_true(inc[["incidence_estimates"]]$n_persons[1] == 20)
  expect_true(is.na(inc[["incidence_estimates"]]$n_persons[2]))
  expect_true(is.na(inc[["incidence_estimates"]]$n_persons[3]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$person_days[2]))
  expect_true(is.na(inc[["incidence_estimates"]]$person_days[3]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$person_years[2]))
  expect_true(is.na(inc[["incidence_estimates"]]$person_years[3]))
  expect_true(inc[["incidence_estimates"]]$n_events[1] == 17)
  expect_true(is.na(inc[["incidence_estimates"]]$n_events[2]))
  expect_true(is.na(inc[["incidence_estimates"]]$n_events[3]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$ir_100000_pys[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[2]))

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
  dpop<-dpop$denominator_populations

  inc<-collect_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = 1,
    cohort_ids_denominator_pops = 1,
    outcome_washout_windows=NULL,
    study_denominator_pop = dpop
  )
  expect_true(nrow(inc[["incidence_estimates"]])>=1)


 DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("expected errors with mock", {
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
  dpop<-dpop$denominator_populations

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
