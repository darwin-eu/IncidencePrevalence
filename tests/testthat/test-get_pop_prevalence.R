test_that("mock db: check output format", {

  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  prev <- get_pop_prevalence(db,
                     results_schema_outcome = NULL,
                     table_name_outcome = "outcome",
                     study_denominator_pop = dpop,
                     period = "Point"
  )

  # prevalence results
  expect_true(all(c(
    "numerator",
    "denominator",
    "prev",
    "calendar_month",
    "calendar_year"
  ) %in%
    names(prev[["pr"]])))

  # analysis settings
  expect_true(all(c(
    "required_days_prior_history",
    "age_strata",
    "sex_strata",
    "period",
    "time_interval"
  ) %in%
    names(prev[["analysis_settings"]])))

  dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: working examples", {
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
  prev<- get_pop_prevalence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    cohort_id_denominator_pop = "1",
    period = "Point",
    time_interval = c("Months"),
    minimum_representative_proportion = 0.5
  )
  expect_true(nrow(prev[["pr"]])>=1)

  prev<- get_pop_prevalence(db,
                            results_schema_outcome = NULL,
                            table_name_outcome = "outcome",
                            cohort_id_outcome = "1",
                            study_denominator_pop = dpop,
                            cohort_id_denominator_pop = "1",
                            period = "Point",
                            time_interval = c("Years"),
                            minimum_representative_proportion = 0.5
  )
  expect_true(nrow(prev[["pr"]])>=1)


  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL,
    study_age_stratas = list(c(0, 100), c(0, 100))
  )
  prev<- get_pop_prevalence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    cohort_id_denominator_pop = "1",
    period = "Month",
    time_interval = c("Months"),
    minimum_representative_proportion = 0.5
  )
  expect_true(nrow(prev[["pr"]])>=1)

  dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check study time periods", {
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
    observation_period_end_date = as.Date("2010-12-31")
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
 prev<- get_pop_prevalence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    cohort_id_denominator_pop = "1",
    period = "Point",
    time_interval = c("Months"),
    minimum_representative_proportion = 0.5,
    verbose = FALSE
  )

   # we expect 12 months of which the last in December
   # the last month should also be included
   # as the person goes up to the last day of the month
   expect_true(length(prev[["pr"]]$calendar_year)==12)
   expect_true(any(prev[["pr"]]$calendar_month %in% 12))

   dbDisconnect(db, shutdown=TRUE)

 })

test_that("mock db: check messages when vebose is true", {
  library(DBI)
  library(dplyr)
  library(tibble)

  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05")
    )
  )

  db <- generate_mock_incidence_prevalence_db(outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  expect_message(get_pop_prevalence(db,
                                    results_schema_outcome = NULL,
                                    table_name_outcome = "outcome",
                                    study_denominator_pop = dpop,
                                    period = "Point",
                                    verbose = TRUE
  ))

  expect_message(get_pop_prevalence(db,
                                    results_schema_outcome = NULL,
                                    table_name_outcome = "outcome",
                                    study_denominator_pop = dpop,
                                    period = "Month",
                                    verbose = TRUE
  ))

  expect_message(get_pop_prevalence(db,
                                    results_schema_outcome = NULL,
                                    table_name_outcome = "outcome",
                                    study_denominator_pop = dpop,
                                    period =  "Year",
                                    verbose = TRUE
  ))

  dbDisconnect(db, shutdown=TRUE)

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

  prev <- get_pop_prevalence(db,
                             results_schema_outcome = NULL,
                             table_name_outcome = "outcome",
                             # converted to character
                             cohort_id_outcome = 1,
                             study_denominator_pop = dpop,
                             # converted to character
                             cohort_id_denominator_pop = 1,
                             period = "Point"
  )
  expect_true(nrow(prev[["pr"]])>=0)

  dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check expected errors", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  # not a db connection
  expect_error(get_pop_prevalence("a",
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  period = "Point"
  ))

  # no study pop
  expect_error(get_pop_prevalence(db,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  cohort_id_denominator_pop="999",
                                  period = "Point"
  ))

  # no outcomes
  expect_error(get_pop_prevalence(db,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  cohort_id_outcome="999",
                                  study_denominator_pop = dpop,
                                  period = "Point"
  ))



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
    observation_period_end_date = as.Date("2010-01-05")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2010-01-04")),
    cohort_end_date = c(as.Date("2010-01-04"))
  )

  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  # expect error because less than one month between
  # cohort_start_date and cohort_end_date among dpop
  expect_error(get_pop_prevalence(db,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  period = "Point"
  ))
  expect_error(get_pop_prevalence(db,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  time_interval = c("Years"),
                                  period = "Point"
  ))

  dbDisconnect(db)
})
