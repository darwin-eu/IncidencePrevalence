test_that("mock db: check output format", {
  cdm_ref <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  prev <- get_pop_prevalence(cdm_ref,
                             results_schema_outcome = NULL,
                             table_name_outcome = "outcome",
                             study_denominator_pop = dpop
  )

  # prevalence results
  expect_true(all(c(
    "time",
    "numerator",
    "denominator",
    "prev",
    "start_time",
    "end_time"
  ) %in%
    names(prev[["pr"]])))

  # analysis settings
  expect_true(all(c(
    "type",
    "point",
    "time_interval",
    "minimum_representative_proportion",
    "full_period_required"
  ) %in%
    names(prev[["analysis_settings"]])))

  DBI::dbDisconnect(attr(cdm_ref, "dbcon"), shutdown = TRUE)
})

test_that("mock db: working examples", {
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble::tibble(
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
  cdm_ref <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)
  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations
  prev <- get_pop_prevalence(cdm_ref,
                            results_schema_outcome = NULL,
                            table_name_outcome = "outcome",
                            cohort_id_outcome = "1",
                            study_denominator_pop = dpop,
                            cohort_id_denominator_pop = "1",
                            type = "point",
                            time_interval = "months",
                            minimum_representative_proportion = 0.5
  )
  expect_true(nrow(prev[["pr"]]) >= 1)

  prev <- get_pop_prevalence(cdm_ref,
                            results_schema_outcome = NULL,
                            table_name_outcome = "outcome",
                            cohort_id_outcome = "1",
                            study_denominator_pop = dpop,
                            cohort_id_denominator_pop = "1",
                            type = "point",
                            time_interval = "years",
                            minimum_representative_proportion = 0.5
  )
  expect_true(nrow(prev[["pr"]]) >= 1)

  dpop <- collect_denominator_pops(
    cdm_ref = cdm_ref,
    study_age_stratas = list(c(0, 100), c(0, 100))
  )
  dpop <- dpop$denominator_populations

  prev <- get_pop_prevalence(cdm_ref,
                            results_schema_outcome = NULL,
                            table_name_outcome = "outcome",
                            cohort_id_outcome = "1",
                            study_denominator_pop = dpop,
                            cohort_id_denominator_pop = "1",
                            type = "period",
                            time_interval = "months",
                            minimum_representative_proportion = 0.5
  )
  expect_true(nrow(prev[["pr"]]) >= 1)

  DBI::dbDisconnect(attr( cdm_ref, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study time periods", {
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2010-12-31")
  )
  outcome <- tibble::tibble(
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

  cdm_ref <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  prev <- get_pop_prevalence(cdm_ref,
                            results_schema_outcome = NULL,
                            table_name_outcome = "outcome",
                            cohort_id_outcome = "1",
                            study_denominator_pop = dpop,
                            cohort_id_denominator_pop = "1",
                            type = "point",
                            time_interval = "months",
                            minimum_representative_proportion = 0.5,
                            verbose = FALSE
  )

  # we expect 12 months of which the last in December
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(nrow(prev[["pr"]]) == 12)

  DBI::dbDisconnect(attr( cdm_ref, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check periods follow calendar dates", {

  # check that even if study_start_date as during a period
  # periods still follow calendar dates
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-03-01"),
      as.Date("2011-01-31"),
      as.Date("2011-02-01"),
      as.Date("2011-03-01")
    ),
    cohort_end_date = c(
      as.Date("2010-03-01"),
      as.Date("2011-01-31"),
      as.Date("2011-02-01"),
      as.Date("2011-03-01")
    )
  )

  cdm_ref <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)

  # study_start_date during a month (with month as time_interval)
  dpop <- collect_denominator_pops(
    cdm_ref = cdm_ref,
    study_start_date = as.Date("2011-01-15")
  )
  dpop <- dpop$denominator_populations
  prev <- get_pop_prevalence(cdm_ref,
                            results_schema_outcome = NULL,
                            table_name_outcome = "outcome",
                            cohort_id_outcome = "1",
                            study_denominator_pop = dpop,
                            cohort_id_denominator_pop = "1",
                            type = "point",
                            time_interval = "months",
                            minimum_representative_proportion = 0.5
  )
  # expect_true(prev[["pr"]]$prev[2]==1)
  # expect_true(prev[["pr"]]$prev[3]==1)

})

test_that("mock db: check messages when vebose is true", {
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05")
    )
  )

  cdm_ref <- generate_mock_incidence_prevalence_db(outcome = outcome)

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  expect_message(get_pop_prevalence(cdm_ref,
                                    results_schema_outcome = NULL,
                                    table_name_outcome = "outcome",
                                    study_denominator_pop = dpop,
                                    type = "point",
                                    verbose = TRUE
  ))

  expect_message(get_pop_prevalence(cdm_ref,
                                    results_schema_outcome = NULL,
                                    table_name_outcome = "outcome",
                                    study_denominator_pop = dpop,
                                    type = "period",
                                    time_interval = "months",
                                    verbose = TRUE
  ))

  expect_message(get_pop_prevalence(cdm_ref,
                                    results_schema_outcome = NULL,
                                    table_name_outcome = "outcome",
                                    study_denominator_pop = dpop,
                                    type = "period",
                                    time_interval = "years",
                                    full_period_required = FALSE,
                                    verbose = TRUE
  ))

  DBI::dbDisconnect(attr(cdm_ref, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check conversion of user inputs", {
  cdm_ref <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  prev <- get_pop_prevalence(cdm_ref,
                             results_schema_outcome = NULL,
                             table_name_outcome = "outcome",
                             # converted to character
                             cohort_id_outcome = 1,
                             study_denominator_pop = dpop,
                             # converted to character
                             cohort_id_denominator_pop = 1,
                             type = "point"
  )
  expect_true(nrow(prev[["pr"]]) >= 0)

  DBI::dbDisconnect(attr(cdm_ref, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check expected errors", {
  cdm_ref <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  # not a cdm_reference
  expect_error(get_pop_prevalence("a",
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  type = "point"
  ))

  # no study pop
  expect_error(get_pop_prevalence(cdm_ref,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  cohort_id_denominator_pop = "999",
                                  type = "point"
  ))

  # no outcomes
  expect_error(get_pop_prevalence(cdm_ref,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  cohort_id_outcome = "999",
                                  study_denominator_pop = dpop,
                                  type = "point"
  ))



  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2010-01-05")
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2010-01-04")),
    cohort_end_date = c(as.Date("2010-01-04"))
  )

  cdm_ref <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  # expect error because less than one month between
  # cohort_start_date and cohort_end_date among dpop
  expect_error(get_pop_prevalence(cdm_ref,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  type = "period"
  ))
  expect_error(get_pop_prevalence(cdm_ref,
                                  results_schema_outcome = NULL,
                                  table_name_outcome = "outcome",
                                  study_denominator_pop = dpop,
                                  time_interval = c("years"),
                                  type = "period"
  ))

  DBI::dbDisconnect(attr(cdm_ref, "dbcon"), shutdown = TRUE)
})
