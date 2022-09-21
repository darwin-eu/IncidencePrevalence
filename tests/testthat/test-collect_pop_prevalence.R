
test_that("mock db: check output format", {
  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  dpop<-dpop$denominator_populations
  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop,
    confidence_interval="poisson"
  )

  expect_true(class(prev) == "list")
  expect_true(all(names(prev) %in%
                    c("prevalence_estimates",
                      "analysis_settings",
                      "person_table",
                      "attrition" )))

  # check analysis settings tibble::tibble
  expect_true(all(c(
    "prevalence_analysis_id",
    "type",
    "point",
    "time_interval",
    "minimum_representative_proportion",
    "full_period_required",
    "cohort_id_outcome",
    "cohort_id_denominator_pop",
    "confidence_interval",
    "minimum_cell_count"
  ) %in%
    names(prev[["analysis_settings"]])))

  # check estimates tibble
  expect_true(all(c(
    "prevalence_analysis_id",
    "time",
    "numerator", "denominator",
    "prev",
    "prev_low",
    "prev_high",
    "start_time", "end_time",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(prev[["prevalence_estimates"]])))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: checks on working example", {
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
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble::tibble(
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
  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop
  )
  expect_true(nrow(prev[["prevalence_estimates"]])>=1)

  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check minimum counts", {
  #20 people
  person <- tibble::tibble(
    person_id = as.character(c(1:20)),
    gender_concept_id = rep("8507",20),
    year_of_birth =  rep(2000,20),
    month_of_birth =  rep(01,20),
    day_of_birth =  rep(01,20)
  )
  observation_period <- dplyr::bind_rows(
    tibble::tibble(
    observation_period_id = as.character(c(1:17)),
    person_id = as.character(c(1:17)),
    observation_period_start_date = rep(as.Date("2000-01-01"),17),
    observation_period_end_date = rep(as.Date("2000-01-31"),17)
  ),
  tibble::tibble(
    observation_period_id = as.character(c(18:20)),
    person_id = as.character(c(18:20)),
    observation_period_start_date = rep(as.Date("2000-01-01"),3),
    observation_period_end_date = rep(as.Date("2012-06-01"),3)
  ))

  outcome <-
    dplyr::bind_rows(
      # 17 in first period
      tibble::tibble(
        cohort_definition_id = rep("1",17),
        subject_id = as.character(c(1:17)),
        cohort_start_date = rep(
          as.Date("2000-01-02"),17),
        cohort_end_date = rep(
          as.Date("2000-01-03"),17)
      ),
      # three in second
      tibble::tibble(
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

  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop,
    minimum_cell_count = NULL,
    type = "period",
    time_intervals = "months",
    confidence_interval = "poisson"
  )
  expect_true(prev[["prevalence_estimates"]]$numerator[1] == 17)
  expect_true(prev[["prevalence_estimates"]]$numerator[2] == 3)
  expect_true(prev[["prevalence_estimates"]]$numerator[3] == 0)
  expect_true(prev[["prevalence_estimates"]]$denominator[1] == 20)
  expect_true(prev[["prevalence_estimates"]]$denominator[2] == 3)
  expect_true(prev[["prevalence_estimates"]]$denominator[3] == 3)
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev[1]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev[2]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev[3]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_low[1]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_low[2]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_low[3]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_high[1]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_high[2]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_high[3]))

  prev <- collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop,
    minimum_cell_count = 5,
    type = "period",
    time_intervals = "months",
    confidence_interval = "poisson"
  )
  expect_true(prev[["prevalence_estimates"]]$numerator[1] == 17)
  expect_true(is.na(prev[["prevalence_estimates"]]$numerator[2]))
  expect_true(is.na(prev[["prevalence_estimates"]]$numerator[3]))
  expect_true(prev[["prevalence_estimates"]]$denominator[1] == 20)
  expect_true(is.na(prev[["prevalence_estimates"]]$denominator[2]))
  expect_true(is.na(prev[["prevalence_estimates"]]$denominator[3]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev[1]))
  expect_true(is.na(prev[["prevalence_estimates"]]$prev[2]))
  expect_true(is.na(prev[["prevalence_estimates"]]$prev[3]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_low[1]))
  expect_true(is.na(prev[["prevalence_estimates"]]$prev_low[2]))
  expect_true(is.na(prev[["prevalence_estimates"]]$prev_low[3]))
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_high[1]))
  expect_true(is.na(prev[["prevalence_estimates"]]$prev_high[2]))
  expect_true(is.na(prev[["prevalence_estimates"]]$prev_high[3]))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check conversion of user inputs", {
  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  dpop<-dpop$denominator_populations

  prev<-collect_pop_prevalence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = 1,
    cohort_ids_denominator_pops = 1,
    study_denominator_pop = dpop
  )
  expect_true(nrow(prev[["prevalence_estimates"]])>=1)


  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check expected errors", {
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
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  dpop<-dpop$denominator_populations

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

