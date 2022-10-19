
test_that("mock db: check output format", {
  cdm <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(cdm = cdm)

  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_windows = 0,
    repetitive_events = FALSE,
    time_interval = c("months"),
    confidence_interval = "none",
    verbose = TRUE
  )

  expect_true(class(inc) == "list")
  expect_true(all(names(inc) %in%
                    c("incidence_estimates",
                    "analysis_settings",
                    "person_table",
                    "attrition" )))

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
    "time", "start_time","end_time",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(inc[["incidence_estimates"]])))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)

  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_windows = 0,
    repetitive_events = FALSE,
    time_interval = c("months"),
    verbose = TRUE
  )
  expect_true(nrow(inc[["incidence_estimates"]]) >= 1)





  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check working example 2", {
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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(cdm,
                            table_name_denominator = "denominator",
                           table_name_outcomes = "outcome",
                           repetitive_events = FALSE,
                           outcome_washout_window = 0,
                           minimum_cell_count = 0,
                           full_periods_required = FALSE
  )
  expect_true(sum(inc[["incidence_estimates"]]$n_events) == 1)

  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               table_name_outcomes = "outcome",
                               repetitive_events = TRUE,
                               outcome_washout_window = 2,
                               minimum_cell_count = 0,
                               full_periods_required = FALSE
  )
  expect_true(sum(inc[["incidence_estimates"]]$n_events) == 3)

  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               table_name_outcomes = "outcome",
                               repetitive_events = TRUE,
                               outcome_washout_window = 10,
                               minimum_cell_count = 0,
                               full_periods_required = FALSE
  )
  expect_true(sum(inc[["incidence_estimates"]]$n_events) == 2)

  # even if repetitive_events = TRUE,
  # if outcome_washout_window=NULL (all of history)
  # then it won´t be possible to have any recurrent events
  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               table_name_outcomes = "outcome",
                               repetitive_events = TRUE,
                               outcome_washout_window = NULL,
                               minimum_cell_count = 0,
                               full_periods_required = FALSE
  )
  expect_true(sum(inc[["incidence_estimates"]]$n_events) == 1)

  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               table_name_outcomes = "outcome",
                               repetitive_events = TRUE,
                               outcome_washout_window = NULL,
                               minimum_cell_count = 0,
                               time_interval ="weeks",
                               full_periods_required = FALSE
  )
  expect_true(sum(inc[["incidence_estimates"]]$n_events) == 1)

  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               table_name_outcomes = "outcome",
                               repetitive_events = TRUE,
                               outcome_washout_window = NULL,
                               minimum_cell_count = 0,
                               time_interval ="days",
                               full_periods_required = FALSE
  )
  expect_true(sum(inc[["incidence_estimates"]]$n_events) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study periods", {
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
    observation_period_start_date = as.Date("2010-01-15"),
    observation_period_end_date = as.Date("2010-12-15")
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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               cohort_ids_denominator_pops = "1",
                           table_name_outcomes = "outcome",
                           cohort_ids_outcomes = "1",
                           time_interval = "months",
                           repetitive_events = TRUE,
                           minimum_cell_count = 0,
                           full_periods_required = FALSE
  )

  # we expect 12 months of which the last in december
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(length(inc[["incidence_estimates"]]$time) == 12)


  inc <- collect_pop_incidence(cdm,
                               table_name_denominator = "denominator",
                               cohort_ids_denominator_pops = "1",
                               table_name_outcomes = "outcome",
                               cohort_ids_outcomes = "1",
                               time_interval = "months",
                               repetitive_events = TRUE,
                               minimum_cell_count = 0,
                               full_periods_required = TRUE
  )

  # now with full_periods_required is TRUE
  # we expect 10 months of which the last in november
  expect_true(length(inc[["incidence_estimates"]]$time) == 10)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check person days", {
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(2000,1999),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2007-01-01"),as.Date("2007-01-01")),
    observation_period_end_date = c(as.Date("2022-12-31"),as.Date("2022-10-05"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2021-06-27")),
    cohort_end_date = c(as.Date("2021-06-27"))
  )


  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = FALSE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0,
    full_periods_required = FALSE
  )

  # in 2019 we expect person 2 to contribute from 1st july to end of December
  expect_true(inc$incidence_estimates$person_days[1] ==
                as.numeric(difftime(as.Date("2019-12-31"),
                                    as.Date("2019-07-01"))) + 1)

  # in 2020 we expect person 2 to contribute all year
  # and person 1 from 1st January to end of December
  expect_true(inc$incidence_estimates$person_days[2] ==
                (as.numeric(difftime(as.Date("2020-12-31"),
                                     as.Date("2020-07-01"))) + 1) +
                (as.numeric(difftime(as.Date("2020-12-31"),
                                     as.Date("2020-01-01")) + 1))
  )

  # in 2021 we expect person 2 to contribute all year
  # and person 1 from 1st January up to 27th june (date of their outcome)
  expect_true(inc$incidence_estimates$person_days[3] ==
                (as.numeric(difftime(as.Date("2021-12-31"),
                                     as.Date("2021-01-01"))) + 1) +
                (as.numeric(difftime(as.Date("2021-06-27"),
                                     as.Date("2021-01-01")) + 1))
  )

  # in 2022 we expect person 2 to contribute all year
  # (person 1 is out- they have had an event)
  expect_true(inc$incidence_estimates$person_days[4] ==
                (as.numeric(difftime(as.Date("2021-10-05"),
                                     as.Date("2021-01-01"))) + 1)
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  # study_start_date during a year (with year as time_interval)
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2010-02-01")
  )
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = TRUE,
    outcome_washout_window = 0,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0,
    full_periods_required = FALSE
  )
  expect_true(inc[["incidence_estimates"]]$n_events[1] == 1)
  expect_true(inc[["incidence_estimates"]]$n_events[2] == 3)


  # study_start_date during a month (with month as time_interval)
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2011-01-15")
  )
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = TRUE,
    outcome_washout_window = 0,
    time_interval = c("months"),
    verbose = TRUE,
    minimum_cell_count = 0,
    full_periods_required = FALSE
  )
  expect_true(inc[["incidence_estimates"]]$n_events[1] == 1)
  expect_true(inc[["incidence_estimates"]]$n_events[2] == 1)
  expect_true(inc[["incidence_estimates"]]$n_events[3] == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check washout windows", {
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
      as.Date("2010-06-01"),
      # more than six months since the last event
      as.Date("2011-01-13"),
      # two days since the end of the last event
      as.Date("2011-01-16"),
      # one day since the end of the last event
      as.Date("2011-01-18")
    ),
    cohort_end_date = c(
      as.Date("2010-06-02"),
      as.Date("2011-01-14"),
      as.Date("2011-01-17"),
      as.Date("2011-01-19")
    )
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  inc_w0 <- collect_pop_incidence(cdm,
                              table_name_denominator="denominator",
                              table_name_outcomes = "outcome",
                              cohort_ids_outcomes = "1",
                              cohort_ids_denominator_pops = "1",
                              repetitive_events = TRUE,
                              outcome_washout_window = 0,
                              minimum_cell_count = 0
  )
  # expect all events if we have zero days washout
  expect_true(sum(inc_w0[["incidence_estimates"]]$n_events) == 4)

  inc_w1 <- collect_pop_incidence(cdm,
                                  table_name_denominator="denominator",
                                  table_name_outcomes = "outcome",
                                  cohort_ids_outcomes = "1",
                                  cohort_ids_denominator_pops = "1",
                                  repetitive_events = TRUE,
                                  outcome_washout_window = 1,
                                  minimum_cell_count = 0
  )
  # expect three events if we have one days washout
  expect_true(sum(inc_w1[["incidence_estimates"]]$n_events) == 3)

  inc_w2 <- collect_pop_incidence(cdm,
                                  table_name_denominator="denominator",
                                  table_name_outcomes = "outcome",
                                  cohort_ids_outcomes = "1",
                                  cohort_ids_denominator_pops = "1",
                                  repetitive_events = TRUE,
                                  outcome_washout_window = 2,
                                  minimum_cell_count = 0
  )
  # expect two events if we have two days washout
  expect_true(sum(inc_w2[["incidence_estimates"]]$n_events) == 2)

  inc_w365 <- collect_pop_incidence(cdm,
                                    table_name_denominator="denominator",
                                    table_name_outcomes = "outcome",
                                    cohort_ids_outcomes = "1",
                                    cohort_ids_denominator_pops = "1",
                                    repetitive_events = TRUE,
                                    outcome_washout_window = 365,
                                    minimum_cell_count = 0
  )
  # expect one event if we have 365 days washout
  expect_true(sum(inc_w365[["incidence_estimates"]]$n_events) == 1)

  inc_null <-  collect_pop_incidence(cdm,
                                     table_name_denominator="denominator",
                                     table_name_outcomes = "outcome",
                                     cohort_ids_outcomes = "1",
                                     cohort_ids_denominator_pops = "1",
                                     repetitive_events = TRUE,
                                     outcome_washout_window = NULL,
                                     minimum_cell_count = 0
  )
  # expect one event if we have NULL (all history washout)
  expect_true(sum(inc_null[["incidence_estimates"]]$n_events) == 1)

  # but, we will have move days when using the 365 day washout
  # as the person came back to contribute more time at risk
  expect_true(sum(inc_null[["incidence_estimates"]]$person_days) < sum(inc_w365[["incidence_estimates"]]$person_days))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check events overlapping with start of a period", {
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(2000,1999),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2000-01-21"),as.Date("2007-01-01")),
    observation_period_end_date = c(as.Date("2022-12-31"),as.Date("2022-12-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2020-06-27")),
    cohort_end_date = c(as.Date("2020-07-19"))
  )


  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    outcome_washout_window = NULL,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )

  expect_true(all(inc$incidence_estimates$n_persons == 1))

  # another example
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(2000,1999),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2000-01-21"),as.Date("2007-01-01")),
    observation_period_end_date = c(as.Date("2022-12-31"),as.Date("2022-12-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1","1"),
    subject_id = c("1","1"),
    cohort_start_date = c(as.Date("2020-06-27"), as.Date("2020-07-30")),
    cohort_end_date = c(as.Date("2020-07-19"), as.Date("2020-08-20"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations

  inc2 <- collect_pop_incidence(
      cdm = cdm,
      table_name_denominator="denominator",
      table_name_outcomes = "outcome",
      cohort_ids_outcomes = "1",
      cohort_ids_denominator_pops = "1",
      outcome_washout_window = NULL,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(all(inc2$incidence_estimates$n_persons == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: compare results from months and years", {
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = rep("8507",2),
    year_of_birth = rep(2000,2),
    month_of_birth = rep(01,2),
    day_of_birth = rep(01,2)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2010-01-01"),
                                      as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2012-01-01"),
                                    as.Date("2012-01-01"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2011-07-01")
    ),
    cohort_end_date = c(
      as.Date("2011-07-01")
    )
  )
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2010-01-01"),
    study_end_date = as.Date("2011-12-31")
  )
  cdm$denominator <- dpop$denominator_populations

  inc_months <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    time_interval = c("months"),
    minimum_cell_count = 0
  )
  inc_years <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    time_interval = c("years"),
    minimum_cell_count = 0
  )

  # consistent results for months and years
  expect_true(sum(inc_months$incidence_estimates$n_events)==
                sum(inc_years$incidence_estimates$n_events))
  expect_equal(sum(inc_months$incidence_estimates$person_days),
               sum(inc_years$incidence_estimates$person_days))
  expect_equal(sum(inc_months$incidence_estimates$person_years),
               sum(inc_years$incidence_estimates$person_years))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check entry and event on same day", {

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
    observation_period_start_date = as.Date("2010-01-28"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-01-28")
    ),
    cohort_end_date = c(
      as.Date("2010-01-28")
    )
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  inc_without_rep <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = FALSE,
    outcome_washout_window = NULL,
    time_interval = "years",
    minimum_cell_count = 0,
    full_periods_required = FALSE
  )
  expect_true(sum(inc_without_rep[["incidence_estimates"]]$n_events) == 1)

  inc_with_rep <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    repetitive_events = TRUE,
    outcome_washout_window = NULL,
    time_interval = "years",
    minimum_cell_count = 0,
    full_periods_required = FALSE
  )
  expect_true(sum(inc_with_rep[["incidence_estimates"]]$n_events) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: cohort start overlaps with the outcome", {
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-05-09"),as.Date("2019-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),as.Date("2020-12-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1","1"),
    subject_id = c("1","1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome[1,])

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations


  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator="denominator",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    outcome_washout_window = 180,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )

  expect_true(all(inc$incidence_estimates$n_persons == c(1,2)))
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
  observation_period <- tibble::tibble(
    observation_period_id = as.character(c(1:20)),
    person_id = as.character(c(1:20)),
    observation_period_start_date = rep(as.Date("2000-01-01"),20),
    observation_period_end_date = rep(as.Date("2012-06-01"),20)
  )
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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)
  dpop <- collect_denominator_pops(cdm = cdm)

  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    repetitive_events = FALSE,
    minimum_cell_count = 0,
    full_periods_required = FALSE
  )
  expect_true(inc[["incidence_estimates"]]$n_persons[1] == 20)
  expect_true(inc[["incidence_estimates"]]$n_persons[2] == 3)
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[2]))
  expect_true(inc[["incidence_estimates"]]$n_events[1] == 17)
  expect_true(inc[["incidence_estimates"]]$n_events[2] == 3)
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[1]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[2]))

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    repetitive_events = FALSE,
    minimum_cell_count = 5,
    full_periods_required = FALSE
  )
  expect_true(inc[["incidence_estimates"]]$n_persons[1] == 20)
  expect_true(is.na(inc[["incidence_estimates"]]$n_persons[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_days[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$person_days[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$person_years[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$person_years[2]))
  expect_true(inc[["incidence_estimates"]]$n_events[1] == 17)
  expect_true(is.na(inc[["incidence_estimates"]]$n_events[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$ir_100000_pys[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$ir_100000_pys_low[2]))
  expect_true(!is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[1]))
  expect_true(is.na(inc[["incidence_estimates"]]$ir_100000_pys_high[2]))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: multiple overlapping outcomes", {
  # two
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-04-29"),as.Date("2019-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),as.Date("2021-12-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1","1"),
    subject_id = c("1","1"),
    cohort_start_date = c(as.Date("2020-04-26"),as.Date("2020-11-10")),
    cohort_end_date = c(as.Date("2020-05-17"),as.Date("2020-12-17"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator<- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = 180,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(all(inc$incidence_estimates$n_persons) == 1)

  # three
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-04-29"),as.Date("2019-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),as.Date("2021-12-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1","1","1"),
    subject_id = c("1","1","1"),
    cohort_start_date = c(as.Date("2020-04-26"),
                          as.Date("2020-11-08"),
                          as.Date("2020-11-10")),
    cohort_end_date = c(as.Date("2020-05-17"),
                        as.Date("2020-11-09"),
                        as.Date("2020-12-17"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = 180,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    minimum_cell_count = 0
  )
  expect_true(all(inc$incidence_estimates$n_persons) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: cohort before start of period and ending after end of period", {
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1990,1990),
    month_of_birth = c(01,01),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2000-07-31"),as.Date("2000-07-31")),
    observation_period_end_date = c(as.Date("2010-01-01"),as.Date("2010-01-01"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "2"),
    cohort_start_date = c(as.Date("2000-08-02"),as.Date("2001-06-01")),
    cohort_end_date = c(as.Date("2020-01-01"), as.Date("2001-07-01"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date(as.Date("2001-01-01")),
    study_end_date = as.Date(as.Date("2001-12-31"))
  )
  cdm$denominator <- dpop$denominator_populations

  # regardless of washout we expect one event
  # with only one participant
  # person 1s outcome starts before period and ends after

  # no washout
  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = 0,
    repetitive_events = FALSE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(all(inc$incidence_estimates$n_events == c(1)))

  # washout
  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = NULL,
    repetitive_events = FALSE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(all(inc$incidence_estimates$n_events == c(1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: check full period requirement - year", {

  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-05-09"),as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2021-06-06"),as.Date("2021-06-06"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations


  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = NULL,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(inc$incidence_estimates$n_persons[1] == 1)

  # edge case first day to last of the year
  # still expect this to work
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-05-09"),as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),as.Date("2020-12-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations


  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = NULL,
    repetitive_events = TRUE,
    time_interval = c("Years"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(inc$incidence_estimates$n_persons[1] == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


})

test_that("mock db: check full period requirement - month", {
  # expected to work
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-05-09"),as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2020-06-06"),as.Date("2020-06-06"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations


  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = NULL,
    repetitive_events = TRUE,
    time_interval = c("Months"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(nrow(inc$incidence_estimates) >= 1)


  # edge case first day to last of the month
  # still expect this to work
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2020-05-09"),as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2020-01-31"),as.Date("2020-01-31"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(20,30))
  )
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    outcome_washout_window = NULL,
    repetitive_events = TRUE,
    time_interval = c("Months"),
    verbose = TRUE,
    minimum_cell_count = 0
  )
  expect_true(inc$incidence_estimates$n_persons == 1)
  expect_true(nrow(inc$incidence_estimates) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: check full_periods_required", {

  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = c(1995,1995),
    month_of_birth = c(07,07),
    day_of_birth = c(01,01)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2019-05-09"),as.Date("2019-02-02")),
    observation_period_end_date = c(as.Date("2022-06-01"),as.Date("2020-06-06"))
  )
  outcome <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                               observation_period = observation_period,
                                               outcome = outcome)

  dpop <- collect_denominator_pops(
    cdm = cdm
  )
  cdm$denominator <- dpop$denominator_populations

# full periods required TRUE
# repetitive events TRUE
# - we expect to start in 2020 (both start during 2019)
# - we expect to go up to 2021 (id 2 end date is in 2022)
inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    table_name_outcomes = "outcome",
    time_interval = c("Years"),
    repetitive_events = TRUE,
    full_periods_required = TRUE,
    minimum_cell_count = 0
  )
  expect_true(nrow(inc$incidence_estimates) == 2)
  expect_true(inc$incidence_estimates$time[1] == "2020")
  expect_true(inc$incidence_estimates$time[2] == "2021")
# repetitive events FALSE
# - now we expect only to use 2020 (id 2 obs end is in 20)
inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    table_name_outcomes = "outcome",
    time_interval = c("Years"),
    repetitive_events = FALSE,
    full_periods_required = TRUE,
    minimum_cell_count = 0
  )
expect_true(nrow(inc$incidence_estimates) == 1)
expect_true(inc$incidence_estimates$time[1] == "2020")

# full periods required FALSE
# repetitive events TRUE
# - we expect to start in 2019
# - we expect to go up to 2022
inc <- collect_pop_incidence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  time_interval = c("Years"),
  repetitive_events = TRUE,
  full_periods_required = FALSE,
  minimum_cell_count = 0
)
expect_true(nrow(inc$incidence_estimates) == 4)
expect_true(inc$incidence_estimates$time[1] == "2019")
expect_true(inc$incidence_estimates$time[2] == "2020")
expect_true(inc$incidence_estimates$time[3] == "2021")
expect_true(inc$incidence_estimates$time[4] == "2022")
# repetitive events FALSE
inc <- collect_pop_incidence(
  cdm = cdm,
  table_name_denominator = "denominator",
  table_name_outcomes = "outcome",
  time_interval = c("Years"),
  repetitive_events = FALSE,
  full_periods_required = FALSE,
  minimum_cell_count = 0
)
expect_true(nrow(inc$incidence_estimates) == 2)
expect_true(inc$incidence_estimates$time[1] == "2019")
expect_true(inc$incidence_estimates$time[2] == "2020")

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: check conversion of user inputs", {
  cdm <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = 1,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = 1,
    outcome_washout_windows = NA
  )
  expect_true(nrow(inc[["incidence_estimates"]]) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check messages when vebose is true", {
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
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05")
    )
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  expect_message(collect_pop_incidence(cdm,
                                    table_name_denominator = "denominator",
                                    cohort_ids_denominator_pops="1",
                                   table_name_outcomes =  "outcome",
                                   cohort_ids_outcomes = "1",
                                   verbose = TRUE))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors with mock", {
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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period,
                                              outcome = outcome)

  dpop <- collect_denominator_pops(cdm = cdm)
  cdm$denominator <- dpop$denominator_populations

  # not a cdm reference
  expect_error(collect_pop_incidence(
    cdm = "a",
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
  ))

  # no study pop
  expect_error(collect_pop_incidence(cdm,
                                 table_name_outcome = "outcome",
                                 time_interval = c("months"),
                                 study_denominator_pop = dpop,
                                 cohort_id_denominator_pop = "999"
  ))

  # no denominator
  expect_error(collect_pop_incidence(cdm,
                                     table_name_denominator = "denominator",
                                     cohort_ids_denominator_pops = "11",
                                     table_name_outcomes = "outcome",
                                     cohort_ids_outcomes = "1"
  ))

  # no outcomes
  expect_error(collect_pop_incidence(cdm,
                                 table_name_denominator = "denominator",
                                 cohort_ids_denominator_pops = "1",
                                 table_name_outcomes = "outcome",
                                 cohort_ids_outcomes = "11"
  ))



  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date= as.Date("2019-06-01"),
    study_end_date= as.Date("2019-08-01"))
  cdm$denominator <- dpop$denominator_populations

  expect_error(collect_pop_incidence(cdm,
                                     table_name_denominator = "denominator",
                                     table_name_outcomes = "outcome",
                                     time_interval = "Years"))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})



