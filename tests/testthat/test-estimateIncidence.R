
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  # check estimates tibble
  expect_true(all(c(
    "analysis_id",
    "n_persons",
    "person_days",
    "person_years",
    "n_events",
    "incidence_100000_pys",
    "incidence_100000_pys_95CI_lower",
    "incidence_100000_pys_95CI_upper",
    "incidence_start_date",
    "incidence_end_date",
    "cohort_obscured",
    "result_obscured",
    "analysis_outcome_washout",
    "analysis_repeated_events",
    "analysis_interval",
    "analysis_complete_database_intervals",
    "analysis_min_cell_count",
    "outcome_cohort_id",
    "outcome_cohort_name",
    "denominator_cohort_id",
    "denominator_age_group",
    "denominator_sex",
    "denominator_days_prior_observation",
    "denominator_start_date",
    "denominator_end_date",
    "denominator_target_cohort_definition_id",
    "denominator_target_cohort_name",
    "cdm_name"
  ) %in%
    names(inc)))

  expect_true(all(c(
    "analysis_id", "number_records", "number_subjects",
    "reason_id", "reason",
    "excluded_records", "excluded_subjects",
    "analysis_outcome_washout",
    "analysis_repeated_events",
    "analysis_interval",
    "analysis_complete_database_intervals",
    "analysis_min_cell_count",
    "outcome_cohort_id",
    "outcome_cohort_name",
    "denominator_cohort_id",
    "denominator_age_group",
    "denominator_sex",
    "denominator_days_prior_observation",
    "denominator_start_date",
    "denominator_end_date",
    "denominator_target_cohort_definition_id",
    "denominator_target_cohort_name",
    "cdm_name"
  ) %in%
    names(incidenceAttrition(inc))))

  # do not return participants as default
  expect_true(is.null(participants(inc, 1)))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  cdm <- mockIncidencePrevalenceRef()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    temporary = FALSE,
    returnParticipants = TRUE
  )
  expect_true(tibble::is_tibble(participants(inc, 1) %>%
    dplyr::collect()))
  expect_true(participants(inc, 1) %>%
    dplyr::collect() %>%
    dplyr::select("subject_id") %>%
    dplyr::pull() == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: checks on working example", {
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "overall")
  )
  expect_true(nrow(inc) >= 1)

  # reconnect
  cdmReconn <- CDMConnector::cdm_from_con(
    con = attr(cdm, "dbcon"),
    cohort_tables = c("denominator", "outcome"),
    write_schema = "main"
  )
  inc_recon <- estimateIncidence(
    cdm = cdmReconn,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "overall")
  )
  # expect_equal(inc, inc_recon)
  # expect_equal(incidenceAttrition(inc), incidenceAttrition(inc_recon))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check working example 2", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 1)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 2,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 3)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 10,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 2)

  # even if repeatedEvents = TRUE,
  # if outcomeWashout=NULL (all of history)
  # then it won´t be possible to have any recurrent events
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 1)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    minCellCount = 0,
    interval = "weeks",
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study periods", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-15"),
    observation_period_end_date = as.Date("2010-12-15")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeWashout = 0,
    repeatedEvents = TRUE,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  # we expect 12 months of which the last in december
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(nrow(inc) == 12)


  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    interval = "months",
    repeatedEvents = TRUE,
    minCellCount = 0,
    completeDatabaseIntervals = TRUE
  )

  # now with completeDatabaseIntervals is TRUE
  # we expect 10 months of which the last in november
  expect_true(nrow(inc) == 10)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check overall", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2005-01-15"),
      as.Date("2005-01-15")
    ),
    observation_period_end_date = c(
      as.Date("2007-05-01"),
      as.Date("2011-06-15")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1", "2", "2"),
    cohort_start_date = c(
      as.Date("2006-02-05"),
      as.Date("2006-02-05"),
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2006-02-05"),
      as.Date("2006-02-05"),
      as.Date("2010-02-05")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2007-01-01"), as.Date(NA))
  )

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall",
    repeatedEvents = FALSE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  # we expect one row with the overall results
  # with two people
  # one person had the event before the study period
  # (but washout was 0 so was included)
  # one person had the event during the study period
  expect_true(nrow(inc) == 1)
  expect_true(inc$n_persons == 2)
  expect_true(inc$incidence_start_date ==
    as.Date("2007-01-01"))
  expect_true(inc$incidence_end_date ==
    as.Date("2010-02-05")) # date of first event


  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(inc) == 1)
  expect_true(inc$incidence_start_date ==
    as.Date("2007-01-01"))
  expect_true(inc$incidence_end_date ==
    as.Date("2011-06-15")) # date of end of obs

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check person days", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(2000, 1999),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2007-01-01"),
      as.Date("2007-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-12-31"),
      as.Date("2022-10-05")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = "1",
    cohort_start_date = c(as.Date("2021-06-27")),
    cohort_end_date = c(as.Date("2021-06-27"))
  )


  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    interval = c("Years"),
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  # in 2019 we expect person 2 to contribute from 1st july to end of December
  expect_true(inc$person_days[1] ==
    as.numeric(difftime(
      as.Date("2019-12-31"),
      as.Date("2019-07-01")
    )) + 1)

  # in 2020 we expect person 2 to contribute all year
  # and person 1 from 1st January to end of December
  expect_true(inc$person_days[2] ==
    (as.numeric(difftime(
      as.Date("2020-12-31"),
      as.Date("2020-07-01")
    )) + 1) +
      (as.numeric(difftime(
        as.Date("2020-12-31"),
        as.Date("2020-01-01")
      ) + 1)))

  # in 2021 we expect person 2 to contribute all year
  # and person 1 from 1st January up to 27th june (date of their outcome)
  expect_true(inc$person_days[3] ==
    (as.numeric(difftime(
      as.Date("2021-12-31"),
      as.Date("2021-01-01")
    )) + 1) +
      (as.numeric(difftime(
        as.Date("2021-06-27"),
        as.Date("2021-01-01")
      ) + 1)))

  # in 2022 we expect person 2 to contribute all year
  # (person 1 is out- they have had an event)
  expect_true(inc$person_days[4] ==
    (as.numeric(difftime(
      as.Date("2021-10-05"),
      as.Date("2021-01-01")
    )) + 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check periods follow calendar dates", {
  skip_on_cran()
  # check that even if startDate as during a period
  # periods still follow calendar dates
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  # startDate during a year (with year as interval)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2010-02-01"), as.Date(NA))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    interval = c("Years"),
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc$n_events[1] == 1)
  expect_true(inc$n_events[2] == 3)


  # startDate during a month (with month as interval)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", overwrite = TRUE,
    cohortDateRange = c(as.Date("2011-01-15"), as.Date(NA))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    interval = c("months"),
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc$n_events[1] == 1)
  expect_true(inc$n_events[2] == 1)
  expect_true(inc$n_events[3] == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check washout windows", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  incW0 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect all events if we have zero days washout
  expect_true(sum(incW0$n_events) == 4)

  incW1 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 1,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect three events if we have one days washout
  expect_true(sum(incW1$n_events) == 3)

  incW2 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 2,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect two events if we have two days washout
  expect_true(sum(incW2$n_events) == 2)

  incW365 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect one event if we have 365 days washout
  expect_true(sum(incW365$n_events) == 1)

  incNull <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect one event if we have NULL (all history washout)
  expect_true(sum(incNull$n_events) == 1)

  # but, we will have move days when using the 365 day washout
  # as the person came back to contribute more time at risk
  expect_true(sum(incNull$person_days) <
    sum(incW365$person_days))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


  # never satisfy criteria in study period
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2009-06-01")
    ),
    cohort_end_date = c(
      as.Date("2010-06-02")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  incW365 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 36500,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(nrow(incW365) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check events overlapping with start of a period", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(2000, 1999),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-01-21"),
      as.Date("2007-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-12-31"),
      as.Date("2022-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = "1",
    cohort_start_date = c(as.Date("2020-06-27")),
    cohort_end_date = c(as.Date("2020-07-19"))
  )


  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(inc$n_persons == 1))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # another example
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(2000, 1999),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-01-21"),
      as.Date("2007-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-12-31"),
      as.Date("2022-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c("1", "1"),
    cohort_start_date = c(as.Date("2020-06-27"), as.Date("2020-07-30")),
    cohort_end_date = c(as.Date("2020-07-19"), as.Date("2020-08-20"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  expect_true(all(inc2$n_persons == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: compare results from months and years", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = rep("8507", 2),
    year_of_birth = rep(2000, 2),
    month_of_birth = rep(01, 2),
    day_of_birth = rep(01, 2)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2011-07-01")
    ),
    cohort_end_date = c(
      as.Date("2011-07-01")
    )
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2010-01-01"), as.Date("2011-12-31"))
  )

  incMonths <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("months"),
    minCellCount = 0
  )
  incYears <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years"),
    minCellCount = 0
  )

  # consistent results for months and years
  expect_true(sum(incMonths$n_events) ==
    sum(incYears$n_events))
  expect_equal(
    sum(incMonths$person_days),
    sum(incYears$person_days)
  )
  expect_equal(
    sum(incMonths$person_years),
    sum(incYears$person_years)
  )
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2010-01-01"), as.Date("2011-12-31"))
  )

  incWeeks <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("weeks"),
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  incQuarters <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("quarters"),
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  incMonths <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("months"),
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  incYears <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years"),
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  # consistent results for months and years
  expect_true(sum(incWeeks$n_events) ==
    sum(incYears$n_events))
  expect_true(sum(incQuarters$n_events) ==
    sum(incYears$n_events))
  expect_true(sum(incMonths$n_events) ==
    sum(incYears$n_events))

  expect_equal(
    sum(incWeeks$person_days),
    sum(incYears$person_days)
  )
  expect_equal(
    sum(incQuarters$person_days),
    sum(incYears$person_days)
  )
  expect_equal(
    sum(incMonths$person_days),
    sum(incYears$person_days)
  )

  expect_equal(
    sum(incWeeks$person_years),
    sum(incYears$person_years)
  )
  expect_equal(
    sum(incQuarters$person_years),
    sum(incYears$person_years)
  )
  expect_equal(
    sum(incMonths$person_years),
    sum(incYears$person_years)
  )


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check entry and event on same day", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-28"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-01-28")
    ),
    cohort_end_date = c(
      as.Date("2010-01-28")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  incWithoutRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    outcomeWashout = Inf,
    interval = "years",
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(incWithoutRep$n_events) == 1)

  incWithRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    interval = "years",
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(incWithRep$n_events) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: cohort start overlaps with the outcome", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2019-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(inc$n_persons == c(1, 2)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check outcome before observation period start", {
  skip_on_cran()
  # 1) with outcome starting and ending before observation period start
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c("1", "1"),
    cohort_start_date = c(as.Date("1999-01-01")),
    cohort_end_date = c(as.Date("1999-05-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2000-01-01"), as.Date("2004-01-01"))
  )

  # with rep events - should have both people
  incRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  incNoRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(incRep$n_persons == 2))
  expect_true(all(incNoRep$n_persons == 1))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # 2) with outcome starting before observation period start,
  # ending during observation period
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("1999-01-01")),
    cohort_end_date = c(as.Date("2021-05-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2000-01-01"), as.Date("2004-01-01"))
  )

  # with rep events - should have one person for rep, both people in second
  incRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  incNoRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(incRep$n_persons == 1))
  expect_true(all(incNoRep$n_persons == 1))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # 3) with outcome starting before observation period start,
  # ending after observation period end
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("1999-01-01")),
    cohort_end_date = c(as.Date("2001-05-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2000-01-01"), as.Date("2004-01-01"))
  )

  # with rep events - should have both people
  incRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  incNoRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(incRep$n_persons == c(1, 2, 2, 2)))
  expect_true(all(incNoRep$n_persons == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check minimum counts", {
  skip_on_cran()
  # 20 people
  personTable <- tibble::tibble(
    person_id = as.character(c(1:20)),
    gender_concept_id = rep("8507", 20),
    year_of_birth = rep(2000, 20),
    month_of_birth = rep(01, 20),
    day_of_birth = rep(01, 20)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = as.character(c(1:20)),
    person_id = as.character(c(1:20)),
    observation_period_start_date = rep(as.Date("2000-01-01"), 20),
    observation_period_end_date = rep(as.Date("2012-06-01"), 20)
  )
  outcomeTable <-
    dplyr::bind_rows(
      # 17 in first period
      tibble::tibble(
        cohort_definition_id = rep("1", 17),
        subject_id = as.character(c(1:17)),
        cohort_start_date = rep(
          as.Date("2000-01-02"), 17
        ),
        cohort_end_date = rep(
          as.Date("2000-01-03"), 17
        )
      ),
      # three in second
      tibble::tibble(
        cohort_definition_id = rep("1", 3),
        subject_id = as.character(c(18:20)),
        cohort_start_date = rep(
          as.Date("2000-02-02"), 3
        ),
        cohort_end_date = rep(
          as.Date("2000-02-03"), 3
        )
      )
    )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    repeatedEvents = FALSE,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc$n_persons[1] == 20)
  expect_true(inc$n_persons[2] == 3)
  expect_true(!is.na(inc$person_days[1]))
  expect_true(!is.na(inc$person_days[2]))
  expect_true(!is.na(inc$person_years[1]))
  expect_true(!is.na(inc$person_years[2]))
  expect_true(inc$n_events[1] == 17)
  expect_true(inc$n_events[2] == 3)
  expect_true(!is.na(inc$incidence_100000_pys[1]))
  expect_true(!is.na(inc$incidence_100000_pys[2]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_lower[1]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_lower[2]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_upper[1]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_upper[2]))

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    repeatedEvents = FALSE,
    minCellCount = 5,
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc$n_persons[1] == 20)
  expect_true(is.na(inc$n_persons[2]))
  expect_true(!is.na(inc$person_days[1]))
  expect_true(is.na(inc$person_days[2]))
  expect_true(!is.na(inc$person_years[1]))
  expect_true(is.na(inc$person_years[2]))
  expect_true(inc$n_events[1] == 17)
  expect_true(is.na(inc$n_events[2]))
  expect_true(!is.na(inc$incidence_100000_pys[1]))
  expect_true(is.na(inc$incidence_100000_pys[2]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_lower[1]))
  expect_true(is.na(inc$incidence_100000_pys_95CI_lower[2]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_upper[1]))
  expect_true(is.na(inc$incidence_100000_pys_95CI_upper[2]))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: multiple overlapping outcomes", {
  skip_on_cran()
  # two
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-04-29"),
      as.Date("2019-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2021-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c("1", "1"),
    cohort_start_date = c(as.Date("2020-04-26"), as.Date("2020-11-10")),
    cohort_end_date = c(as.Date("2020-05-17"), as.Date("2020-12-17"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  expect_true(all(inc$n_persons) == 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # three
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-04-29"),
      as.Date("2019-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2021-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "1", "1"),
    cohort_start_date = c(
      as.Date("2020-04-26"),
      as.Date("2020-11-08"),
      as.Date("2020-11-10")
    ),
    cohort_end_date = c(
      as.Date("2020-05-17"),
      as.Date("2020-11-09"),
      as.Date("2020-12-17")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  expect_true(all(inc$n_persons) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: cohort before period start ending after period", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1990, 1990),
    month_of_birth = c(01, 01),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-07-31"),
      as.Date("2000-07-31")
    ),
    observation_period_end_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c("1", "2"),
    cohort_start_date = c(
      as.Date("2000-08-02"),
      as.Date("2001-06-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-01"),
      as.Date("2001-07-01")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator", cohortDateRange = c(as.Date("2001-01-01"), as.Date("2001-12-31"))
  )

  # regardless of washout we expect one event
  # with only one participant
  # person 1s outcome starts before period and ends after

  # no washout
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = FALSE,
    interval = c("Years"),
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(all(inc$n_events == c(1)))

  # washout
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = FALSE,
    interval = c("Years"),
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(all(inc$n_events == c(1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check full period requirement - year", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2021-06-06"),
      as.Date("2021-06-06")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  expect_true(inc$n_persons[1] == 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # edge case first day to last of the year
  # still expect this to work
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )
  expect_true(inc$n_persons[1] == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check full period requirement - month", {
  skip_on_cran()
  # expected to work
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-06-06"),
      as.Date("2020-06-06")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Months"),
    minCellCount = 0
  )
  expect_true(nrow(inc) >= 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # edge case first day to last of the month
  # still expect this to work
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-01-31"),
      as.Date("2020-01-31")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Months"),
    minCellCount = 0
  )
  expect_true(inc$n_persons == 1)
  expect_true(nrow(inc) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check completeDatabaseIntervals", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2019-05-09"),
      as.Date("2019-02-02")
    ),
    observation_period_end_date = c(
      as.Date("2022-06-01"),
      as.Date("2021-06-06")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )

  # full periods required TRUE
  # repetitive events TRUE
  # - we expect to start in 2020 (both start during 2019)
  # - we expect to go up to 2021 (id 2 end date is in 2022)
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    outcomeWashout = 0,
    repeatedEvents = TRUE,
    completeDatabaseIntervals = TRUE,
    minCellCount = 0
  )
  expect_true(nrow(inc) == 2)
  expect_true(lubridate::year(inc$incidence_start_date[1]) == "2020")
  expect_true(lubridate::year(inc$incidence_start_date[2]) == "2021")
  # repetitive events FALSE
  # - now we expect only to use 2020 (id 2 obs end is in 21)
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = FALSE,
    completeDatabaseIntervals = TRUE,
    minCellCount = 0
  )
  expect_true(nrow(inc) == 1)
  expect_true(lubridate::year(inc$incidence_start_date[1]) == "2020")

  # full periods required FALSE
  # repetitive events TRUE
  # - we expect to start in 2019
  # - we expect to go up to 2022
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    interval = c("Years"),
    repeatedEvents = TRUE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(nrow(inc) == 4)
  expect_true(lubridate::year(inc$incidence_start_date[1]) == "2019")
  expect_true(lubridate::year(inc$incidence_start_date[2]) == "2020")
  expect_true(lubridate::year(inc$incidence_start_date[3]) == "2021")
  expect_true(lubridate::year(inc$incidence_start_date[4]) == "2022")
  # repetitive events FALSE
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = FALSE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(nrow(inc) == 3)
  expect_true(lubridate::year(inc$incidence_start_date[1]) == "2019")
  expect_true(lubridate::year(inc$incidence_start_date[2]) == "2020")
  expect_true(lubridate::year(inc$incidence_start_date[3]) == "2021")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check insufficient study days", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = c(1995, 1995),
    month_of_birth = c(07, 07),
    day_of_birth = c(01, 01)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2019-05-09"),
      as.Date("2019-02-02")
    ),
    observation_period_end_date = c(
      as.Date("2019-06-01"),
      as.Date("2019-06-06")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2019-06-06")),
    cohort_end_date = c(as.Date("2019-06-06"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )

  # we have less than a year of follow up
  # so we should return an empty tibble if full periods are required
  # and we´re looking for yearly incidence
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = TRUE,
    completeDatabaseIntervals = TRUE,
    minCellCount = 0
  )

  expect_true(nrow(inc) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check with and without study start and end date", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3", "4", "5", "6"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 07,
    day_of_birth = 01
  )
  # one person leaving before 2010
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4", "5", "6"),
    person_id = c("1", "2", "3", "4", "5", "6"),
    observation_period_start_date = c(
      rep(as.Date("2007-01-01"), 5),
      as.Date("2010-06-01")
    ),
    observation_period_end_date = c(
      rep(as.Date("2022-12-31"), 4),
      as.Date("2009-06-01"),
      as.Date("2010-11-01")
    )
  )
  outcomeTable <- dplyr::bind_rows(
    # 1 event before obs start ending after obs end
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "1",
      cohort_start_date = c(as.Date("2002-01-01")),
      cohort_end_date = c(as.Date("2022-12-31"))
    ),
    # 2 multiple events
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "2",
      cohort_start_date = c(as.Date("2008-06-01")),
      cohort_end_date = c(as.Date("2008-10-01"))
    ),
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "2",
      cohort_start_date = c(as.Date("2008-11-01")),
      cohort_end_date = c(as.Date("2010-10-14"))
    ),
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "2",
      cohort_start_date = c(as.Date("2010-12-01")),
      cohort_end_date = c(as.Date("2011-06-18"))
    ),
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "2",
      cohort_start_date = c(as.Date("2011-06-01")),
      cohort_end_date = c(as.Date("2012-12-31"))
    ),
    # 3 multiple events into the period
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "3",
      cohort_start_date = c(as.Date("2009-06-01")),
      cohort_end_date = c(as.Date("2010-02-01"))
    ),
    tibble::tibble(
      cohort_definition_id = 1,
      subject_id = "3",
      cohort_start_date = c(as.Date("2010-06-01")),
      cohort_end_date = c(as.Date("2022-12-31"))
    )
  )


  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  # no study period required
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator1")
  # study period
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator2",
    cohortDateRange = c(as.Date("2009-01-01"), as.Date("2011-01-01"))
  )

  # no washout, repetitive events
  inc1A <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  inc2A <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  # given the settings above we would expect the same results for 2010
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())

  # 365 washout, repetitive events
  inc1B <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  inc2B <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  # given the settings above we would expect the same results for 2010
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())

  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study start and end date 10000", {
  skip_on_cran()
  # with one outcome per person
  cdm <- mockIncidencePrevalenceRef(
    sampleSize = 10000,
    earliestObservationStartDate = as.Date("2000-01-01"),
    latestDateOfBirth = as.Date("2005-01-01"),
    maxOutcomes = 1,
    minOutcomeDays = 100,
    maxOutcomeDays = 1000
  )

  # no study period required
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator1")
  # study period
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator2",
    cohortDateRange = c(as.Date("2009-01-01"), as.Date("2011-01-01"))
  )

  # no washout, repetitive events
  inc1A <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  inc2A <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("incidence_100000_pys") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("incidence_100000_pys") %>%
      dplyr::pull())

  # 365 washout, repetitive events
  inc1B <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  inc2B <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())

  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("incidence_100000_pys") %>%
    dplyr::pull() ==
    inc1B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("incidence_100000_pys") %>%
      dplyr::pull())
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # with multiple outcomes per person
  cdm <- mockIncidencePrevalenceRef(
    sampleSize = 10000,
    earliestObservationStartDate = as.Date("2000-01-01"),
    latestDateOfBirth = as.Date("2005-01-01"),
    maxOutcomes = 5,
    minOutcomeDays = 100,
    maxOutcomeDays = 1000
  )

  # no study period required
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator1"
  )
  # study period
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator2",
    cohortDateRange = c(as.Date("2009-01-01"), as.Date("2011-01-01"))
  )

  # no washout, repetitive events
  inc1A <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  inc2A <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("incidence_100000_pys") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("incidence_100000_pys") %>%
      dplyr::pull())

  # 365 washout, repetitive events
  inc1B <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  inc2B <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
    dplyr::select("incidence_100000_pys") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(lubridate::year(incidence_start_date) == 2010) %>%
      dplyr::select("incidence_100000_pys") %>%
      dplyr::pull())

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check messages when vebose is true", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  expect_message(estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors with mock", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  # not a cdm reference
  expect_error(estimateIncidence(
    cdm = "a",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  ))

  # wrong type
  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1"
  ))

  # no study pop
  expect_error(estimateIncidence(cdm,
    outcomeTable = "outcome",
    interval = c("months"),
    denominatorTable = "denominator1"
  ))
  expect_error(estimateIncidence(cdm,
    outcomeTable = "outcome",
    interval = c("months"),
    denominatorTable = "denominator",
    denominatorCohortId = 999
  ))


  # outcome definition id doesn't exist in cohort set
  expect_error(estimateIncidence(cdm,
    outcomeTable = "outcome",
    interval = c("months"),
    denominatorTable = "denominator",
    outcomeCohortId = 11
  ))

  # returnParticipants only works with permanent tables
  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    temporary = TRUE,
    returnParticipants = TRUE
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: multiple observation periods", {
  skip_on_cran()
  # create data for hypothetical people to test
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8507"),
    year_of_birth = c(1998, 1976),
    month_of_birth = c(02, 06),
    day_of_birth = c(12, 01)
  )

  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4"),
    person_id = c("1", "1", "2", "2"),
    observation_period_start_date = c(
      as.Date("2005-04-01"),
      as.Date("2009-04-10"),
      as.Date("2010-08-20"),
      as.Date("2012-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-11-29"),
      as.Date("2016-01-02"),
      as.Date("2011-12-11"),
      as.Date("2015-06-01")
    )
  )

  conditionX <- tibble::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c("1", "2"),
    cohort_start_date = c(
      as.Date("2002-07-19"),
      as.Date("2010-12-11")
    ),
    cohort_end_date = c(
      as.Date("2016-01-02"),
      as.Date("2015-06-01")
    )
  )

  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c("1", "1", "2", "2"),
    cohort_start_date = c(
      as.Date("2005-08-09"),
      as.Date("2010-01-11"),
      as.Date("2010-12-21"),
      as.Date("2014-04-04")
    ),
    cohort_end_date = c(
      as.Date("2005-08-09"),
      as.Date("2010-01-11"),
      as.Date("2010-12-21"),
      as.Date("2014-04-04")
    )
  )

  # should only pick up one of the four observation periods,
  # as the inclusion of the cohorts is only well defined for one
  # (entry event in the observation period)
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )

  incW0 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect all events if we have zero days washout
  expect_true(sum(incW0$n_events) == 1)
  # expect a certain number of person_time days (intersection of observation periods and inclusion criteria)
  expect_true(incW0 %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2011-12-11"), as.Date("2010-12-11"))) + 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # Change the inclusion so that both patients have valid observation periods. Now 1 should have two, and 2 one.
  # Should capture the final part of the first observation period, and the initial part of the second for person 1
  conditionX <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "1", "2"),
    cohort_start_date = c(
      as.Date("2005-07-19"),
      as.Date("2009-04-10"),
      as.Date("2010-12-11")
    ),
    cohort_end_date = c(
      as.Date("2005-08-11"),
      as.Date("2015-01-02"),
      as.Date("2011-12-11")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )

  incW10 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 10,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  # expect all events if we have ten days washout
  expect_true(sum(incW10$n_events) == 3)
  # expect a certain number of person_time days (intersection of observation periods and inclusion criteria)
  expect_true(incW10 %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"), as.Date("2005-07-19"))) + 1 - 2 + as.numeric(difftime(as.Date("2015-01-02"), as.Date("2009-04-10"))) + 1 - 10 + as.numeric(difftime(as.Date("2011-12-11"), as.Date("2010-12-11"))) + 1 - 10)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # try event not counted for outcome but counted for washout as denominator (before observ period)
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1),
    subject_id = c("1", "1", "1", "2", "2"),
    cohort_start_date = c(
      as.Date("2005-07-11"),
      as.Date("2005-08-09"),
      as.Date("2010-01-11"),
      as.Date("2010-12-21"),
      as.Date("2014-04-04")
    ),
    cohort_end_date = c(
      as.Date("2005-07-11"),
      as.Date("2005-08-09"),
      as.Date("2010-01-11"),
      as.Date("2010-12-21"),
      as.Date("2014-04-04")
    )
  )

  # now we would expect same number of events, but three less days in the denominator
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX,
    outcomeTable = outcomeTable
  )
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )
  inc_PreWashout <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 10,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(sum(inc_PreWashout$n_events) == 3)
  expect_true(inc_PreWashout %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"), as.Date("2005-07-19"))) + 1 - 2 + as.numeric(difftime(as.Date("2015-01-02"), as.Date("2009-04-10"))) + 1 - 10 + as.numeric(difftime(as.Date("2011-12-11"), as.Date("2010-12-11"))) + 1 - 10 - 3)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # multiple events in one of the observation periods of person 1
  conditionX <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "1", "2"),
    cohort_start_date = c(
      as.Date("2005-06-19"),
      as.Date("2009-04-10"),
      as.Date("2010-12-11")
    ),
    cohort_end_date = c(
      as.Date("2005-08-11"),
      as.Date("2015-01-02"),
      as.Date("2011-12-11")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )

  inc_Mult1_W0 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  inc_Mult1_W30 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 30,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  # we should have 4 events with washout 0, but 3 events with washout 30
  expect_true(sum(inc_Mult1_W0$n_events) == 4)
  expect_true(sum(inc_Mult1_W30$n_events) == 3)
  expect_true(inc_Mult1_W0 %>% dplyr::select(person_days) %>%
                sum() == as.numeric(difftime(as.Date("2005-08-11"),
                                             as.Date("2005-06-19"))) + 1 +
                as.numeric(difftime(as.Date("2015-01-02"),
                                    as.Date("2009-04-10"))) + 1 +
                as.numeric(difftime(as.Date("2011-12-11"),
                                    as.Date("2010-12-11"))) + 1)
  expect_true(inc_Mult1_W30 %>% dplyr::select(person_days) %>%
                sum() == as.numeric(difftime(as.Date("2005-08-11"),
                                             as.Date("2005-06-19"))) - 30 +
                as.numeric(difftime(as.Date("2015-01-02"),
                                    as.Date("2009-04-10"))) +
                1 - 30 + as.numeric(difftime(as.Date("2011-12-11"),
                                             as.Date("2010-12-11"))) + 1 - 30)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # The first event of person 1 will not be included in the observation period
  # but should also influence the second event with the washout
  conditionX <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c("1", "1", "2"),
    cohort_start_date = c(
      as.Date("2005-07-19"),
      as.Date("2009-04-10"),
      as.Date("2010-12-11")
    ),
    cohort_end_date = c(
      as.Date("2005-08-11"),
      as.Date("2015-01-02"),
      as.Date("2011-12-11")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )

  inc_PreWashEv <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 30,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  # we should have 2 events with washout 30
  expect_true(sum(inc_PreWashEv$n_events) == 2)
  expect_true(inc_PreWashEv %>% dplyr::select(person_days) %>% sum() ==
                as.numeric(difftime(as.Date("2005-08-11"),
                                    as.Date("2005-07-19"))) - 30 + 7 +
                as.numeric(difftime(as.Date("2015-01-02"),
                                    as.Date("2009-04-10"))) + 1 - 30 +
                as.numeric(difftime(as.Date("2011-12-11"),
                                    as.Date("2010-12-11"))) + 1 - 30)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # three observation periods for 1 person and a
  # couple of consecutive events lost to washout
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4"),
    person_id = c("1", "1", "1", "2"),
    observation_period_start_date = c(
      as.Date("2005-04-01"),
      as.Date("2009-04-10"),
      as.Date("2010-08-20"),
      as.Date("2012-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-11-29"),
      as.Date("2010-01-02"),
      as.Date("2011-12-11"),
      as.Date("2015-06-01")
    )
  )

  conditionX <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1),
    subject_id = c("1", "1", "1", "2"),
    cohort_start_date = c(
      as.Date("2005-04-01"),
      as.Date("2009-06-10"),
      as.Date("2010-08-20"),
      as.Date("2010-01-01")
    ),
    cohort_end_date = c(
      as.Date("2005-11-29"),
      as.Date("2010-01-02"),
      as.Date("2011-10-11"),
      as.Date("2015-06-01")
    )
  )

  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 1, 1, 1, 1, 1, 1),
    subject_id = c("1", "1", "1", "1", "1", "1", "2"),
    cohort_start_date = c(
      as.Date("2005-08-09"),
      as.Date("2005-08-10"),
      as.Date("2005-08-11"),
      as.Date("2009-11-11"),
      as.Date("2009-11-21"),
      as.Date("2010-12-21"),
      as.Date("2014-04-04")
    ),
    cohort_end_date = c(
      as.Date("2005-08-09"),
      as.Date("2005-08-10"),
      as.Date("2005-08-11"),
      as.Date("2009-11-11"),
      as.Date("2009-11-21"),
      as.Date("2010-12-21"),
      as.Date("2014-04-04")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )

  inc_3op <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 1,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  # we should have 4 events with washout 1
  expect_true(sum(inc_3op$n_events) == 4)
  expect_true(inc_3op %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-11-29"), as.Date("2005-04-01"))) - 3 + 1 + as.numeric(difftime(as.Date("2010-01-02"), as.Date("2009-06-10"))) + 1 - 2 + as.numeric(difftime(as.Date("2011-10-11"), as.Date("2010-08-20"))) + 1 - 1)

  # try repeated events FALSE.
  inc_repev <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    outcomeWashout = 1,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  # we should have 1 event, and the person only counting for the denom. up until the first event
  expect_true(sum(inc_repev$n_events) == 1)
  expect_true(inc_repev %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-09"), as.Date("2005-04-01"))) + 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check confidence intervals", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(
    sampleSize = 10000
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2011-01-01"))
  )
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    minCellCount = 0,
    completeDatabaseIntervals = TRUE
  )

  expect_equal(inc$incidence_100000_pys_95CI_lower,
    epitools::pois.exact(inc$n_events, inc$person_years)$lower * 100000,
    tolerance = 1e-2
  )
  expect_equal(inc$incidence_100000_pys_95CI_upper,
    epitools::pois.exact(inc$n_events, inc$person_years)$upper * 100000,
    tolerance = 1e-2
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check attrition", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = c("Male", "Female")
  )
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  )
  # for female cohort we should have a row for those excluded for not being male
  expect_true(any("Not Female" ==
    incidenceAttrition(inc) %>%
      dplyr::filter(denominator_sex == "Female") %>%
      dplyr::pull(.data$reason)))
  # for male, the opposite
  expect_true(any("Not Male" == incidenceAttrition(inc) %>%
    dplyr::filter(denominator_sex == "Male") %>%
    dplyr::pull(.data$reason)))

  # check we can pick out specific analysis attrition
  expect_true(nrow(incidenceAttrition(result = inc) %>%
    dplyr::filter(analysis_id == 1)) > 1)
  expect_true(nrow(incidenceAttrition(result = inc) %>%
    dplyr::filter(analysis_id == 2)) > 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # check obscuring counts
  cdm <- mockIncidencePrevalenceRef(sampleSize = 4)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = c("Male", "Female")
  )
  inc <- estimateIncidence(cdm,
                           denominatorTable = "denominator",
                           outcomeTable = "outcome",
                           interval = "years"
  )
  expect_true(incidenceAttrition(inc) %>%
                dplyr::filter(reason == "Not Male") %>%
                dplyr::pull("excluded_subjects") == "<5")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check attrition with complete database intervals", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = c(
      as.Date("2000-06-01"),
      as.Date("2000-06-01")
    ),
    observation_period_end_date = c(
      as.Date("2000-07-01"),
      as.Date("2012-06-01")
    )
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years", minCellCount = 0
  )

  expect_true(incidenceAttrition(inc) %>%
    dplyr::filter(reason == "Not observed during the complete database interval") %>%
    dplyr::pull(excluded_subjects) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check compute permanent", {
  skip_on_cran()

  # using temp
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  )
  # if using temp tables
  # we have temp tables created by dbplyr
  # expect_true(any(stringr::str_starts(
  #   CDMConnector::listTables(attr(cdm, "dbcon")),
  #   "dbplyr_"
  # )))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # using permanent
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall",
    temporary = FALSE
  )

  # no temp tables created by dbplyr
  expect_false(any(stringr::str_starts(
    CDMConnector::listTables(attr(cdm, "dbcon")),
    "dbplyr_"
  )))

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall",
    temporary = FALSE,
    returnParticipants = TRUE
  )
  expect_true(any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
      schema = attr(cdm, "write_schema")
    ),
    "inc_participants"
  )))
  expect_false(any(stringr::str_starts(
    CDMConnector::listTables(attr(cdm, "dbcon")),
    "dbplyr_"
  )))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check participants", {
  skip_on_cran()

  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- c(schema = "main", prefix = "test_")

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "dpop",
    sex = c("Male", "Female", "Both"),
    ageGroup = list(
      c(0, 50),
      c(51, 100)
    )
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    interval = "overall",
    temporary = FALSE,
    returnParticipants = TRUE
  )

  # TODO
  # we should have cleaned up all the intermediate tables
  # expect_true(all(CDMConnector::listTables(attr(cdm, "dbcon"),
  #   schema = attr(cdm, "write_schema")
  # ) %in%
  #   c(
  #     "test_dpop",
  #     "test_inc_participants1",
  #     "test_dpop_attrition",
  #     "test_dpop_set",
  #     "test_dpop_count",
  #     "vocabulary",
  #     "cdm_source", "outcome", "outcome_set", "outcome_count",
  #     "outcome_attrition",
  #     "target",
  #     "target_set", "target_count","target_attrition" ,
  #     "observation_period", "person"
  #   )))
  expect_true(all(!c(
    "incidence_analysis_1",
    "incidence_working_5"
  ) %in%
    CDMConnector::listTables(attr(cdm, "dbcon"),
      schema = attr(
        cdm,
        "write_schema"
      )
    )))
  expect_equal(
    names(participants(inc, 1) %>%
      head(1) %>%
      dplyr::collect()),
    c(
      "subject_id",
      "cohort_start_date",
      "cohort_end_date",
      "outcome_start_date"
    )
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: overwriting participants", {
  skip_on_cran()

  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- c(schema = "main", prefix = "test_")

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "dpop",
    ageGroup = list(
      c(0, 50),
      c(51, 100)
    )
  )
  inc1 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    denominatorCohortId = 1,
    outcomeTable = "outcome",
    temporary = FALSE,
    returnParticipants = TRUE
  )
  inc1_count <- nrow(participants(inc1, 1) %>% dplyr::collect())

  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    denominatorCohortId = 2,
    outcomeTable = "outcome",
    temporary = FALSE,
    returnParticipants = TRUE
  )
  # participants from prev1 should still be the same
  # (ie no interference from having rerun the function)
  expect_true(nrow(participants(inc1, 1) %>% dplyr::collect()) == inc1_count)

  # we should have two tables with participants
  # one for each function call
  expect_true(length(stringr::str_subset(
    CDMConnector::listTables(attr(cdm, "dbcon"),
      schema = attr(cdm, "write_schema")
    ),
    "participants"
  )) == 2)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: if missing cohort attributes", {
  # missing cohort_set
  cdm <- mockIncidencePrevalenceRef()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  attr(cdm$outcome, "cohort_set") <- NULL
  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  ))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # missing cohort_count
  cdm <- mockIncidencePrevalenceRef()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  attr(cdm$outcome, "cohort_count") <- NULL
  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  ))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: empty outcome cohort", {

  cdm <- mockIncidencePrevalenceRef(sampleSize = 200,
                                    earliestObservationStartDate = as.Date("2000-01-01"),
                                    latestObservationStartDate = as.Date("2000-01-01"),
                                    minDaysToObservationEnd = 365,
                                    maxDaysToObservationEnd = 365)

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  cdm$outcome <-  cdm$outcome %>% dplyr::filter(cohort_definition_id == 99)

  expect_no_error(inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  ))
  expect_true(all(!is.na(inc$n_persons)))
  expect_true(sum(inc$n_events)==0)

  # make sure we also have a confidence interval even in the case of an empty outcome cohort
  expect_true(all(inc$incidence_100000_pys == 0))
  expect_true(all(inc$incidence_100000_pys_95CI_lower == 0))
  expect_true(all(inc$incidence_100000_pys_95CI_upper > 0))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: incidence using strata vars", {

  cdm <- mockIncidencePrevalenceRef(sampleSize = 2000,
                                    outPre = 0.7)

  cdm <- generateDenominatorCohortSet(cdm = cdm,
                                      name = "denominator")

  inc_orig <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 2011,
                                             "first", "second")) %>%
    CDMConnector::compute_query()

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  expect_true(all(c("strata_name", "strata_level") %in%
                colnames(inc)))
  expect_true(all(c("Overall", "my_strata") %in%
   unique(inc %>%
     dplyr::pull("strata_name"))))
  expect_true(all(c("Overall",
                    "first", "second") %in%
                unique(inc %>%
                dplyr::pull("strata_level"))))

  # original without strata should be the same as "Overall" strata
  expect_equal(inc_orig,
  inc %>%
    dplyr::filter(strata_name == "Overall") %>%
    dplyr::select(!c("strata_name", "strata_level")))


  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata2 =  dplyr::if_else(month(cohort_start_date)<7,
                                               "a", "b")) %>%
    CDMConnector::compute_query()

  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata","my_strata2"))
  )
  expect_true(all(c("strata_name", "strata_level") %in%
                    colnames(inc2)))
  expect_true(all(c("Overall", "my_strata and my_strata2") %in%
                unique(inc2 %>%
                         dplyr::pull("strata_name"))))
  expect_true(all(c("Overall",
                    "first and a", "first and b",
                    "second and a", "second and b") %in%
                unique(inc2 %>%
                         dplyr::pull("strata_level"))))

  inc3 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"),
                  c("my_strata2"),
                  c("my_strata", "my_strata2")))

  expect_true(all(c("strata_name", "strata_level") %in%
                    colnames(inc3)))
  expect_true(all(c("Overall", "my_strata", "my_strata2",
                    "my_strata and my_strata2") %in%
                unique(inc3 %>%
                         dplyr::pull("strata_name"))))
  expect_true(all(c("Overall",
                    "first", "second",
                    "a", "b",
                    "first and a", "first and b",
                    "second and a", "second and b") %in%
                unique(inc3 %>%
                         dplyr::pull("strata_level"))))


  # without overall strata
  inc4 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"),
                  c("my_strata2"),
                  c("my_strata", "my_strata2")),
    includeOverallStrata = FALSE)
  expect_false("Overall" %in% unique(inc4 %>%
                                       dplyr::pull("strata_name")))
  expect_true(all(c("my_strata", "my_strata2",
                    "my_strata and my_strata2") %in%
                    unique(inc4 %>%
                             dplyr::pull("strata_name"))))
  expect_false("Overall" %in% unique(inc4 %>%
                                       dplyr::pull("strata_level")))
  expect_true(all(c("first", "second",
                    "a", "b",
                    "first and a", "first and b",
                    "second and a", "second and b") %in%
                    unique(inc4 %>%
                             dplyr::pull("strata_level"))))

  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("not_a_col"))))

  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata", "not_a_col"))))

 expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"), c("not_a_col"))))

  CDMConnector::cdm_disconnect(cdm)
})
