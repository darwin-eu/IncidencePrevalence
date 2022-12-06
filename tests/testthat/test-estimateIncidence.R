
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator  <- generateDenominatorCohortSet(cdm = cdm)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = FALSE,
    interval = c("months"),
    confidenceInterval = "none",
    verbose = TRUE
  )

  # check analysis settings tibble
  expect_true(all(c(
    "analysis_id",
    "analysis_outcome_washout",
    "analysis_repeated_events",
    "analysis_interval",
    "analysis_complete_database_intervals",
    "analysis_confidence_interval",
    "analysis_min_cell_count",
    "outcome_cohort_id",
    "denominator_cohort_id",
    "denominator_age_group",
    "denominator_min_age",
    "denominator_max_age",
    "denominator_sex",
    "denominator_days_prior_history",
    "denominator_start_date",
    "denominator_end_date"
  ) %in%
    names(settings(inc))))

  # check estimates tibble
  expect_true(all(c(
    "analysis_id",
    "n_persons",
    "person_days",
    "person_years",
    "n_events",
    "ir_100000_pys",
    "ir_100000_pys_low",
    "ir_100000_pys_high",
    "time", "start_time", "end_time",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(inc)))

  expect_true(tibble::is_tibble(attrition(inc)))
  expect_true(!is.null(sqlTrace(inc)))

  expect_true(is.list(participants(inc))) # list of references to participants
  expect_true(tibble::is_tibble(participants(inc,1) %>%
                                  dplyr::collect()))
  expect_true(participants(inc,1) %>%
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = FALSE,
    interval = c("months"),
    verbose = TRUE
  )
  expect_true(nrow(inc) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check working example 2", {
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <-  generateDenominatorCohortSet(cdm = cdm)

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
    outcomeWashout = NULL,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 1)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = NULL,
    minCellCount = 0,
    interval = "weeks",
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(inc$n_events) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study periods", {
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    repeatedEvents = TRUE,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )

  # we expect 12 months of which the last in december
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(length(inc$time) == 12)


  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    repeatedEvents = TRUE,
    minCellCount = 0,
    completeDatabaseIntervals = TRUE
  )

  # now with completeDatabaseIntervals is TRUE
  # we expect 10 months of which the last in november
  expect_true(length(inc$time) == 10)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check overall", {
  personTable <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2005-01-15"),
                                      as.Date("2005-01-15")),
    observation_period_end_date = c(as.Date("2007-05-01"),
                                    as.Date("2011-06-15"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1","2","2"),
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

  cdm$denominator <- generateDenominatorCohortSet( cdm = cdm,
                              startDate = as.Date("2007-01-01"))

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
  expect_true(inc$start_time ==
                as.Date("2007-01-01"))
  expect_true(inc$end_time ==
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
  expect_true(inc$start_time ==
                as.Date("2007-01-01"))
  expect_true(inc$end_time ==
                as.Date("2011-06-15")) # date of end of obs

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check person days", {
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
    observation_period_start_date = c(as.Date("2007-01-01"),
                                      as.Date("2007-01-01")),
    observation_period_end_date = c(as.Date("2022-12-31"),
                                    as.Date("2022-10-05"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2021-06-27")),
    cohort_end_date = c(as.Date("2021-06-27"))
  )


  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    interval = c("Years"),
    verbose = TRUE,
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  # startDate during a year (with year as interval)
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-02-01")
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc$n_events[1] == 1)
  expect_true(inc$n_events[2] == 3)


  # startDate during a month (with month as interval)
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2011-01-15")
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    interval = c("months"),
    verbose = TRUE,
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc$n_events[1] == 1)
  expect_true(inc$n_events[2] == 1)
  expect_true(inc$n_events[3] == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check washout windows", {
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

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

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
    outcomeWashout = NULL,
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
})

test_that("mock db: check events overlapping with start of a period", {
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
    observation_period_start_date = c(as.Date("2000-01-21"),
                                      as.Date("2007-01-01")),
    observation_period_end_date = c(as.Date("2022-12-31"),
                                    as.Date("2022-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2020-06-27")),
    cohort_end_date = c(as.Date("2020-07-19"))
  )


  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0
  )

  expect_true(all(inc$n_persons == 1))

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
    observation_period_start_date = c(as.Date("2000-01-21"),
                                      as.Date("2007-01-01")),
    observation_period_end_date = c(as.Date("2022-12-31"),
                                    as.Date("2022-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "1"),
    cohort_start_date = c(as.Date("2020-06-27"), as.Date("2020-07-30")),
    cohort_end_date = c(as.Date("2020-07-19"), as.Date("2020-08-20"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0
  )
  expect_true(all(inc2$n_persons == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: compare results from months and years", {
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
    cohort_definition_id = c("1"),
    subject_id = c("1"),
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
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-01-01"),
    endDate = as.Date("2011-12-31")
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
})

test_that("mock db: check entry and event on same day", {
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
    cohort_definition_id = "1",
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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  incWithoutRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    outcomeWashout = NULL,
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
    outcomeWashout = NULL,
    interval = "years",
    minCellCount = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(incWithRep$n_events) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: cohort start overlaps with the outcome", {
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
    observation_period_start_date = c(as.Date("2020-05-09"),
                                      as.Date("2019-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2020-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0
  )

  expect_true(all(inc$n_persons == c(1, 2)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check outcome before observation period start", {
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
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2020-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "1"),
    cohort_start_date = c(as.Date("1999-01-01")),
    cohort_end_date = c(as.Date("1999-05-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2004-01-01")
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
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(incRep$n_persons == 2))
  expect_true(all(incNoRep$n_persons == 1))

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
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2020-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("1999-01-01")),
    cohort_end_date = c(as.Date("2021-05-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2004-01-01")
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
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(incRep$n_persons == 1))
  expect_true(all(incNoRep$n_persons == 1))

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
    observation_period_start_date = c(as.Date("2000-01-01"),
                                      as.Date("2000-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2020-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("1999-01-01")),
    cohort_end_date = c(as.Date("2001-05-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2000-01-01"),
    endDate = as.Date("2004-01-01")
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
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    minCellCount = 0
  )

  expect_true(all(incRep$n_persons == c(1, 2, 2, 2)))
  expect_true(all(incNoRep$n_persons == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check minimum counts", {
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
  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
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
  expect_true(!is.na(inc$ir_100000_pys[1]))
  expect_true(!is.na(inc$ir_100000_pys[2]))
  expect_true(!is.na(inc$ir_100000_pys_low[1]))
  expect_true(!is.na(inc$ir_100000_pys_low[2]))
  expect_true(!is.na(inc$ir_100000_pys_high[1]))
  expect_true(!is.na(inc$ir_100000_pys_high[2]))

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
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
  expect_true(!is.na(inc$ir_100000_pys[1]))
  expect_true(is.na(inc$ir_100000_pys[2]))
  expect_true(!is.na(inc$ir_100000_pys_low[1]))
  expect_true(is.na(inc$ir_100000_pys_low[2]))
  expect_true(!is.na(inc$ir_100000_pys_high[1]))
  expect_true(is.na(inc$ir_100000_pys_high[2]))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: multiple overlapping outcomes", {
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
    observation_period_start_date = c(as.Date("2020-04-29"),
                                      as.Date("2019-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2021-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "1"),
    cohort_start_date = c(as.Date("2020-04-26"), as.Date("2020-11-10")),
    cohort_end_date = c(as.Date("2020-05-17"), as.Date("2020-12-17"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0
  )
  expect_true(all(inc$n_persons) == 1)

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
    observation_period_start_date = c(as.Date("2020-04-29"),
                                      as.Date("2019-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2021-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

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
    observation_period_start_date = c(as.Date("2000-07-31"),
                                      as.Date("2000-07-31")),
    observation_period_end_date = c(as.Date("2010-01-01"),
                                    as.Date("2010-01-01"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1", "1"),
    subject_id = c("1", "2"),
    cohort_start_date = c(as.Date("2000-08-02"),
                          as.Date("2001-06-01")),
    cohort_end_date = c(as.Date("2020-01-01"),
                        as.Date("2001-07-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date(as.Date("2001-01-01")),
    endDate = as.Date(as.Date("2001-12-31"))
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
    verbose = TRUE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(all(inc$n_events == c(1)))

  # washout
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = NULL,
    repeatedEvents = FALSE,
    interval = c("Years"),
    verbose = TRUE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(all(inc$n_events == c(1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check full period requirement - year", {
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
    observation_period_start_date = c(as.Date("2020-05-09"),
                                      as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2021-06-06"),
                                    as.Date("2021-06-06"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0
  )
  expect_true(inc$n_persons[1] == 1)

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
    observation_period_start_date = c(as.Date("2020-05-09"),
                                      as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2020-12-31"),
                                    as.Date("2020-12-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Years"),
    verbose = TRUE,
    minCellCount = 0
  )
  expect_true(inc$n_persons[1] == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check full period requirement - month", {
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
    observation_period_start_date = c(as.Date("2020-05-09"),
                                      as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2020-06-06"),
                                    as.Date("2020-06-06"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Months"),
    verbose = TRUE,
    minCellCount = 0
  )
  expect_true(nrow(inc) >= 1)


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
    observation_period_start_date = c(as.Date("2020-05-09"),
                                      as.Date("2020-01-01")),
    observation_period_end_date = c(as.Date("2020-01-31"),
                                    as.Date("2020-01-31"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(20, 30))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    outcomeWashout = NULL,
    repeatedEvents = TRUE,
    interval = c("Months"),
    verbose = TRUE,
    minCellCount = 0
  )
  expect_true(inc$n_persons == 1)
  expect_true(nrow(inc) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check completeDatabaseIntervals", {
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
    observation_period_start_date = c(as.Date("2019-05-09"),
                                      as.Date("2019-02-02")),
    observation_period_end_date = c(as.Date("2022-06-01"),
                                    as.Date("2021-06-06"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
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
    repeatedEvents = TRUE,
    completeDatabaseIntervals = TRUE,
    minCellCount = 0
  )
  expect_true(nrow(inc) == 2)
  expect_true(inc$time[1] == "2020")
  expect_true(inc$time[2] == "2021")
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
  expect_true(inc$time[1] == "2020")

  # full periods required FALSE
  # repetitive events TRUE
  # - we expect to start in 2019
  # - we expect to go up to 2022
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = TRUE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(nrow(inc) == 4)
  expect_true(inc$time[1] == "2019")
  expect_true(inc$time[2] == "2020")
  expect_true(inc$time[3] == "2021")
  expect_true(inc$time[4] == "2022")
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
  expect_true(inc$time[1] == "2019")
  expect_true(inc$time[2] == "2020")
  expect_true(inc$time[3] == "2021")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check insufficient study days", {
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
    observation_period_start_date = c(as.Date("2019-05-09"),
                                      as.Date("2019-02-02")),
    observation_period_end_date = c(as.Date("2019-06-01"),
                                    as.Date("2019-06-06"))
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(as.Date("2019-06-06")),
    cohort_end_date = c(as.Date("2019-06-06"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
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

test_that("mock db: check conversion of user inputs", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = 1,
    outcomeTable = "outcome",
    outcomeCohortId = 1,
    outcomeWashout = NA
  )
  expect_true(nrow(inc) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check with and without study start and end date", {
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
      cohort_definition_id = "1",
      subject_id = "1",
      cohort_start_date = c(as.Date("2002-01-01")),
      cohort_end_date = c(as.Date("2022-12-31"))
    ),
    # 2 multiple events
    tibble::tibble(
      cohort_definition_id = "1",
      subject_id = "2",
      cohort_start_date = c(as.Date("2008-06-01")),
      cohort_end_date = c(as.Date("2008-10-01"))
    ),
    tibble::tibble(
      cohort_definition_id = "1",
      subject_id = "2",
      cohort_start_date = c(as.Date("2008-11-01")),
      cohort_end_date = c(as.Date("2010-10-14"))
    ),
    tibble::tibble(
      cohort_definition_id = "1",
      subject_id = "2",
      cohort_start_date = c(as.Date("2010-12-01")),
      cohort_end_date = c(as.Date("2011-06-18"))
    ),
    tibble::tibble(
      cohort_definition_id = "1",
      subject_id = "2",
      cohort_start_date = c(as.Date("2011-06-01")),
      cohort_end_date = c(as.Date("2012-12-31"))
    ),
    # 3 multiple events into the period
    tibble::tibble(
      cohort_definition_id = "1",
      subject_id = "3",
      cohort_start_date = c(as.Date("2009-06-01")),
      cohort_end_date = c(as.Date("2010-02-01"))
    ),
    tibble::tibble(
      cohort_definition_id = "1",
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
  cdm$denominator1 <- generateDenominatorCohortSet(cdm = cdm)
  # study period
  cdm$denominator2 <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2009-01-01"),
    endDate = as.Date("2011-01-01")
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
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
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
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())

  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study start and end date 10000", {
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
  cdm$denominator1 <- generateDenominatorCohortSet(cdm = cdm)
  # study period
  cdm$denominator2 <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2009-01-01"),
    endDate = as.Date("2011-01-01")
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
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("ir_100000_pys") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("ir_100000_pys") %>%
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
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())

  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("ir_100000_pys") %>%
    dplyr::pull() ==
    inc1B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("ir_100000_pys") %>%
      dplyr::pull())



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
  cdm$denominator1 <- generateDenominatorCohortSet(cdm = cdm)
  # study period
  cdm$denominator2 <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2009-01-01"),
    endDate = as.Date("2011-01-01")
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
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1A %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("ir_100000_pys") %>%
    dplyr::pull() ==
    inc2A %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("ir_100000_pys") %>%
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
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_persons") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_persons") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("person_days") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("person_days") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("n_events") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("n_events") %>%
      dplyr::pull())
  expect_true(inc1B %>%
    dplyr::filter(time == 2010) %>%
    dplyr::select("ir_100000_pys") %>%
    dplyr::pull() ==
    inc2B %>%
      dplyr::filter(time == 2010) %>%
      dplyr::select("ir_100000_pys") %>%
      dplyr::pull())

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check messages when vebose is true", {
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
    cohort_definition_id = "1",
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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  expect_message(estimateIncidence(cdm,
    denominatorTable = "denominator",
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    verbose = TRUE
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors with mock", {
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
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  # not a cdm reference
  expect_error(estimateIncidence(
    cdm = "a",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
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
    denominatorCohortId = "999"
  ))


  # no outcomes
  expect_error(estimateIncidence(cdm,
                                 outcomeTable = "outcome1",
                                 interval = c("months"),
                                 denominatorTable = "denominator",
  ))
  expect_error(estimateIncidence(cdm,
                                 outcomeTable = "outcome",
                                 interval = c("months"),
                                 denominatorTable = "denominator",
                                 outcomeCohortId = "11"
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: multiple observation periods", {
  # create data for hypothetical people to test
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507","8507"),
    year_of_birth = c(1998,1976),
    month_of_birth = c(02,06),
    day_of_birth = c(12,01)
  )

  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1","2","3","4"),
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
    cohort_definition_id = c("1", "1"),
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
    cohort_definition_id = c("1","1","1","1"),
    subject_id = c("1","1","2", "2"),
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

  # should only pick up one of the four observation periods, as the inclusion of the cohorts is only well defined for one (entry event in the observation period)
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
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
  expect_true(incW0 %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2011-12-11"),as.Date("2010-12-11"))) + 1)


  # Change the inclusion so that both patients have valid observation periods. Now 1 should have two, and 2 one.
  # Should capture the final part of the first observation period, and the initial part of the second for person 1
  conditionX <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
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
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
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
  expect_true(incW10 %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"),as.Date("2005-07-19"))) + 1 -2 + as.numeric(difftime(as.Date("2015-01-02"),as.Date("2009-04-10"))) + 1 -10 + as.numeric(difftime(as.Date("2011-12-11"),as.Date("2010-12-11"))) + 1 -10)


  # try event not counted for outcome but counted for washout as denominator (before observ period)
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1","1","1","1","1"),
    subject_id = c("1","1","1","2", "2"),
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
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
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
  expect_true(inc_PreWashout %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"),as.Date("2005-07-19"))) + 1 -2 + as.numeric(difftime(as.Date("2015-01-02"),as.Date("2009-04-10"))) + 1 -10 + as.numeric(difftime(as.Date("2011-12-11"),as.Date("2010-12-11"))) + 1 -10 - 3)


  # multiple events in one of the observation periods of person 1
  conditionX <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
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
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
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
  expect_true(inc_Mult1_W0 %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"),as.Date("2005-06-19"))) + 1 + as.numeric(difftime(as.Date("2015-01-02"),as.Date("2009-04-10"))) + 1 + as.numeric(difftime(as.Date("2011-12-11"),as.Date("2010-12-11"))) + 1)
  expect_true(inc_Mult1_W30 %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"),as.Date("2005-06-19"))) -30 + as.numeric(difftime(as.Date("2015-01-02"),as.Date("2009-04-10"))) + 1 -30 + as.numeric(difftime(as.Date("2011-12-11"),as.Date("2010-12-11"))) + 1 - 30)


  # The first event of person 1 will not be included in the observation period but should also influence the second event with the washout
  conditionX <- tibble::tibble(
    cohort_definition_id = c("1", "1", "1"),
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
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
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
  expect_true(inc_PreWashEv %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-11"),as.Date("2005-07-19"))) -30 + 7 + as.numeric(difftime(as.Date("2015-01-02"),as.Date("2009-04-10"))) + 1 -30 + as.numeric(difftime(as.Date("2011-12-11"),as.Date("2010-12-11"))) + 1 - 30)


  # three observation periods for 1 person and a couple of consecutive events lost to washout
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1","2","3","4"),
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
    cohort_definition_id = c("1","1","1","1"),
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
    cohort_definition_id = c("1","1","1","1","1","1","1"),
    subject_id = c("1","1","1","1","1","1","2"),
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
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
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
  expect_true(inc_3op %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-11-29"),as.Date("2005-04-01"))) -3 + 1 + as.numeric(difftime(as.Date("2010-01-02"),as.Date("2009-06-10"))) + 1 -2 + as.numeric(difftime(as.Date("2011-10-11"),as.Date("2010-08-20"))) + 1 - 1)

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
  expect_true(inc_repev %>% dplyr::select(person_days) %>% sum() == as.numeric(difftime(as.Date("2005-08-09"),as.Date("2005-04-01"))) + 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


