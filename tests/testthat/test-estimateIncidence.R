test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalence()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  my_settings <- settings(inc)
  expect_gt(nrow(my_settings), 0)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )
  expect_true(inherits(inc, "summarised_result"))
  expect_identical(
    sort(colnames(settings(inc))),
    sort(c(
      "result_id", "result_type",
      "package_name", "package_version",
      "group", "strata", "additional", "min_cell_count",
      "analysis_complete_database_intervals",
      "analysis_censor_cohort_name",
      "analysis_outcome_washout", "analysis_repeated_events",
      "denominator_age_group", "denominator_sex",
      "denominator_days_prior_observation", "denominator_start_date",
      "denominator_end_date",
      "denominator_requirements_at_entry",
      "denominator_target_cohort_name",
      "denominator_time_at_risk"
    ))
  )

  expect_identical(
    colnames(inc),
    c(
      "result_id", "cdm_name", "group_name",
      "group_level", "strata_name", "strata_level",
      "variable_name", "variable_level", "estimate_name",
      "estimate_type", "estimate_value", "additional_name",
      "additional_level"
    )
  )


  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: checks on working example", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
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
  expect_gte(nrow(inc), 1)

  # reconnect
  cdmReconn <- CDMConnector::cdmFromCon(
    con = attr(attr(cdm, "cdm_source"), "dbcon"),
    cohortTables = c("denominator", "outcome"),
    writeSchema = "main", cdmSchema = "main", cdmName = "mock"
  )
  inc_recon <- estimateIncidence(
    cdm = cdmReconn,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "overall")
  )
  expect_identical(inc, inc_recon)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check working example 2", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc |>
    dplyr::filter(
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value"))) == 1)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 2,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc |>
    dplyr::filter(
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value"))) == 3)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 10,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc |>
    dplyr::filter(
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value"))) == 2)

  # even if repeatedEvents = TRUE,
  # if outcomeWashout=NULL (all of history)
  # then it won<U+00B4>t be possible to have any recurrent events
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc |>
    dplyr::filter(
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value"))) == 1)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    interval = "weeks",
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc |>
    dplyr::filter(
      estimate_name == "outcome_count"
    ) |>
    dplyr::pull("estimate_value"))) == 1)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check study periods", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-15"),
    observation_period_end_date = as.Date("2010-12-15")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  # we expect 12 months of which the last in december
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 12)

  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    interval = "months",
    repeatedEvents = TRUE,
    completeDatabaseIntervals = TRUE
  )

  # now with completeDatabaseIntervals is TRUE
  # we expect 10 months of which the last in november
  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 10)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check overall", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2005-01-15"),
      as.Date("2005-01-15")
    ),
    observation_period_end_date = c(
      as.Date("2007-05-01"),
      as.Date("2011-06-15")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1L, 2L, 2L),
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  # we expect one row with the overall results
  # with two people
  # one person had the event before the study period
  # (but washout was 0 so was included)
  # one person had the event during the study period
  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 1)
  expect_true(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "2")

  expect_true(all(inc |>
    omopgenerics::filterSettings(result_type == "incidence") |>
    omopgenerics::splitAdditional() |>
    dplyr::pull("incidence_start_date") == as.Date("2007-01-01")))
  expect_true(all(inc |>
    omopgenerics::filterSettings(result_type == "incidence") |>
    omopgenerics::splitAdditional() |>
    dplyr::pull("incidence_end_date") == as.Date("2010-02-05"))) # date of first event


  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(inc |>
    omopgenerics::filterSettings(result_type == "incidence") |>
    dplyr::filter(estimate_name == "outcome_count")) == 1)
  expect_true(all(inc |>
    omopgenerics::filterSettings(result_type == "incidence") |>
    omopgenerics::splitAdditional() |>
    dplyr::pull("incidence_start_date") == as.Date("2007-01-01")))
  expect_true(all(inc |>
    omopgenerics::filterSettings(result_type == "incidence") |>
    omopgenerics::splitAdditional() |>
    dplyr::pull("incidence_end_date") == as.Date("2011-06-15"))) # date of end of obs

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check person days", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(2000L, 1999L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2007-01-01"),
      as.Date("2007-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-12-31"),
      as.Date("2022-10-05")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2021-06-27")),
    cohort_end_date = c(as.Date("2021-06-27"))
  )


  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  # in 2019 we expect person 2 to contribute from 1st july to end of December
  expect_true(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    head(1) |>
    dplyr::pull("estimate_value") ==
    as.numeric(difftime(
      as.Date("2019-12-31"),
      as.Date("2019-07-01")
    )) + 1)

  # in 2020 we expect person 2 to contribute all year
  # and person 1 from 1st January to end of December
  expect_true(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value") ==
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
  expect_true(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 3) |>
    dplyr::pull("estimate_value") ==
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
  expect_true(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 4) |>
    dplyr::pull("estimate_value") ==
    (as.numeric(difftime(
      as.Date("2021-10-05"),
      as.Date("2021-01-01")
    )) + 1))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check periods follow calendar dates", {
  skip_on_cran()
  # check that even if startDate as during a period
  # periods still follow calendar dates
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value") == "3")

  # startDate during a month (with month as interval)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2011-01-15"), as.Date(NA))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    interval = c("months"),
    completeDatabaseIntervals = FALSE
  )
  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value") == "1")
  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::filter(dplyr::row_number() == 3) |>
    dplyr::pull("estimate_value") == "1")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check washout windows", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  # expect all events if we have zero days washout
  expect_true(sum(as.numeric(incW0 |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == 4)

  incW1 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 1L,
    completeDatabaseIntervals = FALSE
  )
  # expect three events if we have one days washout
  expect_true(sum(as.numeric(incW1 |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == 3)

  incW2 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 2,
    completeDatabaseIntervals = FALSE
  )
  # expect two events if we have two days washout
  expect_true(sum(as.numeric(incW2 |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == 2)

  incW365 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )
  # expect one event if we have 365 days washout
  expect_true(sum(as.numeric(incW365 |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == 1)

  incInf <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    completeDatabaseIntervals = FALSE
  )
  # expect one event if we have NULL (all history washout)
  expect_true(sum(as.numeric(incInf |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == 1)

  # but, we will have move days when using the 365 day washout
  # as the person came back to contribute more time at risk
  expect_lt(sum(as.numeric(incInf |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incW365 |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))))

  omopgenerics::cdmDisconnect(cdm)


  # never satisfy criteria in study period
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2009-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2009-12-31")
    ),
    cohort_end_date = c(
      as.Date("2010-06-02")
    )
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(c("2010-01-01", NA))
  )

  incW365 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 36500,
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(incW365 |>
    omopgenerics::filterSettings(result_type == "incidence")) == 0)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check events overlapping with start of a period", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(2000L, 1999L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2000-01-21"),
      as.Date("2007-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-12-31"),
      as.Date("2022-12-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2020-06-27")),
    cohort_end_date = c(as.Date("2020-07-19"))
  )


  cdm <- mockIncidencePrevalence(
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
    interval = c("Years")
  )

  expect_true(all(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == 1))
  omopgenerics::cdmDisconnect(cdm)

  # another example
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(2000L, 1999L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2000-01-21"),
      as.Date("2007-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-12-31"),
      as.Date("2022-12-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 1L),
    cohort_start_date = c(as.Date("2020-06-27"), as.Date("2020-07-30")),
    cohort_end_date = c(as.Date("2020-07-19"), as.Date("2020-08-20"))
  )

  cdm <- mockIncidencePrevalence(
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
    interval = c("Years")
  )
  expect_true(all(inc2 |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == 1))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: compare results from months and years", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = rep(8507L, 2),
    year_of_birth = rep(2000L, 2),
    month_of_birth = rep(01L, 2),
    day_of_birth = rep(01L, 2)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2012-01-01"),
      as.Date("2012-01-01")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2011-07-01")
    ),
    cohort_end_date = c(
      as.Date("2011-07-01")
    )
  )
  cdm <- mockIncidencePrevalence(
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
    interval = c("months")
  )
  incYears <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years")
  )

  # consistent results for months and years
  expect_true(sum(as.numeric(incMonths |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) ==
    sum(as.numeric(incYears |>
      dplyr::filter(estimate_name == "outcome_count") |>
      dplyr::pull("estimate_value"))))

  expect_true(sum(as.numeric(incMonths |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))) ==
    sum(as.numeric(incYears |>
      dplyr::filter(estimate_name == "person_days") |>
      dplyr::pull("estimate_value"))))

  expect_equal(
    sum(as.numeric(incMonths |>
      dplyr::filter(estimate_name == "person_years") |>
      dplyr::pull("estimate_value"))),
    sum(as.numeric(incYears |>
      dplyr::filter(estimate_name == "person_years") |>
      dplyr::pull("estimate_value")))
  )

  omopgenerics::cdmDisconnect(cdm)


  cdm <- mockIncidencePrevalence(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2010-01-01"), as.Date("2011-12-31"))
  )

  incWeeks <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("weeks"),
    completeDatabaseIntervals = FALSE
  )
  incQuarters <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("quarters"),
    completeDatabaseIntervals = FALSE
  )
  incMonths <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("months"),
    completeDatabaseIntervals = FALSE
  )
  incYears <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years"),
    completeDatabaseIntervals = FALSE
  )

  # consistent results for months and years
  expect_identical(sum(as.numeric(incWeeks |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incYears |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))))
  expect_identical(sum(as.numeric(incQuarters |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incYears |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))))
  expect_identical(sum(as.numeric(incMonths |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incYears |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))))

  expect_identical(sum(as.numeric(incWeeks |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incYears |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))))
  expect_identical(sum(as.numeric(incQuarters |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incYears |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))))
  expect_identical(sum(as.numeric(incMonths |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))), sum(as.numeric(incYears |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull("estimate_value"))))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: multiple denominator inputs", {
  cdm <- mockIncidencePrevalence(sampleSize = 6000,
                                 maxOutcomeDays = 5,
                                 maxOutcomes = 6)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator_1",
    cohortDateRange = c(as.Date("2010-01-01"),
                        as.Date("2020-12-31")),
    ageGroup = list(c(25, 50)),
    sex = "Both")
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator_2",
    cohortDateRange = c(as.Date("2010-01-01"),
                        as.Date("2020-12-31")),
    ageGroup = list(c(0, 50),
                    c(25, 30),
                    c(25, 50),
                    c(25, 80),
                    c(45, 100)),
    sex =  c("Both", "Male", "Female"))

  inc_1 <- estimateIncidence(cdm,
                             denominatorTable = "denominator_1",
                             outcomeTable = "outcome")
  inc_2 <- estimateIncidence(cdm,
                             denominatorTable = "denominator_2",
                             outcomeTable = "outcome")

  expect_identical(
    tableIncidence(inc_1,
                   type = "tibble"),
    tableIncidence(inc_2 |>
                     omopgenerics::filterSettings(denominator_age_group == "25 to 50",
                                                  denominator_sex == "Both"),
                   type = "tibble")
  )

  omopgenerics::cdmDisconnect(cdm)

})

test_that("mock db: check entry and event on same day", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-28"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2010-01-28")
    ),
    cohort_end_date = c(
      as.Date("2010-01-28")
    )
  )

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  expect_true(sum(as.numeric(incWithoutRep |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == "1")

  incWithRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = Inf,
    interval = "years",
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(incWithRep |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == "1")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: cohort start overlaps with the outcome", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2019-05-09"),
      as.Date("2019-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2022-05-19"),
      as.Date("2021-12-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2019-05-09")),
    cohort_end_date = c(as.Date("2022-05-19"))
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = as.Date(c(
      "2021-01-01",
      "2021-12-31"
    ))
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    repeatedEvents = TRUE,
    interval = c("Years")
  )
  expect_true(all(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "1"))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check outcome in previous obeservation period", {
  skip_on_cran()
  # 1) with outcome starting and ending before observation period start
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L, 1L, 2L),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2010-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-12-31"),
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 1L),
    cohort_start_date = as.Date(c("2000-01-01", "2018-01-01")),
    cohort_end_date = as.Date(c("2005-12-31", "2019-01-01"))
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2011-01-01"), as.Date("2020-01-01"))
  )

  # with rep events - should have both people
  incRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0,
    repeatedEvents = TRUE,
    interval = c("Years")
  )
  expect_true(all(incRep |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "2"))

  # with inf wash out- should only have 1 person
  incNoRep <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = TRUE,
    interval = c("Years")
  )
  expect_true(all(incNoRep |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "1"))

  # with 5 year wash out- should have 2 people at the start of the study period
  incNoRep2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 1825,
    repeatedEvents = TRUE,
    interval = c("Years")
  )

  expect_true(max(as.numeric(incNoRep2 |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value"))) == 2)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check minimum counts", {
  skip_on_cran()
  # 20 people
  personTable <- dplyr::tibble(
    person_id = c(1:20),
    gender_concept_id = rep(8507L, 20),
    year_of_birth = rep(2000L, 20),
    month_of_birth = rep(01L, 20),
    day_of_birth = rep(01L, 20)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1:20),
    person_id = c(1:20),
    observation_period_start_date = rep(as.Date("2000-01-01"), 20),
    observation_period_end_date = rep(as.Date("2012-06-01"), 20)
  )
  outcomeTable <-
    dplyr::bind_rows(
      # 17 in first period
      dplyr::tibble(
        cohort_definition_id = as.integer(rep(1, 17)),
        subject_id = as.integer(c(1:17)),
        cohort_start_date = rep(
          as.Date("2000-01-02"), 17
        ),
        cohort_end_date = rep(
          as.Date("2000-01-03"), 17
        )
      ),
      # three in second
      dplyr::tibble(
        cohort_definition_id = as.integer(rep(1, 3)),
        subject_id = as.integer(c(18:20)),
        cohort_start_date = rep(
          as.Date("2000-02-02"), 3
        ),
        cohort_end_date = rep(
          as.Date("2000-02-03"), 3
        )
      )
    )

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  expect_identical(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value"), c("20", "3"))
  expect_identical(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"), c("17", "3"))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "person_years") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "person_years") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value")))

  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys_95CI_lower") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys_95CI_lower") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys_95CI_upper") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value")))
  expect_false(is.na(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys_95CI_upper") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value")))


  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    repeatedEvents = FALSE,
    completeDatabaseIntervals = FALSE
  ) |> omopgenerics::suppress(5)
  expect_identical(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value"), c("20", "-"))
  expect_identical(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"), c("17", "-"))
  expect_false(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value") == "-")
  expect_true(inc |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value") == "-")
  expect_false(inc |>
    dplyr::filter(estimate_name == "person_years") |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("estimate_value") == "-")
  expect_true(inc |>
    dplyr::filter(estimate_name == "person_years") |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("estimate_value") == "-")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: multiple overlapping outcomes", {
  # technically overlapping outcomes are not allowed
  # check this edge case, but validation might also not allow this

  skip_on_cran()
  # two people
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2020-04-29"),
      as.Date("2019-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2021-12-31")
    )
  )
  # two outcomes for person one
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 1L),
    cohort_start_date = c(as.Date("2020-04-29"), as.Date("2020-11-10")),
    cohort_end_date = c(as.Date("2020-05-17"), as.Date("2020-12-17"))
  )

  cdm <- mockIncidencePrevalence(
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
    interval = "overall"
  )

  expect_true(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "2")
  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value") == "1")
  omopgenerics::cdmDisconnect(cdm)

  # three
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2020-04-29"),
      as.Date("2019-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2021-12-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L),
    subject_id = c(1L, 1L, 1L),
    cohort_start_date = c(
      as.Date("2020-04-29"),
      as.Date("2020-11-08"),
      as.Date("2020-11-10")
    ),
    cohort_end_date = c(
      as.Date("2020-05-17"),
      as.Date("2020-11-09"),
      as.Date("2020-12-17")
    )
  )

  cdm <- mockIncidencePrevalence(
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
    interval = "overall"
  )
  expect_true(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "2")
  expect_true(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value") == "1")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: cohort before period start ending after period", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1990L, 1990L),
    month_of_birth = c(01L, 01L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2000-07-31"),
      as.Date("2000-07-31")
    ),
    observation_period_end_date = c(
      as.Date("2020-01-01"),
      as.Date("2010-01-01")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 2L),
    cohort_start_date = c(
      as.Date("2000-08-02"),
      as.Date("2001-06-01")
    ),
    cohort_end_date = c(
      as.Date("2020-01-01"),
      as.Date("2001-07-01")
    )
  )

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  expect_true(all(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value") == "1"))

  # washout
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = Inf,
    repeatedEvents = FALSE,
    interval = c("Years"),
    completeDatabaseIntervals = FALSE
  )
  expect_true(all(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value") == "1"))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check full period requirement - year", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2020-03-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-06-06"),
      as.Date("2021-06-06")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2020-05-28")),
    cohort_end_date = c(as.Date("2020-05-29"))
  )

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = TRUE,
    interval = c("Years")
  )
  expect_true(nrow(inc |>
    omopgenerics::filterSettings(result_type == "incidence")) == 0)
  omopgenerics::cdmDisconnect(cdm)

  # edge case first day to last of the year
  # still expect this to work
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2020-05-09"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-12-31"),
      as.Date("2020-12-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2020-05-29")),
    cohort_end_date = c(as.Date("2020-05-29"))
  )

  cdm <- mockIncidencePrevalence(
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
    interval = c("Years")
  )

  expect_true(nrow(inc |>
    omopgenerics::filterSettings(result_type == "incidence") |>
    dplyr::filter(estimate_name == "outcome_count")) ==
    1)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check full period requirement - month", {
  skip_on_cran()
  # expected to work
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2020-04-28"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-06-06"),
      as.Date("2020-06-06")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalence(
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
    interval = c("Months")
  )
  expect_gte(nrow(inc), 1)
  omopgenerics::cdmDisconnect(cdm)

  # edge case first day to last of the month
  # still expect this to work
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2020-04-28"),
      as.Date("2020-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2020-04-29"),
      as.Date("2020-01-31")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-04-28"))
  )

  cdm <- mockIncidencePrevalence(
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
    interval = c("Months")
  )
  expect_gte(nrow(inc |>
    omopgenerics::filterSettings(result_type == "incidence")), 1)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check completeDatabaseIntervals", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2019-05-09"),
      as.Date("2019-02-02")
    ),
    observation_period_end_date = c(
      as.Date("2022-06-01"),
      as.Date("2021-06-06")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2020-04-28")),
    cohort_end_date = c(as.Date("2020-05-19"))
  )

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = TRUE
  )

  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 2)
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2020")
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2021")

  # repetitive events FALSE
  # - now we expect only to use 2020 (id 2 obs end is in 21)
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = FALSE,
    completeDatabaseIntervals = TRUE
  )
  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 1)
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2020")

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
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 4)
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2019")
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2020")
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 3) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2021")
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 4) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2022")

  # repetitive events FALSE
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = FALSE,
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(inc |>
    dplyr::filter(estimate_name == "outcome_count")) == 3)
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 1) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2019")
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 2) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2020")
  expect_true(clock::get_year(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(dplyr::row_number() == 3) |>
    dplyr::pull("incidence_start_date") |>
    as.Date()) == "2021")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check insufficient study days", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = c(1995L, 1995L),
    month_of_birth = c(07L, 07L),
    day_of_birth = c(01L, 01L)
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2019-05-09"),
      as.Date("2019-02-02")
    ),
    observation_period_end_date = c(
      as.Date("2019-06-01"),
      as.Date("2019-06-06")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(as.Date("2019-06-01")),
    cohort_end_date = c(as.Date("2019-06-01"))
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )

  # we have less than a year of follow up
  # so we should return an empty tibble if full periods are required
  # and we<U+00B4>re looking for yearly incidence
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("Years"),
    repeatedEvents = TRUE,
    completeDatabaseIntervals = TRUE
  )

  expect_true(nrow(inc |>
    omopgenerics::filterSettings(result_type == "incidence")) == 0)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check with and without study start and end date", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L, 3L, 4L, 5L, 6L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 07L,
    day_of_birth = 01L
  )
  # one person leaving before 2010
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L, 4L, 5L, 6L),
    person_id = c(1L, 2L, 3L, 4L, 5L, 6L),
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
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 1L,
      cohort_start_date = c(as.Date("2007-01-01")),
      cohort_end_date = c(as.Date("2022-12-31"))
    ),
    # 2 multiple events
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 2L,
      cohort_start_date = c(as.Date("2008-06-01")),
      cohort_end_date = c(as.Date("2008-10-01"))
    ),
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 2L,
      cohort_start_date = c(as.Date("2008-11-01")),
      cohort_end_date = c(as.Date("2010-10-14"))
    ),
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 2L,
      cohort_start_date = c(as.Date("2010-12-01")),
      cohort_end_date = c(as.Date("2011-06-18"))
    ),
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 2L,
      cohort_start_date = c(as.Date("2011-06-19")),
      cohort_end_date = c(as.Date("2012-12-31"))
    ),
    # 3 multiple events into the period
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 3L,
      cohort_start_date = c(as.Date("2009-06-01")),
      cohort_end_date = c(as.Date("2010-02-01"))
    ),
    dplyr::tibble(
      cohort_definition_id = 1L,
      subject_id = 3L,
      cohort_start_date = c(as.Date("2010-06-01")),
      cohort_end_date = c(as.Date("2022-12-31"))
    )
  )


  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  inc2A <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE
  )

  # given the settings above we would expect the same results for 2010
  expect_identical(inc1A |>
    dplyr::filter(estimate_name == "denominator_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"), inc2A |>
    dplyr::filter(estimate_name == "denominator_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"))

  expect_identical(inc1A |>
    dplyr::filter(estimate_name == "person_days") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"), inc2A |>
    dplyr::filter(estimate_name == "person_days") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"))

  expect_identical(inc1A |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"), inc2A |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"))

  # 365 washout, repetitive events
  inc1B <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )
  inc2B <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )
  # given the settings above we would expect the same results for 2010
  expect_identical(inc1B |>
    dplyr::filter(estimate_name == "denominator_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"), inc2B |>
    dplyr::filter(estimate_name == "denominator_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"))

  expect_identical(inc1B |>
    dplyr::filter(estimate_name == "person_days") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"), inc2B |>
    dplyr::filter(estimate_name == "person_days") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"))

  expect_identical(inc1B |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"), inc2B |>
    dplyr::filter(estimate_name == "outcome_count") |>
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) |>
    dplyr::pull("estimate_value"))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check study start and end date 1000", {
  skip_on_cran()
  # with one outcome per person
  cdm <- mockIncidencePrevalence(
    sampleSize = 1000
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
    completeDatabaseIntervals = FALSE
  )
  inc2A <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc1A |>
    omopgenerics::filterSettings(result_type == "incidence") %>%
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) %>%
    dplyr::filter(estimate_name == "denominator_count") %>%
    dplyr::pull("estimate_value") ==
    inc2A |>
      omopgenerics::filterSettings(result_type == "incidence") %>%
      omopgenerics::splitAdditional() |>
      dplyr::filter(clock::get_year(incidence_start_date |>
        as.Date()) == 2010) %>%
      dplyr::filter(estimate_name == "denominator_count") %>%
      dplyr::pull("estimate_value"))
  expect_true(inc1A |>
    omopgenerics::filterSettings(result_type == "incidence") %>%
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) %>%
    dplyr::filter(estimate_name == "person_days") %>%
    dplyr::pull("estimate_value") ==
    inc2A |>
      omopgenerics::filterSettings(result_type == "incidence") %>%
      omopgenerics::splitAdditional() |>
      dplyr::filter(clock::get_year(incidence_start_date |>
        as.Date()) == 2010) %>%
      dplyr::filter(estimate_name == "person_days") %>%
      dplyr::pull("estimate_value"))
  expect_true(inc1A |>
    omopgenerics::filterSettings(result_type == "incidence") %>%
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value") ==
    inc2A |>
      omopgenerics::filterSettings(result_type == "incidence") %>%
      omopgenerics::splitAdditional() |>
      dplyr::filter(clock::get_year(incidence_start_date |>
        as.Date()) == 2010) %>%
      dplyr::filter(estimate_name == "outcome_count") %>%
      dplyr::pull("estimate_value"))

  # 365 washout, repetitive events
  inc1B <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )
  inc2B <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )

  expect_true(inc1B |>
    omopgenerics::filterSettings(result_type == "incidence") %>%
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) %>%
    dplyr::filter(estimate_name == "incidence_100000_pys") %>%
    dplyr::pull("estimate_value") ==
    inc2B |>
      omopgenerics::filterSettings(result_type == "incidence") %>%
      omopgenerics::splitAdditional() |>
      dplyr::filter(clock::get_year(incidence_start_date |>
        as.Date()) == 2010) %>%
      dplyr::filter(estimate_name == "incidence_100000_pys") %>%
      dplyr::pull("estimate_value"))


  omopgenerics::cdmDisconnect(cdm)

  # with multiple outcomes per person
  cdm <- mockIncidencePrevalence(
    sampleSize = 1000
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
    completeDatabaseIntervals = FALSE
  )
  inc2A <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 0,
    completeDatabaseIntervals = FALSE
  )


  expect_true(inc1A |>
    omopgenerics::filterSettings(result_type == "incidence") %>%
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) %>%
    dplyr::filter(estimate_name == "incidence_100000_pys") %>%
    dplyr::pull("estimate_value") ==
    inc2A |>
      omopgenerics::filterSettings(result_type == "incidence") %>%
      omopgenerics::splitAdditional() |>
      dplyr::filter(clock::get_year(incidence_start_date |>
        as.Date()) == 2010) %>%
      dplyr::filter(estimate_name == "incidence_100000_pys") %>%
      dplyr::pull("estimate_value"))

  # 365 washout, repetitive events
  inc1B <- estimateIncidence(cdm,
    denominatorTable = "denominator1",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )
  inc2B <- estimateIncidence(cdm,
    denominatorTable = "denominator2",
    outcomeTable = "outcome",
    interval = "years",
    repeatedEvents = TRUE,
    outcomeWashout = 365,
    completeDatabaseIntervals = FALSE
  )


  expect_true(inc1B |>
    omopgenerics::filterSettings(result_type == "incidence") %>%
    omopgenerics::splitAdditional() |>
    dplyr::filter(clock::get_year(incidence_start_date |>
      as.Date()) == 2010) %>%
    dplyr::filter(estimate_name == "incidence_100000_pys") %>%
    dplyr::pull("estimate_value") ==
    inc2B |>
      omopgenerics::filterSettings(result_type == "incidence") %>%
      omopgenerics::splitAdditional() |>
      dplyr::filter(clock::get_year(incidence_start_date |>
        as.Date()) == 2010) %>%
      dplyr::filter(estimate_name == "incidence_100000_pys") %>%
      dplyr::pull("estimate_value"))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("expected errors with mock", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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
  cdm <- mockIncidencePrevalence(
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
    outcomeCohortId = 1
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

  # cohortId wrong format
  expect_error(estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = TRUE
  ))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: multiple observation periods", {
  skip_on_cran()
  # create data for hypothetical people to test
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8507L),
    year_of_birth = c(1998L, 1976L),
    month_of_birth = c(02L, 06L),
    day_of_birth = c(12L, 01L)
  )

  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L, 1L, 2L),
    observation_period_start_date = c(
      as.Date("2005-04-01"),
      as.Date("2009-04-10"),
      as.Date("2010-12-11")
    ),
    observation_period_end_date = c(
      as.Date("2005-11-29"),
      as.Date("2016-01-02"),
      as.Date("2015-06-01")
    )
  )

  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 2L),
    cohort_start_date = c(
      as.Date("2005-07-19"),
      as.Date("2010-12-11")
    ),
    cohort_end_date = c(
      as.Date("2005-07-19"),
      as.Date("2015-06-01")
    )
  )

  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L),
    subject_id = c(1L, 1L, 2L, 2L),
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
  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  # expect all events if we have zero days washout
  expect_true(sum(as.numeric(incW0 %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 2)
  omopgenerics::cdmDisconnect(cdm)

  # Change the inclusion so that both patients have valid observation periods. Now 1 should have two, and 2 one.
  # Should capture the final part of the first observation period, and the initial part of the second for person 1
  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L),
    subject_id = c(1L, 1L, 2L),
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  # expect all events if we have ten days washout
  expect_true(sum(as.numeric(incW10 %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 3)
  omopgenerics::cdmDisconnect(cdm)

  # try event not counted for outcome but counted for washout as denominator (before observ period)
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 1L),
    subject_id = c(1L, 1L, 1L, 2L, 2L),
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
  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc_PreWashout %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 3)
  expect_true(sum(as.numeric(inc_PreWashout %>%
    dplyr::filter(estimate_name == "person_days") %>%
    dplyr::pull("estimate_value"))) ==
    as.numeric(difftime(as.Date("2005-08-11"), as.Date("2005-07-19"))) +
      1 - 2 + as.numeric(difftime(
        as.Date("2015-01-02"),
        as.Date("2009-04-10")
      )) + 1 - 10 +
      as.numeric(difftime(as.Date("2011-12-11"), as.Date("2010-12-11"))) +
      1 - 10 - 3)
  omopgenerics::cdmDisconnect(cdm)

  # multiple events in one of the observation periods of person 1
  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L),
    subject_id = c(1L, 1L, 2L),
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  inc_Mult1_W30 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = TRUE,
    outcomeWashout = 30,
    completeDatabaseIntervals = FALSE
  )

  # we should have 4 events with washout 0, but 3 events with washout 30
  expect_true(sum(as.numeric(inc_Mult1_W0 %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 4)
  expect_true(sum(as.numeric(inc_Mult1_W30 %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 3)
  expect_true(sum(as.numeric(inc_Mult1_W0 %>%
    dplyr::filter(estimate_name == "person_days") %>%
    dplyr::pull("estimate_value"))) ==
    as.numeric(difftime(
      as.Date("2005-08-11"),
      as.Date("2005-06-19")
    )) + 1 +
      as.numeric(difftime(
        as.Date("2015-01-02"),
        as.Date("2009-04-10")
      )) + 1 +
      as.numeric(difftime(
        as.Date("2011-12-11"),
        as.Date("2010-12-11")
      )) + 1)
  expect_true(sum(as.numeric(inc_Mult1_W30 %>%
    dplyr::filter(estimate_name == "person_days") %>%
    dplyr::pull("estimate_value"))) ==
    as.numeric(difftime(
      as.Date("2005-08-11"),
      as.Date("2005-06-19")
    )) - 30 +
      as.numeric(difftime(
        as.Date("2015-01-02"),
        as.Date("2009-04-10")
      )) +
      1 - 30 + as.numeric(difftime(
        as.Date("2011-12-11"),
        as.Date("2010-12-11")
      )) + 1 - 30)
  omopgenerics::cdmDisconnect(cdm)

  # The first event of person 1 will not be included in the observation period
  # but should also influence the second event with the washout
  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L),
    subject_id = c(1L, 1L, 2L),
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

  cdm <- mockIncidencePrevalence(
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
    completeDatabaseIntervals = FALSE
  )

  # we should have 2 events with washout 30
  expect_true(sum(as.numeric(inc_PreWashEv %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 2)

  expect_true(sum(as.numeric(inc_PreWashEv %>%
    dplyr::filter(estimate_name == "person_days") %>%
    dplyr::pull("estimate_value"))) ==
    as.numeric(difftime(
      as.Date("2005-08-11"),
      as.Date("2005-07-19")
    )) - 30 + 7 +
      as.numeric(difftime(
        as.Date("2015-01-02"),
        as.Date("2009-04-10")
      )) + 1 - 30 +
      as.numeric(difftime(
        as.Date("2011-12-11"),
        as.Date("2010-12-11")
      )) + 1 - 30)
  omopgenerics::cdmDisconnect(cdm)

  # three observation periods for 1 person and a
  # couple of consecutive events lost to washout
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L, 4L),
    person_id = c(1L, 1L, 1L, 2L),
    observation_period_start_date = c(
      as.Date("2005-04-01"),
      as.Date("2009-04-10"),
      as.Date("2010-08-20"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-11-29"),
      as.Date("2010-01-02"),
      as.Date("2011-12-11"),
      as.Date("2015-06-01")
    )
  )

  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L),
    subject_id = c(1L, 1L, 1L, 2L),
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

  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L, 1L, 1L, 1L, 1L),
    subject_id = c(1L, 1L, 1L, 1L, 1L, 1L, 2L),
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

  cdm <- mockIncidencePrevalence(
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
    outcomeWashout = 1L,
    completeDatabaseIntervals = FALSE
  )

  # we should have 5 events with washout 1
  expect_true(sum(as.numeric(inc_3op %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 5)

  # try repeated events FALSE.
  inc_repev <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    repeatedEvents = FALSE,
    outcomeWashout = 1L,
    completeDatabaseIntervals = FALSE
  )
  expect_true(sum(as.numeric(inc_repev %>%
    dplyr::filter(estimate_name == "outcome_count") %>%
    dplyr::pull("estimate_value"))) == 2)

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check confidence intervals", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(
    sampleSize = 1000
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
    completeDatabaseIntervals = TRUE
  )

  pkg_est <- inc %>%
    omopgenerics::filterSettings(result_type == "incidence") |>
    dplyr::select(
      "estimate_name",
      "estimate_value", "additional_level"
    ) |>
    tidyr::pivot_wider(
      names_from = "estimate_name",
      values_from = "estimate_value"
    ) |>
    dplyr::filter(denominator_count > 1)

  expect_equal(as.numeric(pkg_est$incidence_100000_pys_95CI_lower),
    epitools::pois.exact(
      as.numeric(pkg_est$outcome_count),
      as.numeric(pkg_est$person_years)
    )$lower * 100000,
    tolerance = 1e-2
  )
  expect_equal(as.numeric(pkg_est$incidence_100000_pys_95CI_upper),
    epitools::pois.exact(
      as.numeric(pkg_est$outcome_count),
      as.numeric(pkg_est$person_years)
    )$upper * 100000,
    tolerance = 1e-2
  )

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check attrition", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(sampleSize = 1000)
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
  expect_gt(nrow(inc |>
    omopgenerics::filterSettings(
      result_type == "incidence_attrition",
      denominator_sex == "Female"
    ) |>
    dplyr::filter(strata_level == "Not Female")), 0)


  # for male, the opposite
  expect_gt(nrow(inc |>
    omopgenerics::filterSettings(
      result_type == "incidence_attrition",
      denominator_sex == "Male"
    ) |>
    dplyr::filter(strata_level == "Not Male")), 0)

  omopgenerics::cdmDisconnect(cdm)

  # check obscuring counts
  cdm <- mockIncidencePrevalence(sampleSize = 4)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = c("Male", "Female")
  )
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  ) |> omopgenerics::suppress(5)
  expect_true(inc |>
    omopgenerics::filterSettings(
      result_type == "incidence_attrition",
      denominator_sex == "Male"
    ) |>
    dplyr::filter(strata_level == "Not Male") |>
    dplyr::filter(variable_name == "excluded_subjects") |>
    dplyr::pull("estimate_value") == "-")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check attrition with complete database intervals", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2000-06-01"),
      as.Date("2000-06-01")
    ),
    observation_period_end_date = c(
      as.Date("2011-07-01"),
      as.Date("2000-07-01")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  )

  expect_true(inc |>
    omopgenerics::filterSettings(result_type == "incidence_attrition") |>
    dplyr::filter(strata_level == "Not observed during the complete database interval") |>
    dplyr::filter(variable_name == "excluded_subjects") |>
    dplyr::pull("estimate_value") == "1")

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check compute permanent", {
  skip_on_cran()

  # using temp
  cdm <- mockIncidencePrevalence(sampleSize = 1000)
  attr(cdm, "write_schema") <- "main"

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  )
  # we have no temp tables created by dbplyr
  expect_false(any(stringr::str_starts(
    CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon")),
    "dbplyr_"
  )))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: if missing cohort attributes", {
  skip_on_cran()
  # missing cohort_set
  cdm <- mockIncidencePrevalence()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  attr(cdm$outcome, "cohort_set") <- NULL
  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  ))
  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: empty outcome cohort", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(sampleSize = 200)

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  cdm$outcome <- cdm$outcome %>% dplyr::filter(cohort_definition_id == 99)

  expect_no_error(inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  ))
  expect_true(sum(as.numeric(inc |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull("estimate_value"))) == 0)

  # make sure we also have a confidence interval even in the case of an empty outcome cohort
  expect_true(all(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys") |>
    dplyr::pull("estimate_value") == "0"))
  expect_true(all(as.numeric(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys_95CI_lower") |>
    dplyr::pull("estimate_value")) == 0))
  expect_true(all(as.numeric(inc |>
    dplyr::filter(estimate_name == "incidence_100000_pys_95CI_upper") |>
    dplyr::pull("estimate_value")) > 0))

  tidyInc <- inc |>
    asIncidenceResult()

  # compare our wilson CIs with those from epitools
  epi_ci <- epitools::binom.wilson(
    as.numeric(tidyInc$outcome_count),
    as.numeric(tidyInc$denominator_count)
  )

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: incidence using strata vars", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(
    sampleSize = 2000L,
    outPre = 0.2
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    cohortDateRange = as.Date(
      c("1993-01-01", "1998-01-01")
    )
  )

  inc_orig <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995L,
      "first", "second"
    )) %>%
    dplyr::compute(
      name = "denominator",
      temporary = FALSE
    )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  ) |>
    omopgenerics::filterSettings(result_type == "incidence")
  expect_true(all(c("overall", "my_strata") %in% unique(inc$strata_name)))
  expect_true(all(c("overall", "first", "second") %in% unique(inc$strata_level)))

  org <- inc_orig |>
    omopgenerics::filterSettings(result_type == "incidence")
  attr(org, "settings") <- NULL
  attr(inc, "settings") <- NULL
  # original without strata should be the same as "Overall" strata
  expect_identical(org, inc |> dplyr::filter(strata_level == "overall"))

  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata2 = dplyr::if_else(month(cohort_start_date) < 7,
      "a", "b"
    )) %>%
    dplyr::compute(
      name = "denominator",
      temporary = FALSE
    )
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata", "my_strata2"))
  )

  expect_true(all(c("overall", "my_strata &&& my_strata2") %in%
    unique(inc2$strata_name)))
  expect_true(all(c(
    "overall",
    "first &&& a",
    "second &&& a",
    "first &&& b",
    "second &&& b"
  ) %in%
    unique(inc2$strata_level)))

  inc3 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(
      c("my_strata"),
      c("my_strata2"),
      c("my_strata", "my_strata2")
    )
  )

  expect_true(all(c(
    "overall",
    "my_strata",
    "my_strata2",
    "my_strata &&& my_strata2"
  ) %in%
    unique(inc3$strata_name)))
  expect_true(all(c(
    "overall",
    "first", "second",
    "a", "b",
    "first &&& a",
    "second &&& a",
    "first &&& b",
    "second &&& b"
  ) %in%
    unique(inc3$strata_level)))

  # without overall strata
  inc4 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(
      c("my_strata"),
      c("my_strata2"),
      c("my_strata", "my_strata2")
    ),
    includeOverallStrata = FALSE
  )
  expect_true(all(c(
    "my_strata",
    "my_strata2",
    "my_strata &&& my_strata2"
  ) %in%
    unique(inc4$strata_name)))
  expect_true(all(c(
    "first", "second",
    "a", "b",
    "first &&& a",
    "second &&& a",
    "first &&& b",
    "second &&& b"
  ) %in%
    unique(inc4$strata_level)))

  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("not_a_col"))
  ))

  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata", "not_a_col"))
  ))

  expect_error(estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"), c("not_a_col"))
  ))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: multiple outcome cohort id", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2005-01-15"),
      as.Date("2005-01-15")
    ),
    observation_period_end_date = c(
      as.Date("2007-05-01"),
      as.Date("2011-06-15")
    )
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 2L),
    subject_id = c(1L, 2L, 2L),
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

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )

  inc_all_outcome <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = c(1L, 2L),
    interval = "overall"
  )
  inc_all_outcome_1 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 1L,
    interval = "overall"
  )
  inc_all_outcome_2 <- estimateIncidence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 2,
    interval = "overall"
  )

  expect_identical(inc_all_outcome %>%
    omopgenerics::filterGroup(outcome_cohort_name == "cohort_1") |>
    dplyr::pull("estimate_value"), inc_all_outcome_1 |>
    dplyr::pull("estimate_value"))


  expect_identical(inc_all_outcome %>%
    omopgenerics::filterGroup(outcome_cohort_name == "cohort_2") |>
    dplyr::pull("estimate_value"), inc_all_outcome_2 |>
    dplyr::pull("estimate_value"))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: cohort names for cohortId args", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-28"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2010-01-28")
    ),
    cohort_end_date = c(
      as.Date("2010-01-28")
    )
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  inc1 <- estimateIncidence(cdm, "target", "outcome")
  inc2 <- estimateIncidence(cdm, denominatorTable = "target", outcomeTable = "outcome",
                            denominatorCohortId = 1, outcomeCohortId = 1)
  inc3 <-  estimateIncidence(cdm, denominatorTable = "target", outcomeTable = "outcome",
                             denominatorCohortId = "cohort_1", outcomeCohortId = "cohort_1")

  expect_true(all.equal(inc1, inc2))
  expect_true(all.equal(inc2, inc3))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: empty denominator", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-28"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2010-01-28")
    ),
    cohort_end_date = c(
      as.Date("2010-01-28")
    )
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  attr(cdm$target, "cohort_set") <- dplyr::union_all(
    attr(cdm$target, "cohort_set"),
    dplyr::tibble(
      cohort_definition_id = 2,
      cohort_name = "cohort_2"
    ),
    copy = TRUE
  )

  expect_error(estimateIncidence(cdm, "target", "outcome", 2))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("mock db: check local cdm", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  cdm <- cdm |> dplyr::collect()

  expect_no_error(inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "overall")
  ))
})

test_that("mock db: censor cohort", {
  skip_on_cran()

  # if appearing in censor cohort before denominator, the person should be
  # excluded
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 1990L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("1990-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
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
  censorCohort <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("1995-01-01")
    ),
    cohort_end_date = c(
      as.Date("1995-01-01")
    )
  )
  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable,
    censorTable = censorCohort
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm,
                                      name = "denominator",
                                      cohortDateRange = c(as.Date("2000-01-01"),
                                                          as.Date(NA)))
  # using censor cohort should mean person excluded before start
  inc <- estimateIncidence(cdm,
                    denominatorTable = "denominator",
                    outcomeTable = "outcome",
                    censorTable = "censor")
  expect_true(all(inc |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull("estimate_value") == "0"))



  # if appearing in censor cohort after denominator but before outcome, they
  # should contribute time at risk until censor and then no more time and no
  # outcome

  # censor cohort after 1st outcome
  # so should be excluded from then (if analysis is allowing repetitive events)
  censorCohort2 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2009-01-01")
    ),
    cohort_end_date = c(
      as.Date("2009-01-01")
    )
  )
  cdm <- omopgenerics::insertTable(cdm, "my_censor_cohort_2", censorCohort2)
  cdm$my_censor_cohort_2 <- omopgenerics::newCohortTable(cdm$my_censor_cohort_2)

  inc_2 <- estimateIncidence(cdm,
                             interval = "overall",
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            censorTable = "my_censor_cohort_2",
                            outcomeWashout = 0,
                            repeatedEvents = TRUE)
  expect_true(all(sum(as.numeric(inc_2 |>
                    dplyr::filter(estimate_name == "outcome_count") |>
                    dplyr::pull("estimate_value"))) == 1))

  # if appearing in the censor cohort on the day of the outcome, they
  # contribute the outcome but are censored from then on
  censorCohort3 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2008-02-05")
    ),
    cohort_end_date = c(
      as.Date("2008-02-05")
    )
  )
  cdm <- omopgenerics::insertTable(cdm, "my_censor_cohort_3", censorCohort3)
  cdm$my_censor_cohort_3 <- omopgenerics::newCohortTable(cdm$my_censor_cohort_3)

  inc_3 <- estimateIncidence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             censorTable = "my_censor_cohort_3",
                             outcomeWashout = 0,
                             repeatedEvents = TRUE)
  expect_true(all(sum(as.numeric(inc_2 |>
                                   dplyr::filter(estimate_name == "outcome_count") |>
                                   dplyr::pull("estimate_value"))) == 1))


  # multiple denominators - censor cohort should apply with each

  # check censor cohort id argument
  # if multiple cohorts in censor cohort table, one must be chosen
  censorCohort5 <- dplyr::tibble(
    cohort_definition_id = c(1L, 2L),
    subject_id = c(1L,1L),
    cohort_start_date = c(
      as.Date("2008-02-05"),
      as.Date("2009-02-05")
    ),
    cohort_end_date = c(
      as.Date("2008-02-05"),
      as.Date("2009-02-05")
    )
  )
  cdm <- omopgenerics::insertTable(cdm, "my_censor_cohort_5", censorCohort5)
  cdm$my_censor_cohort_5 <- omopgenerics::newCohortTable(cdm$my_censor_cohort_5)
  # need to specify one cohort if multiple exist
  expect_no_error(estimateIncidence(cdm,
                    denominatorTable = "denominator",
                    outcomeTable = "outcome",
                    censorTable = "my_censor_cohort_5",
                    censorCohortId = "cohort_1")) # can use name
  expect_error(estimateIncidence(cdm,
                                 denominatorTable = "denominator",
                                 outcomeTable = "outcome",
                                 censorTable = "my_censor_cohort_5",
                                 censorCohortId = c(1,2)))
  expect_error(estimateIncidence(cdm,
                                 denominatorTable = "denominator",
                                 outcomeTable = "outcome",
                                 censorTable = "my_censor_cohort_5",
                                 censorCohortId = c(1,2)))
  expect_error(estimateIncidence(cdm,
                                 denominatorTable = "denominator",
                                 outcomeTable = "outcome",
                                 censorTable = "my_censor_cohort_5",
                                 censorCohortId = NULL))


  # censor cohort must be one record per person
  censorCohort6 <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("1995-01-01"),
      as.Date("1998-01-01")
    ),
    cohort_end_date = c(
      as.Date("1995-01-01"),
      as.Date("1998-01-01")
    )
  )
  cdm <- omopgenerics::insertTable(cdm, "my_censor_cohort_6", censorCohort6)
  cdm$my_censor_cohort_6 <- omopgenerics::newCohortTable(cdm$my_censor_cohort_6)
  expect_error(estimateIncidence(cdm,
                    denominatorTable = "denominator",
                    outcomeTable = "outcome",
                    censorTable = "my_censor_cohort_6"))

  # validate input
  expect_error(estimateIncidence(cdm,
                    denominatorTable = "denominator",
                    outcomeTable = "outcome",
                    censorTable = "not_a_table",
                    outcomeWashout = 0,
                    repeatedEvents = TRUE))
 expect_error(estimateIncidence(cdm,
                    denominatorTable = "denominator",
                    outcomeTable = "outcome",
                    censorTable = "my_censor_cohort_3",
                    censorCohortId = 99)) # not in cohort table

 # multiple observation periods
 # should look back
 personTable <- dplyr::tibble(
   person_id = 1L,
   gender_concept_id = 8507L,
   year_of_birth = 1990L,
   month_of_birth = 01L,
   day_of_birth = 01L
 )
 observationPeriodTable <- dplyr::tibble(
   observation_period_id = c(1L, 2L),
   person_id = c(1L, 1L),
   observation_period_start_date = c(as.Date("2000-01-01"),
                                     as.Date("2002-01-01")),
   observation_period_end_date = c(as.Date("2001-06-01"),
                                   as.Date("2003-06-01"))

 )
 outcomeTable <- dplyr::tibble(
   cohort_definition_id = 1L,
   subject_id = 1L,
   cohort_start_date = c(
     as.Date("2002-02-05")
   ),
   cohort_end_date = c(
     as.Date("2002-02-05")
   )
 )
 censorCohort <- dplyr::tibble(
   cohort_definition_id = 1L,
   subject_id = 1L,
   cohort_start_date = c(
     as.Date("2000-04-01")
   ),
   cohort_end_date = c(
     as.Date("2000-04-01")
   )
 )
 cdm <- mockIncidencePrevalence(
   personTable = personTable,
   observationPeriodTable = observationPeriodTable,
   outcomeTable = outcomeTable,
   censorTable = censorCohort
 )

 cdm <- generateDenominatorCohortSet(cdm = cdm,
                                     name = "denominator")
 inc_1 <- estimateIncidence(cdm,
                          denominatorTable = "denominator",
                          outcomeTable = "outcome",
                          completeDatabaseIntervals = FALSE)
 expect_true(all(inc_1 |>
                   dplyr::filter(estimate_name == "denominator_count") |>
                   dplyr::pull("estimate_value") == "1"))
 expect_true(any(inc_1 |>
                   dplyr::filter(estimate_name == "outcome_count") |>
                   dplyr::pull("estimate_value") == "1"))



 # using censor cohort should mean person excluded before start
 inc_2 <- estimateIncidence(cdm,
                          denominatorTable = "denominator",
                          outcomeTable = "outcome",
                          censorTable = "censor",
                          completeDatabaseIntervals = FALSE)
 expect_true(inc_2 |>
                   dplyr::filter(estimate_name == "outcome_count") |>
                   dplyr::pull("estimate_value") == "0")
})

test_that("rateDenominator works correctly", {
  skip_on_cran()

  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2011-01-01"),
    cohort_end_date = as.Date("2011-01-01")
  )

  cdm <- mockIncidencePrevalence(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  # Default (100000)
  inc1 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  )

  expect_true(any(grepl("incidence_100000_pys", inc1$estimate_name)))

  # Custom (1000)
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall",
    rateDenominator = 1000
  )

  expect_true(any(grepl("incidence_1000_pys", inc2$estimate_name)))
  expect_false(any(grepl("incidence_100000_pys", inc2$estimate_name)))

  # Check scaling
  val1 <- inc1 %>%
    dplyr::filter(estimate_name == "incidence_100000_pys") %>%
    dplyr::pull(estimate_value) %>%
    as.numeric()

  val2 <- inc2 %>%
    dplyr::filter(estimate_name == "incidence_1000_pys") %>%
    dplyr::pull(estimate_value) %>%
    as.numeric()

  if (length(val1) > 0 && length(val2) > 0) {
    expect_equal(val1, val2 * 100, tolerance = 0.5)
  }

  # Check tableIncidence output
  expect_no_error(tbl <- tableIncidence(inc2))
  expect_s3_class(tbl, "gt_tbl")

  # Clean up
  omopgenerics::cdmDisconnect(cdm)
})

