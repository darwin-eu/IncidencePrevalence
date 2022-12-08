
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
  )

  # check estimates tibble
  expect_true(all(c(
    "analysis_id",
    "n_cases",
    "n_population",
    "prevalence",
    "prevalence_95CI_lower",
    "prevalence_95CI_upper",
    "prevalence_start_date",
    "prevalence_end_date",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(prev)))

  # check analysis settings tibble::tibble
  expect_true(all(c(
    "outcome_cohort_id",
    "analysis_id",
    "analysis_type",
    "analysis_time_point",
    "analysis_interval",
    "analysis_full_contribution",
    "analysis_complete_database_intervals",
    "analysis_min_cell_count",
    "denominator_cohort_id",
    "denominator_age_group",
    "denominator_min_age",
    "denominator_max_age",
    "denominator_sex",
    "denominator_days_prior_history",
    "denominator_start_date",
    "denominator_end_date"
  ) %in%
    names(settings(prev))))

  # check we can get the reference to participants who contributed
  expect_true(is.list(participants(prev))) # list of references to participants
  expect_true(tibble::is_tibble(participants(prev,1) %>%
                                  dplyr::collect()))
  expect_true(participants(prev,1) %>%
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

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    minCellCount = 0
  )
  expect_true(nrow(prev) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: working examples 2", {
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
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )

  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = "months"
  )
  expect_true(nrow(prev) >= 1)

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "point",
                            interval = "days"
  )
  expect_true(nrow(prev) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check outcome lookback", {
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
      as.Date("2008-02-05")
    ),
    cohort_end_date = c(
      as.Date("2008-02-05")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm,
                                       startDate = as.Date("2006-01-01"),
                                       endDate = as.Date("2010-12-31"))

  # without look back we´ll only include ongoing outcomes
  # of which none are at the start of a year
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = "years",
    minCellCount = 0
  )
  expect_true(all(prev$n_cases == 0))

  # with a lookback of 365 days
  # the person would be considered as a prevalent case at the start of 2009
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeLookbackDays = 365,
    type = "point",
    interval = "years",
    minCellCount = 0
  )
  expect_true((prev %>%
                 dplyr::filter(lubridate::year(prevalence_start_date)=="2008") %>%
                 dplyr::select(n_cases) %>%
                 dplyr::pull() == 0))
  expect_true((prev %>%
                 dplyr::filter(lubridate::year(prevalence_start_date)=="2009") %>%
                 dplyr::select(n_cases) %>%
                 dplyr::pull() == 1))
  expect_true((prev %>%
                 dplyr::filter(lubridate::year(prevalence_start_date)=="2010") %>%
                 dplyr::select(n_cases) %>%
                 dplyr::pull() == 0))

  # with a NULL lookback
  # where any prior outcome is used
  # the person would be considered as a prevalent case at the start of 2009 and 2010
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeLookbackDays = NULL,
    type = "point",
    interval = "years",
    minCellCount = 0
  )
  expect_true((prev %>%
                 dplyr::filter(lubridate::year(prevalence_start_date)=="2008") %>%
                 dplyr::select(n_cases) %>%
                 dplyr::pull() == 0))
  expect_true((prev %>%
                 dplyr::filter(lubridate::year(prevalence_start_date)=="2009") %>%
                 dplyr::select(n_cases) %>%
                 dplyr::pull() == 1))
  expect_true((prev %>%
                 dplyr::filter(lubridate::year(prevalence_start_date)=="2010") %>%
                 dplyr::select(n_cases) %>%
                 dplyr::pull() == 1))

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
  observationPeriodTable <- dplyr::bind_rows(
    tibble::tibble(
      observation_period_id = as.character(c(1:17)),
      person_id = as.character(c(1:17)),
      observation_period_start_date = rep(as.Date("2000-01-01"), 17),
      observation_period_end_date = rep(as.Date("2000-01-31"), 17)
    ),
    tibble::tibble(
      observation_period_id = as.character(c(18:20)),
      person_id = as.character(c(18:20)),
      observation_period_start_date = rep(as.Date("2000-01-01"), 3),
      observation_period_end_date = rep(as.Date("2012-06-01"), 3)
    )
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

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    minCellCount = 0,
    type = "period",
    interval = "months"
  )
  expect_true(prev$n_cases[1] == 17)
  expect_true(prev$n_cases[2] == 3)
  expect_true(prev$n_cases[3] == 0)
  expect_true(prev$n_population[1] == 20)
  expect_true(prev$n_population[2] == 3)
  expect_true(prev$n_population[3] == 3)
  expect_true(!is.na(prev$prevalence[1]))
  expect_true(!is.na(prev$prevalence[2]))
  expect_true(!is.na(prev$prevalence[3]))
  expect_true(!is.na(prev$prevalence_95CI_lower[1]))
  expect_true(!is.na(prev$prevalence_95CI_upper[1]))

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    minCellCount = 5,
    type = "period",
    interval = "months"
  )
  expect_true(prev$n_cases[1] == 17)
  expect_true(is.na(prev$n_cases[2]))
  expect_true(is.na(prev$n_cases[3]))
  expect_true(prev$n_population[1] == 20)
  expect_true(is.na(prev$n_population[2]))
  expect_true(is.na(prev$n_population[3]))
  expect_true(!is.na(prev$prevalence[1]))
  expect_true(is.na(prev$prevalence[2]))
  expect_true(is.na(prev$prevalence[3]))
  expect_true(!is.na(prev$prevalence_95CI_lower[1]))
  expect_true(is.na(prev$prevalence_95CI_lower[2]))
  expect_true(is.na(prev$prevalence_95CI_lower[3]))
  expect_true(!is.na(prev$prevalence_95CI_upper[1]))
  expect_true(is.na(prev$prevalence_95CI_upper[2]))
  expect_true(is.na(prev$prevalence_95CI_upper[3]))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check study time periods", {
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
    observation_period_end_date = as.Date("2010-12-31")
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

  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = "months",
    verbose = FALSE
  )

  # we expect 12 months of which the last in December
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(nrow(prev) == 12)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check fullContribution requirement", {
  personTable <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id =  c("1","2"),
    person_id =  c("1","2"),
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = c(as.Date("2011-06-01"),
                                    as.Date("2012-06-01"))
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
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = "years",
                            fullContribution = FALSE,
                            minCellCount = 0
  )
  expect_true(all(prev[["denominator"]] == 2))

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = "years",
                            fullContribution = TRUE,
                            minCellCount = 0
  )
  expect_true(all(prev[["denominator"]] == c(2,1)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check periods follow calendar dates", {
  # check that even if study_start_date is during a period
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
    observation_period_start_date = as.Date("2010-06-05"),
    observation_period_end_date = as.Date("2013-06-15")
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

  # if completeDatabaseIntervals is TRUE we should go from 2010 to 2013
  # but if FALSE we should go from 2011 to 2012
  # for yearly incidence
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )
  prev1 <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "years",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(prev1) == 4)
  expect_true(all(lubridate::year(prev1$prevalence_start_date) ==
                    c("2010", "2011", "2012", "2013")))

  prev2 <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "years",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = TRUE
  )
  expect_true(nrow(prev2) == 2)
  expect_true(all(lubridate::year(prev2$prevalence_start_date)==
                    c("2011", "2012")))

  # for months
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2011-01-15")
  )

  # where we expect the study to start on 2011-01-15
  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "months",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = FALSE
  )
  expect_true(prev$prevalence_start_date[1] ==
                as.Date("2011-01-15"))
  # where we expect the study to start the next month
  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "months",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = TRUE
  )
  expect_true(prev$prevalence_start_date[1] ==
    as.Date("2011-02-01"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check multiple outcome ids", {
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
    observation_period_start_date = as.Date("2011-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1","2"), # two different outcome ids
    subject_id = c("1","2"),
    cohort_start_date = c(
      as.Date("2011-02-05")
    ),
    cohort_end_date = c(
      as.Date("2011-02-05")
    )
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = "years",
                            minCellCount = 0
  )
  expect_true(all(prev[["n_cases"]] == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: some empty result sets", {
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
    observation_period_start_date = as.Date("2012-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1","2"), # two different outcome ids
    subject_id = c("1","2"),
    cohort_start_date = c(
      as.Date("2011-02-05")
    ),
    cohort_end_date = c(
      as.Date("2011-02-05")
    )
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = "years",
                            minCellCount = 0
  )
  expect_true(nrow(prev) == 0)

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = c("months","years"),
                            minCellCount = 0
  )
  expect_true(nrow(prev) > 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check conversion of user inputs", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 1,
    denominatorCohortId = 1,
  )
  expect_true(nrow(prev) >= 1)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check messages when vebose is true", {
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

  cdm <- mockIncidencePrevalenceRef(outcomeTable = outcomeTable)

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  expect_message(estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    verbose = TRUE
  ))

  expect_message(estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    verbose = TRUE
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check expected errors", {
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

  expect_error(estimatePrevalence(
    cdm = "a",
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 1,
    denominatorCohortId = 1
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check user point prevalence function", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  prev_point <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  expect_true(all(names(prev)==names(prev_point)))
  expect_true(all(names(settings(prev))==
                    names(settings(prev_point))))
  expect_true(all(names(prev)==
                    names(prev_point)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check user period prevalence function", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    type="period",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  expect_true(all(names(prev)==names(prev_period)))
  expect_true(all(names(settings(prev))==
                    names(settings(prev_period))))
  expect_true(all(names(prev)==
                    names(prev_period)))

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

  # should expect for period prevalence monthly 3 times with n_cases 1, and denominator 1 only at inclusion criteria satisfaction
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0
  )
  expect_true(sum(ppe$n_cases) == 3)
  expect_true(sum(ppe$n_population) == 8+8+14)

  # same if we look back 1 day, as some repeated events at month 8 disappear but the person still has an outcome then
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0,
    outcomeLookbackDays = 1
  )
  expect_true(sum(ppe$n_cases) == 3)

  # if we look back 365 days, all outcomes count monthly for a whole year after their onset, so we should see 4+3+14
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0,
    outcomeLookbackDays = 365
  )
  expect_true(sum(ppe$n_cases) == 21)

  # as for point prevalence, we would expect no positive n_cases at default
  ppo <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0
  )
  expect_true(sum(ppo$n_cases) == 0)

  # we would expect 4 n_cases == 1 at daily calculation
  ppo <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "days",
    minCellCount = 0
  )
  expect_true(sum(ppo$n_cases) == 6)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})



test_that("mock db: check confidence intervals", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "point",
                             interval = "years",
                             minCellCount = 0
  )

  # compare our wilson CIs with those from Hmisc
  hmisc_ci <-  Hmisc::binconf(prev$n_cases, prev$n_population,
                              alpha=0.05,
                              method=c("wilson"),
                              return.df=TRUE)
  expect_equal(prev$prevalence_95CI_lower, hmisc_ci$Lower,
               tolerance = 1e-2)
  expect_equal(prev$prevalence_95CI_upper, hmisc_ci$Upper,
               tolerance = 1e-2)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
