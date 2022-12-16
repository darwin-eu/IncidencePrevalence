
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    confidenceInterval = "none"
  )

  # check estimates tibble
  expect_true(all(c(
    "analysis_id",
    "time",
    "numerator", "denominator",
    "prev",
    "prev_low",
    "prev_high",
    "start_time", "end_time",
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
    "analysis_confidence_interval",
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
    outcomeCohortId = "1",
    denominatorCohortId = "1",
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
  expect_true(all(prev$numerator == 0))

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
                 dplyr::filter(time=="2008") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 0))
  expect_true((prev %>%
                 dplyr::filter(time=="2009") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 1))
  expect_true((prev %>%
                 dplyr::filter(time=="2010") %>%
                 dplyr::select(numerator) %>%
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
                 dplyr::filter(time=="2008") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 0))
  expect_true((prev %>%
                 dplyr::filter(time=="2009") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 1))
  expect_true((prev %>%
                 dplyr::filter(time=="2010") %>%
                 dplyr::select(numerator) %>%
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
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    minCellCount = 0,
    type = "period",
    interval = "months",
    confidenceInterval = "binomial"
  )
  expect_true(prev$numerator[1] == 17)
  expect_true(prev$numerator[2] == 3)
  expect_true(prev$numerator[3] == 0)
  expect_true(prev$denominator[1] == 20)
  expect_true(prev$denominator[2] == 3)
  expect_true(prev$denominator[3] == 3)
  expect_true(!is.na(prev$prev[1]))
  expect_true(!is.na(prev$prev[2]))
  expect_true(!is.na(prev$prev[3]))
  expect_true(!is.na(prev$prev_low[1]))
  expect_true(!is.na(prev$prev_high[1]))

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    minCellCount = 5,
    type = "period",
    interval = "months",
    confidenceInterval = "binomial"
  )
  expect_true(prev$numerator[1] == 17)
  expect_true(is.na(prev$numerator[2]))
  expect_true(is.na(prev$numerator[3]))
  expect_true(prev$denominator[1] == 20)
  expect_true(is.na(prev$denominator[2]))
  expect_true(is.na(prev$denominator[3]))
  expect_true(!is.na(prev$prev[1]))
  expect_true(is.na(prev$prev[2]))
  expect_true(is.na(prev$prev[3]))
  expect_true(!is.na(prev$prev_low[1]))
  expect_true(is.na(prev$prev_low[2]))
  expect_true(is.na(prev$prev_low[3]))
  expect_true(!is.na(prev$prev_high[1]))
  expect_true(is.na(prev$prev_high[2]))
  expect_true(is.na(prev$prev_high[3]))

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
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    type = "period",
    interval = "years",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(prev1) == 4)
  expect_true(all(prev1$time ==
                    c("2010", "2011", "2012", "2013")))

  prev2 <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    type = "period",
    interval = "years",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = TRUE
  )
  expect_true(nrow(prev2) == 2)
  expect_true(all(prev2$time == c("2011", "2012")))

  # for months
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2011-01-15")
  )

  # where we expect the study to start on 2011-01-15
  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    type = "period",
    interval = "months",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = FALSE
  )
  expect_true(prev$start_time[1] ==
                as.Date("2011-01-15"))
  # where we expect the study to start the next month
  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    type = "period",
    interval = "months",
    minCellCount = 0,
    fullContribution = FALSE,
    completeDatabaseIntervals = TRUE
  )
  expect_true(prev$start_time[1] ==
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
  expect_true(all(prev[["numerator"]] == 1))

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
    outcomeCohortId = "1",
    denominatorCohortId = "1",
    type = "point",
    verbose = TRUE
  ))

  expect_message(estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    denominatorCohortId = "1",
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
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    confidenceInterval = "none"
  )
  prev_point <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    confidenceInterval = "none"
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
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    confidenceInterval = "none"
  )
  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    denominatorCohortId = "1",
    outcomeTable = "outcome",
    outcomeCohortId = "1",
    confidenceInterval = "none"
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

  # should expect for period prevalence monthly 3 times with numerator 1, and denominator 1 only at inclusion criteria satisfaction
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0
  )
  expect_true(sum(ppe$numerator) == 3)
  expect_true(sum(ppe$denominator) == 8+8+14)

  # same if we look back 1 day, as some repeated events at month 8 disappear but the person still has an outcome then
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0,
    outcomeLookbackDays = 1
  )
  expect_true(sum(ppe$numerator) == 3)

  # if we look back 365 days, all outcomes count monthly for a whole year after their onset, so we should see 4+3+14
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0,
    outcomeLookbackDays = 365
  )
  expect_true(sum(ppe$numerator) == 21)

  # as for point prevalence, we would expect no positive numerator at default
  ppo <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0
  )
  expect_true(sum(ppo$numerator) == 0)

  # we would expect 4 numerator == 1 at daily calculation
  ppo <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "days",
    minCellCount = 0
  )
  expect_true(sum(ppo$numerator) == 6)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: multiple denominators and outcomes, lookback and time point arguments", {

  # create data for hypothetical people to test
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
    gender_concept_id = c("8507","8507","8507","8507","8507","8507","8507","8532","8532","8532","8532"),
    year_of_birth = c(1943,1955,1956,1958,1976,1987,1989,1954,1968,1968,1991),
    month_of_birth = c(02,06,01,03,12,11,10,10,09,04,04),
    day_of_birth = c(07,06,23,10,13,24,05,17,19,05,23)
  )

  # one observation period per person.
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
    person_id = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"),
    observation_period_start_date = c(
      as.Date("2005-04-02"),
      as.Date("2009-04-10"),
      as.Date("1990-08-20"),
      as.Date("1977-04-01"),
      as.Date("2001-04-10"),
      as.Date("2000-08-20"),
      as.Date("2002-04-01"),
      as.Date("1972-04-10"),
      as.Date("1967-08-20"),
      as.Date("2001-04-01"),
      as.Date("2009-11-01")
    ),
    observation_period_end_date = c(
      as.Date("2006-04-01"),
      as.Date("2020-04-10"),
      as.Date("2021-08-20"),
      as.Date("2021-04-01"),
      as.Date("2021-04-10"),
      as.Date("2018-08-20"),
      as.Date("2011-04-01"),
      as.Date("2013-04-10"),
      as.Date("2019-08-20"),
      as.Date("2022-04-01"),
      as.Date("2022-01-01")
    )
  )

  # all outcomes in 2010 but two. Also two types.
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c("1","1","1","1","1","1","1","1","2","2","2","2","2"),
    subject_id = c("1","2","2","5","6","9","10","11","4","7","8","10","11"),
    cohort_start_date = c(
      as.Date("2005-07-01"),
      as.Date("2010-05-10"),
      as.Date("2010-01-20"),
      as.Date("2010-11-06"),
      as.Date("2010-12-11"),
      as.Date("2010-07-12"),
      as.Date("2010-03-03"),
      as.Date("2010-03-22"),
      as.Date("2010-06-17"),
      as.Date("2010-06-05"),
      as.Date("2010-09-02"),
      as.Date("2010-10-26"),
      as.Date("2009-04-01")
    ),
    cohort_end_date = c(
      as.Date("2005-07-01"),
      as.Date("2010-05-10"),
      as.Date("2010-01-20"),
      as.Date("2010-11-06"),
      as.Date("2010-12-11"),
      as.Date("2010-07-12"),
      as.Date("2010-03-03"),
      as.Date("2010-03-22"),
      as.Date("2010-06-17"),
      as.Date("2010-06-05"),
      as.Date("2010-09-02"),
      as.Date("2010-10-26"),
      as.Date("2009-04-01")

    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    outcomeTable = outcomeTable
  )

  # 8 (2**3) denominator groups.
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-01-01"),
    endDate = as.Date("2010-12-31"),
    ageGroups = list(
      c(0, 50),
      c(51, 100)
    ),
    sex = c("Male", "Female"),
    daysPriorHistory = c(0, 365)
  )

  ppe_fullC <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    fullContribution = TRUE,
    minCellCount = 0
  )
  expect_true(sum(ppe_fullC$numerator) == 18)
  expect_true(all(ppe_fullC$prev == c(2/3,1/3,1/3,1/2,1/3,1/2,1,1,2/3,1,1,1/3,0,0,0,1/3))) # the order of the analysis is 1 -> 10 -> 11 -> ... -> 16 -> 2 -> ... -> 9

  ppe_nofullC <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    minCellCount = 0
  )
  expect_true(sum(ppe_nofullC$numerator) == 19)
  expect_true(all(ppe_nofullC$prev == c(2/3,1/3,1/3,1/3,1/3,1/3,1,1,2/3,1,2/3,1/3,1/3,0,0,1/3))) # the order of the analysis is 1 -> 10 -> 11 -> ... -> 16 -> 2 -> ... -> 9

  ppe_fullC_m <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    fullContribution = TRUE,
    minCellCount = 0
  )
  expect_true(sum(ppe_fullC_m$numerator) == 20)
  expect_true(ppe_fullC_m$numerator[111] == 2)
  expect_true(ppe_fullC_m$numerator[115] == 1)
  expect_true(ppe_fullC_m$denominator[148] == 2)

  ppe_nofullC_m <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    fullContribution = FALSE,
    minCellCount = 0
  )
  expect_true(sum(ppe_nofullC_m$numerator) == 20)
  expect_true(ppe_nofullC_m$numerator[111] == 2)
  expect_true(ppe_nofullC_m$numerator[115] == 1)
  expect_true(ppe_nofullC_m$denominator[148] == 3)


  # do point prevalence too
  ppo_start <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    outcomeLookbackDays = c(0,30),
    timePoint = "start",
    minCellCount = 0
  )
  expect_true(sum(ppo_start$numerator) == 0) # no events, either with look back or not

  pop_middle <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    outcomeLookbackDays = c(0,30),
    timePoint = "middle",
    minCellCount = 0
  )
  expect_true(sum(pop_middle$numerator) == 4) # mid point is 2010-07-01 so look back 30 days will show two events. As the two people have enough past data, they both are in two cohorts.

  pop_end <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    outcomeLookbackDays = c(0,30),
    timePoint = "end",
    minCellCount = 0
  )
  expect_true(sum(pop_end$numerator) == 2) # only sees one event (both for 0 or 365 days of previous observation)

  ppo_start_m <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeLookbackDays = c(0,30),
    timePoint = "start",
    minCellCount = 0
  )
  expect_true(sum(ppo_start_m$numerator) == 18) # no events without look back and all events with look back 30 (except from the one in December)

  ppo_start_m_lb10 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeLookbackDays = 10,
    timePoint = "start",
    minCellCount = 0
  )
  expect_true(sum(ppo_start_m_lb10$numerator) == 3) # two events with look back 10, one of them from a person not contributing when we ask for 365d of previous obvs

  ppo_start_m_lb12 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeLookbackDays = 12,
    timePoint = "start",
    minCellCount = 0
  )
  expect_true(sum(ppo_start_m_lb12$numerator) == 4) # one event at day 2010-01-20 added, from a person only contirbuting when we don't ask for previous obvs

  ppo_middle_m_lb10 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeLookbackDays = 10,
    timePoint = "middle",
    minCellCount = 0
  )
  expect_true(sum(ppo_middle_m_lb10$numerator) == 10) # five events with look back 10 (days of the month 06 to 15)

  ppo_middle_m_lb9 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeLookbackDays = 9,
    timePoint = "middle",
    minCellCount = 0
  )
  expect_true(sum(ppo_middle_m_lb9$numerator) == 8) # lost the event at 2010-01-06

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
