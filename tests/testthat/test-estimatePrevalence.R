
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

  expect_true(class(prev) == "list")
  expect_true(all(names(prev) %in%
    c(
      "prevalence_estimates",
      "analysis_settings",
      "person_table",
      "attrition"
    )))

  # check analysis settings tibble::tibble
  expect_true(all(c(
    "prevalence_analysis_id",
    "type",
    "point",
    "interval",
    "full_contribution",
    "full_periods",
    "outcome_cohort_id",
    "denominator_cohort_id",
    "confidence_interval",
    "min_cell_count"
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
  expect_true(nrow(prev[["prevalence_estimates"]]) >= 1)

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
  expect_true(nrow(prev[["prevalence_estimates"]]) >= 1)

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "point",
                            interval = "days"
  )
  expect_true(nrow(prev[["prevalence_estimates"]]) >= 1)

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
  expect_true(all(prev[["prevalence_estimates"]]$numerator == 0))

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
  expect_true((prev[["prevalence_estimates"]] %>%
                 dplyr::filter(time=="2008") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 0))
  expect_true((prev[["prevalence_estimates"]] %>%
                 dplyr::filter(time=="2009") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 1))
  expect_true((prev[["prevalence_estimates"]] %>%
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
  expect_true((prev[["prevalence_estimates"]] %>%
                 dplyr::filter(time=="2008") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 0))
  expect_true((prev[["prevalence_estimates"]] %>%
                 dplyr::filter(time=="2009") %>%
                 dplyr::select(numerator) %>%
                 dplyr::pull() == 1))
  expect_true((prev[["prevalence_estimates"]] %>%
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
  expect_true(!is.na(prev[["prevalence_estimates"]]$prev_high[1]))

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
  expect_true(nrow(prev[["prevalence_estimates"]]) == 12)

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
  expect_true(all(prev$prevalence_estimates[["denominator"]] == 2))

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = "years",
                            fullContribution = TRUE,
                            minCellCount = 0
  )
  expect_true(all(prev$prevalence_estimates[["denominator"]] == c(2,1)))


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

  # if fullPeriods is TRUE we should go from 2010 to 2013
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
    fullPeriods = FALSE
  )
  expect_true(nrow(prev1$prevalence_estimates) == 4)
  expect_true(all(prev1$prevalence_estimates$time ==
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
    fullPeriods = TRUE
  )
  expect_true(nrow(prev2$prevalence_estimates) == 2)
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
    fullPeriods = FALSE
  )
  expect_true(prev[["prevalence_estimates"]]$start_time[1] ==
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
    fullPeriods = TRUE
  )
  expect_true(prev[["prevalence_estimates"]]$start_time[1] ==
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
  expect_true(all(prev$prevalence_estimates[["numerator"]] == 1))

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
  expect_true(nrow(prev$prevalence_estimates) == 0)

  prev <- estimatePrevalence(cdm,
                            denominatorTable = "denominator",
                            outcomeTable = "outcome",
                            type = "period",
                            interval = c("months","years"),
                            minCellCount = 0
  )
  expect_true(nrow(prev$prevalence_estimates) > 0)

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
  expect_true(nrow(prev[["prevalence_estimates"]]) >= 1)


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

  expect_true(class(prev) == class(prev_point))
  expect_true(all(names(prev)==names(prev_point)))
  expect_true(all(names(prev[["analysis_settings"]])==
                    names(prev_point[["analysis_settings"]])))
  expect_true(all(names(prev[["prevalence_estimates"]])==
                    names(prev_point[["prevalence_estimates"]])))

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

  expect_true(class(prev) == class(prev_period))
  expect_true(all(names(prev)==names(prev_period)))
  expect_true(all(names(prev[["analysis_settings"]])==
                    names(prev_period[["analysis_settings"]])))
  expect_true(all(names(prev[["prevalence_estimates"]])==
                    names(prev_period[["prevalence_estimates"]])))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
