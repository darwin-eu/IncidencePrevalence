
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
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
    "analysis_id",
    "outcome_cohort_id",
    "outcome_cohort_name",
    "analysis_type",
    "analysis_time_point",
    "analysis_interval",
    "analysis_full_contribution",
    "analysis_complete_database_intervals",
    "analysis_min_cell_count",
    "denominator_cohort_id",
    "denominator_age_group",
    "denominator_sex",
    "denominator_days_prior_history",
    "denominator_start_date",
    "denominator_end_date",
    "cdm_name"
  ) %in%
    names(prevalenceSet(prev))))

  expect_true(all(c(
    "analysis_id", "number_records", "number_subjects",
    "reason_id","reason",
    "excluded_records", "excluded_subjects"
  ) %in%
    names(prevalenceAttrition(prev))))

  # by default we don´t return the participants
  expect_true(is.null(participants(prev, analysisId = 1)))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)


  cdm <- mockIncidencePrevalenceRef()
  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    tablePrefix = "result",
    returnParticipants = TRUE
  )
  # now we do return the participants
  expect_true(is.list(participants(prev, 1))) # list of references to participants
  expect_true(tibble::is_tibble(participants(prev, 1) %>%
    dplyr::collect()))
  expect_true(participants(prev, 1) %>%
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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("months", "years"),
    minCellCount = 0
  )
  expect_true(nrow(prev) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: working examples 2", {
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
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )

  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = c("months", "years")
  )
  expect_true(nrow(prev) >= 1)

  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = c("months", "years")
  )
  expect_true(nrow(prev) >= 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check outcome lookback", {
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
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = 1,
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

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2006-01-01"),
    endDate = as.Date("2010-12-31")
  )

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
    dplyr::filter(lubridate::year(prevalence_start_date) == "2008") %>%
    dplyr::select(n_cases) %>%
    dplyr::pull() == 0))
  expect_true((prev %>%
    dplyr::filter(lubridate::year(prevalence_start_date) == "2009") %>%
    dplyr::select(n_cases) %>%
    dplyr::pull() == 1))
  expect_true((prev %>%
    dplyr::filter(lubridate::year(prevalence_start_date) == "2010") %>%
    dplyr::select(n_cases) %>%
    dplyr::pull() == 0))

  # with a NULL lookback
  # where any prior outcome is used
  # the person would be a prevalent case at the start of 2009 and 2010
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
    dplyr::filter(lubridate::year(prevalence_start_date) == "2008") %>%
    dplyr::select(n_cases) %>%
    dplyr::pull() == 0))
  expect_true((prev %>%
    dplyr::filter(lubridate::year(prevalence_start_date) == "2009") %>%
    dplyr::select(n_cases) %>%
    dplyr::pull() == 1))
  expect_true((prev %>%
    dplyr::filter(lubridate::year(prevalence_start_date) == "2010") %>%
    dplyr::select(n_cases) %>%
    dplyr::pull() == 1))

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
        cohort_definition_id = rep(1, 17),
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
        cohort_definition_id = rep(1, 3),
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
    observation_period_end_date = as.Date("2010-12-31")
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


  # overall period
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "overall",
                             verbose = FALSE
  )
  # just one row
  expect_true(nrow(prev) == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # should return empty if no study days
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
    observation_period_end_date = as.Date("2010-11-15")
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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             fullContribution = TRUE,
                             interval = "weeks",
                             verbose = FALSE
  )
  expect_true(nrow(prev) == 45)

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             fullContribution = TRUE,
                             interval = "months",
                             verbose = FALSE
  )
  expect_true(nrow(prev) == 10)

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             fullContribution = TRUE,
                             interval = "years",
                             verbose = FALSE
  )
  expect_true(nrow(prev) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: check fullContribution requirement", {
  skip_on_cran()
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2010-01-01"),
      as.Date("2012-04-01")
    ),
    observation_period_end_date = c(
      as.Date("2011-06-01"),
      as.Date("2012-06-01"),
      as.Date("2012-06-01")
    )
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
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )

  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "years",
    fullContribution = FALSE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )
  expect_true(all(prev$n_population == 2))

  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "years",
    fullContribution = TRUE,
    completeDatabaseIntervals = FALSE,
    minCellCount = 0
  )

  expect_true(all(prev$n_population == c(2, 1, 1)))

  expect_true(prevalenceAttrition(prev) %>%
    dplyr::filter(reason ==
            "Do not satisfy full contribution requirement for an interval") %>%
    dplyr::pull("excluded_subjects") == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check periods follow calendar dates", {
  skip_on_cran()
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
  expect_true(all(lubridate::year(prev2$prevalence_start_date) ==
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

  # for overall
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "overall",
                             minCellCount = 0,
                             fullContribution = FALSE,
                             completeDatabaseIntervals = FALSE
  )
  expect_true(prev$prevalence_start_date[1] ==
                as.Date("2011-01-15"))
  expect_true(prev$prevalence_end_date[1] ==
                as.Date("2013-06-15"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check multiple outcome ids", {
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
    observation_period_start_date = as.Date("2011-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 2), # two different outcome ids
    subject_id = c("1", "2"),
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
    observation_period_start_date = as.Date("2012-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- tibble::tibble(
    cohort_definition_id = c(1, 2), # two different outcome ids
    subject_id = c("1", "2"),
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
    interval = c("months", "years"),
    minCellCount = 0
  )
  expect_true(nrow(prev) > 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check messages when vebose is true", {
  skip_on_cran()
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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  expect_error(estimatePrevalence(
    cdm = "a",
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 1,
    denominatorCohortId = 1
  ))

  expect_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 1,
    denominatorCohortId = 1,
    tablePrefix = NULL,
    returnParticipants = TRUE
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check user point prevalence function", {
  skip_on_cran()
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

  expect_true(all(names(prev) == names(prev_point)))
  expect_true(all(names(prevalenceSet(prev)) ==
    names(prevalenceSet(prev_point))))
  expect_true(all(names(prev) ==
    names(prev_point)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check user period prevalence function", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePrevalence(
    cdm = cdm,
    type = "period",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  expect_true(all(names(prev) == names(prev_period)))
  expect_true(all(names(prevalenceSet(prev)) ==
    names(prevalenceSet(prev_period))))
  expect_true(all(names(prev) ==
    names(prev_period)))

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

  # three observation periods for 1 person
  # and a couple of consecutive events lost to washout
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
    strataTable = conditionX,
    outcomeTable = outcomeTable
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = 1
  )

  # should expect for period prevalence monthly 3 times with n_cases 1,
  # and denominator 1 only at inclusion criteria satisfaction
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0
  )
  expect_true(sum(ppe$n_cases) == 3)
  expect_true(sum(ppe$n_population) == 8 + 8 + 14)

  # same if we look back 1 day, as some repeated events at month 8 disappear
  # but the person still has an outcome then
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    minCellCount = 0,
    outcomeLookbackDays = 1
  )
  expect_true(sum(ppe$n_cases) == 3)

  # if we look back 365 days, all outcomes count monthly for a whole year
  # after their onset, so we should see 4+3+14
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

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check confidence intervals", {
  skip_on_cran()
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
  hmisc_ci <- Hmisc::binconf(prev$n_cases, prev$n_population,
    alpha = 0.05,
    method = c("wilson"),
    return.df = TRUE
  )
  expect_equal(prev$prevalence_95CI_lower, hmisc_ci$Lower,
    tolerance = 1e-2
  )
  expect_equal(prev$prevalence_95CI_upper, hmisc_ci$Upper,
    tolerance = 1e-2
  )

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check attrition", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = c("Male", "Female")
  )
  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = "years",
    minCellCount = 0
  )

  # for female cohort we should have a row for those excluded for not being male
  expect_true(any("Not Female" == prevalenceSet(prev) %>%
    dplyr::filter(denominator_sex == "Female") %>%
    dplyr::inner_join(prevalenceAttrition(prev),
      by = "analysis_id", multiple = "all"
    ) %>%
    dplyr::pull(.data$reason)))
  # for male, the opposite
  expect_true(any("Not Male" == prevalenceSet(prev) %>%
    dplyr::filter(denominator_sex == "Male") %>%
    dplyr::inner_join(prevalenceAttrition(prev),multiple = "all",
      by = "analysis_id"
    ) %>%
    dplyr::pull(.data$reason)))

  # check we can pick out specific analysis attrition
  expect_true(nrow(prevalenceAttrition(result = prev) %>%
                     dplyr::filter(analysis_id == 1)) > 1)
  expect_true(nrow(prevalenceAttrition(result = prev) %>%
                     dplyr::filter(analysis_id == 2)) > 1)

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

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm
  )
  prev <- estimatePrevalence(cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "point",
    interval = "years",
    completeDatabaseIntervals = TRUE,
    minCellCount = 0
  )

  expect_true(prevalenceAttrition(prev) %>%
    dplyr::filter(reason == "Not observed during the complete database interval") %>%
    dplyr::pull("excluded_subjects") == 1)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check compute permanent", {
  skip_on_cran()

  # using temp
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"

  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    interval = "years"
  )
  # if using temp tables
  # we have temp tables created by dbplyr
  expect_true(any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                                      "dbplyr_")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # using permanent
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"

  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm,
                                           tablePrefix = "result")
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    interval = "years",
    tablePrefix = "result"
  )

  # we´ll now have the stem table
  expect_true(any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
                             schema = attr(cdm, "write_schema")),
    "result")))
  # with no temp tables created by dbplyr
  expect_false(any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                                      "dbplyr_")))

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    interval = "years",
    tablePrefix = "result",
    returnParticipants = TRUE
  )
  expect_true(any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
                             schema = attr(cdm, "write_schema")),
    "result_prevalence_participants")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: check participants", {
  skip_on_cran()

  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"

  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm,
                                           tablePrefix = "test",
                                           sex = c("Male", "Female", "Both"),
                                           ageGroup = list(c(0,50),
                                                           c(51,100)))
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    tablePrefix = "test",
    returnParticipants = TRUE
  )

  # we should have cleaned up all the intermediate tables
  expect_true(all(CDMConnector::listTables(attr(cdm, "dbcon"),
                                           schema = attr(cdm, "write_schema")) %in%
                    c("test_denominator",
                      "test_prevalence_participants",
                      "test_denominator_attrition",
                      "test_denominator_set" ,
                      "test_denominator_count",
                      "vocabulary" ,
                      "cdm_source", "outcome", "strata",
                      "observation_period", "person" )))
  expect_true(all(!c("test_prevalence_analysis_1",
                     "test_prev_working_1") %in%
                    CDMConnector::listTables(attr(cdm, "dbcon"),
                                             schema = attr(cdm,
                                                           "write_schema"))))

  expect_equal(names(participants(prev, 1) %>%
                       head(1) %>%
                       dplyr::collect()),
               c("subject_id",
                 "cohort_start_date",
                 "cohort_end_date",
                 "outcome_start_date"
               ))

 expect_true(nrow(participants(prev, 1) %>%
    dplyr::collect() %>%
    dplyr::filter(is.na(cohort_start_date))) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
