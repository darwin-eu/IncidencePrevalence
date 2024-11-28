test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalence() %>%
    generateDenominatorCohortSet(name = "denominator")

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  )
  my_settings <- settings(prev)
  expect_true(nrow(my_settings) > 0)

  expect_identical(colnames(prev),
                   c("result_id", "cdm_name", "group_name",
                     "group_level", "strata_name", "strata_level",
                     "variable_name", "variable_level", "estimate_name",
                     "estimate_type", "estimate_value", "additional_name",
                     "additional_level"))
  expect_true(all(c("result_id", "result_type", "group", "additional",
                    "package_name", "package_version", "min_cell_count",
                    "analysis_type", "analysis_interval",
                    "analysis_complete_database_intervals", "analysis_full_contribution",
                    "denominator_age_group", "denominator_sex",
                    "denominator_days_prior_observation", "denominator_start_date",
                    "denominator_end_date", "denominator_target_cohort_name",
                    "denominator_time_at_risk") %in% colnames(settings(prev))))


  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: checks on working example", {
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

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("months", "years")
  )
  expect_true(nrow(prev) >= 1)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: working examples 2", {
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
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
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

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check minimum counts", {
  skip_on_cran()
  # 20 people
  personTable <- dplyr::tibble(
    person_id = as.integer(c(1:20)),
    gender_concept_id = as.integer(rep(8507, 20)),
    year_of_birth = as.integer(rep(2000, 20)),
    month_of_birth = as.integer(rep(01, 20)),
    day_of_birth = as.integer(rep(01, 20))
  )
  observationPeriodTable <- dplyr::bind_rows(
    dplyr::tibble(
      observation_period_id = as.integer(c(1:17)),
      person_id = as.integer(c(1:17)),
      observation_period_start_date = rep(as.Date("2000-01-01"), 17),
      observation_period_end_date = rep(as.Date("2000-01-31"), 17)
    ),
    dplyr::tibble(
      observation_period_id = as.integer(c(18:20)),
      person_id = as.integer(c(18:20)),
      observation_period_start_date = rep(as.Date("2000-01-01"), 3),
      observation_period_end_date = rep(as.Date("2012-06-01"), 3)
    )
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

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    type = "period",
    interval = "months"
  )
  prev_est <- prev |>
    dplyr::filter(estimate_name == "outcome_count")
  expect_true(prev_est$estimate_value[1] == "17")
  expect_true(prev_est$estimate_value[2] == "3")
  expect_true(prev_est$estimate_value[3] == "0")
  prev_est <- prev |>
    dplyr::filter(estimate_name == "denominator_count")
  expect_true(prev_est$estimate_value[1] == "20")
  expect_true(prev_est$estimate_value[2] == "3")
  expect_true(prev_est$estimate_value[3] == "3")
  prev_est <- prev |>
    dplyr::filter(estimate_name == "prevalence")
  expect_true(!is.na(prev_est$estimate_value[1]))
  expect_true(!is.na(prev_est$estimate_value[2]))
  expect_true(!is.na(prev_est$estimate_value[3]))
  prev_est <- prev |>
    dplyr::filter(estimate_name == "prevalence_95CI_lower")
  expect_true(!is.na(prev_est$estimate_value[1]))
  prev_est <- prev |>
    dplyr::filter(estimate_name == "prevalence_95CI_upper")
  expect_true(!is.na(prev_est$estimate_value[1]))

  # suppress results
  prev <- omopgenerics::suppress(prev, minCellCount = 5)
  prev_est <- prev |>
    dplyr::filter(estimate_name == "outcome_count")
  expect_true(prev_est$estimate_value[1] == "17")
  expect_true(prev_est$estimate_value[2] == "-")
  expect_true(prev_est$estimate_value[3] == "0") # don't suppress zero
  prev_est <- prev |>
    dplyr::filter(estimate_name == "denominator_count")
  expect_true(prev_est$estimate_value[1] == "20")
  expect_true(prev_est$estimate_value[2] == "-")
  expect_true(prev_est$estimate_value[3] == "-")
  prev_est <- prev |>
    dplyr::filter(estimate_name == "prevalence")
  expect_true(prev_est$estimate_value[1] != "-")
  expect_true(prev_est$estimate_value[2] == "-")
  expect_true(prev_est$estimate_value[3] == "0")
  prev_est <- prev |>
    dplyr::filter(estimate_name == "prevalence_95CI_lower")
  expect_true(!is.na(prev_est$estimate_value[1]))
  expect_true(prev_est$estimate_value[2] == "-")
  expect_true(prev_est$estimate_value[3] == "0")
  prev_est <- prev |>
    dplyr::filter(estimate_name == "prevalence_95CI_upper")
  expect_true(prev_est$estimate_value[1] != "-")
  expect_true(prev_est$estimate_value[2] == "-")
  expect_equal(as.numeric(prev_est$estimate_value[3]), 0.56, tolerance = 0.1)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check study time periods", {
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
    observation_period_end_date = as.Date("2010-12-31")
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

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "point",
                             interval = "months"
  )

  # we expect 12 months of which the last in December
  # the last month should also be included
  # as the person goes up to the last day of the month
  expect_true(nrow(prev |>
                     dplyr::filter(
                       estimate_name == "outcome_count")) == 12)


  # overall period
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "overall"
  )
  # no overall for point
  expect_error(estimatePrevalence(cdm,
                     denominatorTable = "denominator",
                     outcomeTable = "outcome",
                     type = "point",
                     interval = "overall"
  ))
  # just one row
  expect_true(nrow(prev |>
                     dplyr::filter(estimate_name == "outcome_count")) == 1)

  CDMConnector::cdm_disconnect(cdm)

  # should return empty if no study days
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
    observation_period_end_date = as.Date("2010-11-15")
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

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             fullContribution = TRUE,
                             interval = "weeks"
  )
  expect_true(nrow(prev |>
                     dplyr::filter(estimate_name == "outcome_count")) == 45)

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             fullContribution = TRUE,
                             interval = "months"
  )
  expect_true(nrow(prev |>
                     dplyr::filter(estimate_name == "outcome_count")) == 10)

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             fullContribution = TRUE,
                             interval = "years"
  )
  expect_true(nrow(prev |>
                     dplyr::filter(estimate_name == "outcome_count")) == 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check fullContribution requirement", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = as.integer(c(1, 2, 3)),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = as.integer(c(1, 2, 3)),
    person_id = as.integer(c(1, 2, 3)),
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
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "years",
                             fullContribution = FALSE,
                             completeDatabaseIntervals = FALSE
  )
  expect_true(all(prev |>
                    dplyr::filter(
                      estimate_name == "denominator_count") |>
                    dplyr::pull("estimate_value") == "2"))

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "years",
                             fullContribution = TRUE,
                             completeDatabaseIntervals = FALSE
  )
  expect_true(all(prev |>
                    dplyr::filter(
                      estimate_name == "denominator_count") |>
                    dplyr::pull("estimate_value") == c("2", "1", "1")))
  expect_true(prev |>
                omopgenerics::filterSettings(result_type == "prevalence_attrition") |>
                dplyr::filter(strata_level == "Do not satisfy full contribution requirement for an interval") |>
                dplyr::filter(variable_name == "excluded_subjects") |>
                dplyr::pull("estimate_value") == "1")

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check periods follow calendar dates", {
  skip_on_cran()
  # check that even if study_start_date is during a period
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
    observation_period_start_date = as.Date("2010-06-05"),
    observation_period_end_date = as.Date("2013-06-15")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = c(
      as.Date("2011-01-31"),
      as.Date("2011-02-01"),
      as.Date("2011-03-01")
    ),
    cohort_end_date = c(
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

  # if completeDatabaseIntervals is TRUE we should go from 2010 to 2013
  # but if FALSE we should go from 2011 to 2012
  # for yearly incidence
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )
  prev1 <- estimatePrevalence(cdm,
                              denominatorTable = "denominator",
                              outcomeTable = "outcome",
                              type = "period",
                              interval = "years",
                              fullContribution = FALSE,
                              completeDatabaseIntervals = FALSE
  )
  expect_true(nrow(prev1 |>
                     dplyr::filter(
                       estimate_name == "denominator_count")) == 4)
  expect_true(all(clock::get_year(
    prev1 |> omopgenerics::splitAdditional() |>
      dplyr::filter(
        estimate_name == "denominator_count") |>
      dplyr::pull("prevalence_start_date") %>%
      as.Date()) ==
      c("2010", "2011", "2012", "2013")))

  prev2 <- estimatePrevalence(cdm,
                              denominatorTable = "denominator",
                              outcomeTable = "outcome",
                              type = "period",
                              interval = "years",
                              fullContribution = FALSE,
                              completeDatabaseIntervals = TRUE
  )
  expect_true(all(clock::get_year(
    prev2 |> omopgenerics::splitAdditional() |>
      dplyr::filter(
        estimate_name == "denominator_count") |>
      dplyr::pull("prevalence_start_date") |>
      as.Date()) ==
      c("2011", "2012")))

  # for months
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2011-01-15"), as.Date(NA))
  )

  # where we expect the study to start on 2011-01-15
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "months",
                             fullContribution = FALSE,
                             completeDatabaseIntervals = FALSE
  )

  expect_true(prev |> omopgenerics::splitAdditional() |>
                dplyr::filter(
                  estimate_name == "denominator_count") |>
                head(1) |>
                dplyr::pull("prevalence_start_date") ==
                "2011-01-15")
  # where we expect the study to start the next month
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "months",
                             fullContribution = FALSE,
                             completeDatabaseIntervals = TRUE
  )
  expect_true(prev |> omopgenerics::splitAdditional() |>
                dplyr::filter(
                  estimate_name == "denominator_count") |>
                head(1) |>
                dplyr::pull("prevalence_start_date") ==
                "2011-02-01")

  # for overall
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "overall",
                             fullContribution = FALSE,
                             completeDatabaseIntervals = FALSE
  )
  expect_true(prev |> omopgenerics::splitAdditional() |>
                dplyr::filter(
                  estimate_name == "denominator_count") |>
                head(1) |>
                dplyr::pull("prevalence_start_date") ==
                "2011-01-15")
  expect_true(prev |> omopgenerics::splitAdditional() |>
                dplyr::filter(
                  estimate_name == "denominator_count") |>
                head(1) |>
                dplyr::pull("prevalence_end_date") ==
                "2013-06-15")

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check multiple outcome ids", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = as.integer(c(1, 2)),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = as.Date("2011-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 2L), # two different outcome ids
    subject_id = c(1L, 2L),
    cohort_start_date = c(
      as.Date("2011-02-05")
    ),
    cohort_end_date = c(
      as.Date("2011-02-05")
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

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             outcomeCohortId = c(1,2),
                             type = "period",
                             interval = "years"
  )

  expect_true(all(prev |>
                    dplyr::filter(
                      estimate_name == "outcome_count") |>
                    dplyr::pull("estimate_value") == "1"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: some empty result sets", {
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
    observation_period_start_date = as.Date("2012-02-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcomeTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 2L), # two different outcome ids
    subject_id = c(1L, 2L),
    cohort_start_date = c(
      as.Date("2012-02-05")
    ),
    cohort_end_date = c(
      as.Date("2012-02-05")
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

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = "years"
  )
  expect_true(nrow(prev %>%
                     omopgenerics::filterSettings(result_type == "prevalence")) == 0)

  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "period",
                             interval = c("months", "years")
  )
  expect_true(nrow(prev %>%
                     omopgenerics::filterSettings(result_type == "prevalence")) > 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check expected errors", {
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

  expect_error(estimatePrevalence(
    cdm = "a",
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeCohortId = 1,
    denominatorCohortId = 1
  ))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check user point prevalence function", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

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
  expect_true(all(names(prev) ==
                    names(prev_point)))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check user period prevalence function", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

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
  expect_true(all(names(prev) ==
                    names(prev_period)))

  CDMConnector::cdm_disconnect(cdm)
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

  # three observation periods for 1 person
  # and a couple of consecutive events lost to washout
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L, 4L),
    person_id = c(1L, 1L, 1L, 2L),
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

  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 1L),
    subject_id = c(1L, 1L, 2L),
    cohort_start_date = c(
      as.Date("2005-04-01"),
      as.Date("2009-06-10"),
      as.Date("2013-01-01")
    ),
    cohort_end_date = c(
      as.Date("2005-11-29"),
      as.Date("2010-01-02"),
      as.Date("2015-01-01")
    )
  )

  outcomeTable <- dplyr::tibble(
    cohort_definition_id = as.integer(c(1, 1, 1, 1, 1, 1, 1)),
    subject_id = as.integer(c(1, 1, 1, 1, 1, 1, 2)),
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
  ppe <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    fullContribution = FALSE,
    completeDatabaseIntervals = FALSE
  )
  # nobody should appear in 2006
  expect_true(ppe %>%
                omopgenerics::splitAdditional() |>
                dplyr::filter(prevalence_start_date == "2006-01-01",
                              estimate_name == "denominator_count") %>%
                dplyr::pull("estimate_value") == 0)
  expect_true(ppe %>%
                omopgenerics::splitAdditional() |>
                dplyr::filter(prevalence_start_date == "2006-01-01",
                              estimate_name == "outcome_count") %>%
                dplyr::pull("estimate_value") == 0)

  # one person with an event in 2005
  expect_true(ppe %>%
                omopgenerics::filterSettings(result_type == "prevalence") %>%
                omopgenerics::splitAdditional() |>
                dplyr::filter(clock::get_year(prevalence_start_date %>%
                                                as.Date()) == "2005",
                              estimate_name == "denominator_count") %>%
                dplyr::pull("estimate_value") == "1")
  expect_true(ppe %>%
                omopgenerics::filterSettings(result_type == "prevalence") %>%
                omopgenerics::splitAdditional() |>
                dplyr::filter(clock::get_year(prevalence_start_date %>%
                                                as.Date()) == "2005",
                              estimate_name == "outcome_count") %>%
                dplyr::pull("estimate_value") == "1")


  # as for point prevalence, we would expect no positive n_cases at default
  ppo <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  expect_true(sum(as.numeric(ppo %>%
                               omopgenerics::splitAdditional() |>
                               dplyr::filter(estimate_name == "outcome_count") %>%
                               dplyr::pull("estimate_value"))) == 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check confidence intervals", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "point",
                             interval = "years"
  )

  pkg_est <- prev %>%
    omopgenerics::filterSettings(result_type == "prevalence") %>%
    dplyr::select("estimate_name",
                  "estimate_value", "additional_level") |>
    tidyr::pivot_wider(names_from = "estimate_name",
                       values_from = "estimate_value") |>
    dplyr::filter(denominator_count > 1)

  # compare our wilson CIs with those from Hmisc
  hmisc_ci <- Hmisc::binconf(as.numeric(pkg_est$outcome_count),
                             as.numeric(pkg_est$denominator_count),
                             alpha = 0.05,
                             method = c("wilson"),
                             return.df = TRUE
  )

  expect_equal(as.numeric(pkg_est$prevalence_95CI_lower),
               hmisc_ci$Lower,
               tolerance = 1e-2
  )
  expect_equal(as.numeric(pkg_est$prevalence_95CI_upper),
               hmisc_ci$Upper,
               tolerance = 1e-2
  )

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check attrition", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = c("Male", "Female")
  )
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "point",
                             interval = "years"
  )

  # for female cohort we should have a row for those excluded for not being male
  expect_true(nrow(prev |>
                     omopgenerics::filterSettings(result_type == "prevalence_attrition",
                                                    denominator_sex == "Female") |>
                     dplyr::filter(strata_level == "Not Female")) > 0)

  # for male, the opposite
  expect_true(nrow(prev |>
                     omopgenerics::filterSettings(result_type == "prevalence_attrition",
                                                    denominator_sex == "Male") |>
                     dplyr::filter(strata_level == "Not Male")) > 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check attrition with complete database intervals", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L, 3L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L, 2L, 3L),
    observation_period_start_date = c(
      as.Date("2000-06-01"),
      as.Date("2000-06-01"),
      as.Date("2000-06-01")
    ),
    observation_period_end_date = c(
      as.Date("2011-07-01"),
      as.Date("2012-06-01"),
      as.Date("2000-06-15")
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

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator"
  )
  prev <- estimatePrevalence(cdm,
                             denominatorTable = "denominator",
                             outcomeTable = "outcome",
                             type = "point",
                             interval = "years",
                             completeDatabaseIntervals = TRUE
  )

  expect_true(prev |>
                omopgenerics::filterSettings(result_type == "prevalence_attrition") |>
                dplyr::filter(strata_level == "Not observed during the complete database interval") |>
                dplyr::filter(variable_name == "excluded_subjects") |>
                dplyr::pull("estimate_value") == "1")

  # check min cell suppression
  prev2 <- estimatePrevalence(cdm,
                              denominatorTable = "denominator",
                              outcomeTable = "outcome",
                              type = "point",
                              interval = "years",
                              completeDatabaseIntervals = TRUE
  ) |> omopgenerics::suppress(5)

  expect_true(prev2 |>
                omopgenerics::filterSettings(result_type == "prevalence_attrition") |>
                dplyr::filter(strata_level == "Not observed during the complete database interval") |>
                dplyr::filter(variable_name == "excluded_subjects") |>
                dplyr::pull("estimate_value") == "-")

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check compute permanent", {
  skip_on_cran()

  # using permanent (no prefix)
  cdm <- mockIncidencePrevalence(sampleSize = 1000)

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "dpop"
  )
  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    interval = "years"
  )
  # no temp tables created by dbplyr
  expect_false(any(stringr::str_starts(
    CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
                             schema = attr(attr(cdm, "cdm_source"), "write_schema")
    ),
    "dbplyr_"
  )))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: if missing cohort attributes", {
  # missing cohort_set
  cdm <- mockIncidencePrevalence()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  attr(cdm$outcome, "cohort_set") <- NULL
  expect_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  ))
  CDMConnector::cdm_disconnect(cdm)

  # missing cohort_count
  cdm <- mockIncidencePrevalence()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  attr(cdm$outcome, "cohort_attrition") <- NULL
  expect_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  ))
  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: test empty outcome table works", {
  skip_on_cran()

  cdm <- mockIncidencePrevalence(sampleSize = 1000)

  cdm[["outcome"]] <- cdm[["outcome"]] %>%
    dplyr::filter(cohort_definition_id == 33)

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  expect_no_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  ))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: prevalence using strata vars", {

  cdm <- mockIncidencePrevalence(sampleSize = 1000,
                                 outPre = 0.7)

  cdm <- generateDenominatorCohortSet(cdm = cdm,
                                      name = "denominator")

  prev_orig <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  )

  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1990,
                                             "first", "second")) %>%
    dplyr::compute(temporary = FALSE,
                   name = "denominator")

  prev <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("my_strata"))
  )

  expect_true(all(c("overall", "first", "second") %in%
                    unique(prev |> dplyr::pull("strata_level"))))

  # original without strata should be the same as "Overall" strata
  prev1 <- prev_orig |>
    omopgenerics::filterSettings(result_type == "prevalence")
  attr(prev1, "settings") <- NULL
  prev2 <- prev |>
    omopgenerics::filterSettings(result_type == "prevalence") %>%
    dplyr::filter(strata_level == "overall")
  attr(prev2, "settings") <- NULL
  expect_equal(prev1,prev2)


  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata2 =  dplyr::if_else(month(cohort_start_date)<7,
                                               "a", "b")) %>%
    dplyr::compute(temporary = FALSE,
                   name = "denominator")

  prev2 <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("my_strata","my_strata2"))
  )

  expect_true(all(c("overall", "first &&& a",
                    "first &&& b",
                    "second &&& a",
                    "second &&& b") %in%
                    unique(prev2 |> dplyr::pull("strata_level"))))

  prev3 <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("my_strata"),
                  c("my_strata2"),
                  c("my_strata", "my_strata2")))

  expect_true(all(c("overall",
                    "first",
                    "second",
                    "first &&& a",
                    "first &&& b",
                    "second &&& a",
                    "second &&& b") %in%
                    unique(prev3 |> dplyr::pull("strata_level"))))



  # without overall strata
  prev4 <- estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("my_strata"),
                  c("my_strata2"),
                  c("my_strata", "my_strata2")),
    includeOverallStrata = FALSE)
  expect_true(all(c("first",
                    "second",
                    "first &&& a",
                    "first &&& b",
                    "second &&& a",
                    "second &&& b") %in%
                    unique(prev4 |> dplyr::pull("strata_level"))))
  expect_false(c("overall") %in%
                 unique(prev4 |> dplyr::pull("strata_level")))

  expect_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("not_a_col"))))

  expect_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("my_strata", "not_a_col"))))

  expect_error(estimatePrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years",
    strata = list(c("my_strata"), c("not_a_col"))))

  CDMConnector::cdm_disconnect(cdm)
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

  pre1 <- estimatePrevalence(cdm, "target", "outcome")
  pre2 <- estimatePrevalence(cdm, "target", "outcome", 1, 1)
  pre3 <- estimatePrevalence(cdm, "target", "outcome", "cohort_1", "cohort_1")

  expect_true(all.equal(pre1,pre2))
  expect_true(all.equal(pre2,pre3))

  CDMConnector::cdmDisconnect(cdm)
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

  expect_error(estimatePrevalence(cdm, "target", "outcome", 2))

  CDMConnector::cdmDisconnect(cdm)
})



