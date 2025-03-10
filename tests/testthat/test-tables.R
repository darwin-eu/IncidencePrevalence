test_that("test tables", {
  skip_on_cran()

  cdm <- mockIncidencePrevalence()

  cdm <- generateDenominatorCohortSet(cdm = cdm,
                                      name = "denominator")
  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  # test prevalence works
  expect_no_error(tablePrevalence(prev_period, type = "gt"))
  expect_no_error(tablePrevalence(prev_period |>
                                    omopgenerics::suppress(5),
                                  type = "gt"))
  expect_no_error(tablePrevalence(prev_period,
                                  groupColumn = c("denominator_cohort_name",
                                                  "outcome_cohort_name"),
                                  hide = NULL))
  expect_no_error(tablePrevalence(prev_period,
                  hide = "prevalence_end_date",
                  settingsColumn = "denominator_age_group",
                  groupColumn = "denominator_age_group"))
  expect_no_error(tablePrevalenceAttrition(prev_period))

  # point prevalence
  prev_point <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "weeks")
  )
  expect_no_error(tablePrevalence(prev_point,
    type = "flextable",
    groupColumn = list("Group" = c("cdm_name", "denominator_cohort_name", "outcome_cohort_name")),
    hide = NULL
  ))
  expect_no_error(tablePrevalence(prev_point,
    header = c("outcome_cohort_name", "estimate_name"),
    groupColumn = NULL,
    hide = NULL
  ))
  expect_no_error(tablePrevalenceAttrition(prev_point))

  # incidence
  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995,
      "first", "second"
    )) %>%
    dplyr::compute(temporary = FALSE, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  expect_no_error(tableIncidence(inc |>
                                   omopgenerics::suppress(5),
                                 type = "tibble"))
  expect_no_error(tableIncidence(inc, type = "tibble"))
  expect_no_error(tableIncidence(inc, type = "flextable",
                                 header = "my_strata",
                                 groupColumn = "outcome_cohort_name"))
  expect_no_error(tableIncidenceAttrition(inc))

  # test >1 result
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "weeks"),
    strata = list(c("my_strata"))
  )
  expect_no_error(tableIncidence(inc))

  cdm <- generateDenominatorCohortSet(cdm = cdm,
                                      name = "denominator",
                                      ageGroup = list(c(0, 50),
                                                      c(51, 150)),
                                      sex = c("Male", "Female"),
                                      daysPriorObservation = c(0, 30))
  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  expect_no_error(tablePrevalence(prev_period,
                                  settingsColumn =
                                    c("denominator_age_group", "denominator_sex",
                                      "denominator_days_prior_observation")
                                  ))
  expect_no_error(tablePrevalenceAttrition(prev_period, hide = "reason_id",
                                           settingsColumn =
                                             c("denominator_age_group", "denominator_sex",
                                               "denominator_days_prior_observation")))


  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "weeks")
  )

  expect_no_error(tableIncidence(inc, groupColumn = "analysis_interval",
                                 hide = c("denominator_cohort_name"),
                                  settingsColumn =
                                    c("denominator_age_group", "denominator_sex",
                                      "denominator_days_prior_observation")
  ))
  expect_no_error(tableIncidenceAttrition(inc,
                                 settingsColumn =
                                   c("denominator_age_group", "denominator_sex",
                                     "denominator_days_prior_observation")
  ))


  omopgenerics::cdmDisconnect(cdm)
})

test_that("test importing results", {
  skip_on_cran()

  cdm <- mockIncidencePrevalence()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  prev_path <- tempdir("prev")
  omopgenerics::exportSummarisedResult(prev_period, path = prev_path)

  prev_period_imported <- omopgenerics::importSummarisedResult(prev_path)
  expect_no_error(tablePrevalence(prev_period_imported, type = "gt"))

  # incidence
  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995,
      "first", "second"
    )) %>%
    dplyr::compute(temporary = FALSE, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  inc_path <- tempdir("inc")
  omopgenerics::exportSummarisedResult(inc, path = inc_path)

  inc_imported <- omopgenerics::importSummarisedResult(prev_path)
  expect_no_error(tableIncidence(inc_imported, type = "tibble"))

  results_path <- tempdir("results")
  results <- bind(inc, prev_period)
  omopgenerics::exportSummarisedResult(results, path = results_path)

  results_imported <- omopgenerics::importSummarisedResult(results_path)
  expect_no_error(tableIncidence(results_imported))
  expect_no_error(tablePrevalence(results_imported))

  omopgenerics::cdmDisconnect(cdm)
})
