test_that("test tables", {
  cdm <- mockIncidencePrevalenceRef()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  # test prevalence works
  tablePrevalence(prev_period, type = "gt")
  tablePrevalence(prev_period, groupColumn = c("denominator_cohort_name", "outcome_cohort_name"))
  tablePrevalence(prev_period, hide = "prevalence_end_date", settingsColumns = "denominator_age_group", groupColumn = "denominator_age_group")
  tablePrevalenceAttrition(prev_period)

  # point prevalence
  prev_point <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "weeks")
  )
  tablePrevalence(prev_point, type = "flextable", groupColumn = list("Group" = c("cdm_name", "denominator_cohort_name", "outcome_cohort_name")))
  tablePrevalence(prev_point, header = c("outcome_cohort_name", "estimate_name"))
  tablePrevalenceAttrition(prev_point)

  # incidence
  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995,
                                             "first", "second")) %>%
    dplyr::compute()
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  tableIncidence(inc, type = "tibble")
  tableIncidence(inc, type = "flextable", header = "my_strata", groupColumn = "outcome_cohort_name")
  tableIncidenceAttrition(inc)

  # test >1 result
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("years", "weeks"),
    strata = list(c("my_strata"))
  )
  tableIncidence(inc)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("test importing results", {
  cdm <- mockIncidencePrevalenceRef()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  prev_path <- tempdir("prev")
  omopgenerics::exportSummarisedResult(prev_period,path = prev_path)

  prev_period_imported <-  omopgenerics::importSummarisedResult(prev_path)
  expect_no_error(tablePrevalence(prev_period_imported, type = "gt"))

  # incidence
  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995,
                                             "first", "second")) %>%
    dplyr::compute()
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  inc_path <- tempdir("inc")
  omopgenerics::exportSummarisedResult(inc, path = inc_path)

  inc_imported <-  omopgenerics::importSummarisedResult(prev_path)
  expect_no_error(tableIncidence(inc_imported, type = "tibble"))

  results_path <- tempdir("results")
  results <- bind(inc, prev_period)
  omopgenerics::exportSummarisedResult(results, path = results_path)

  results_imported <-  omopgenerics::importSummarisedResult(results_path)
  expect_no_error(tableIncidence(results_imported))
  expect_no_error(tablePrevalence(results_imported))

  CDMConnector::cdm_disconnect(cdm)
})
