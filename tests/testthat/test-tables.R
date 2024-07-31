test_that("test tables", {
  cdm <- mockIncidencePrevalenceRef()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  gt1 <- tablePrevalence(prev_period, type = "gt")
  expect_true("gt_tbl" %in% class(gt1))

  # point prevalence
  prev_point <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  expect_no_error(gt2 <- tablePrevalence(prev_point))

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
  tableInc <- tableIncidence(inc,
                             type = "flextable")
  expect_true("flextable" %in% class(tableInc))

  CDMConnector::cdm_disconnect(cdm)
})
