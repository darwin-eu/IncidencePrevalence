test_that("check gathering of results", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev1 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )
  prev2 <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )
  inc1 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeWashout = 0
  )
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    outcomeWashout = 180
  )

  # with both types of results
  g1 <- gatherIncidencePrevalenceResults(cdm = cdm,
                                         resultList = list(prev1, prev2, inc1, inc2))
  expect_true(is.list(g1))
  expect_true(inherits(g1, "IncidencePrevalenceGatheredResult"))
  expect_true(length(g1) == 5)
  expect_true(all(names(g1) == c(
    "prevalence_estimates", "prevalence_attrition",
    "incidence_estimates", "incidence_attrition",
    "cdm_snapshot"
  )))
  expect_true(attr(g1, "cdm_name") == "test database")

  # with only prevalence
  g2 <- gatherIncidencePrevalenceResults(cdm = cdm,
                                         resultList = list(prev1, prev2))
  expect_true(is.list(g2))
  expect_true(length(g2) == 3)
  expect_true(all(names(g2) == c("prevalence_estimates", "prevalence_attrition",
                                 "cdm_snapshot")))
  # with only incidence
  g3 <- gatherIncidencePrevalenceResults(cdm = cdm,
                                         resultList = list(inc1))
  expect_true(is.list(g3))
  expect_true(length(g3) == 3)
  expect_true(all(names(g3) == c("incidence_estimates", "incidence_attrition",
                                 "cdm_snapshot")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("expected errors", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0
  )
  expect_error(gatherIncidencePrevalenceResults(cdm = "a",
                                                resultList = list(prev, "b")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
