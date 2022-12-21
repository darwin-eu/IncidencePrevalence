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
  g1 <- gatherIncidencePrevalenceResults(resultList = list(prev1, prev2, inc1, inc2))
  expect_true(is.list(g1))
  expect_true(inherits(g1, "IncidencePrevalenceGatheredResult"))
  expect_true(length(g1) == 4)
  expect_true(all(names(g1) == c(
    "prevalence_estimates", "prevalence_attrition",
    "incidence_estimates", "incidence_attrition"
  )))
  # with only prevalence
  g2 <- gatherIncidencePrevalenceResults(resultList = list(prev1, prev2))
  expect_true(is.list(g2))
  expect_true(length(g2) == 2)
  expect_true(all(names(g2) == c("prevalence_estimates", "prevalence_attrition")))
  # with only incidence
  g3 <- gatherIncidencePrevalenceResults(resultList = list(inc1))
  expect_true(is.list(g3))
  expect_true(length(g3) == 2)
  expect_true(all(names(g3) == c("incidence_estimates", "incidence_attrition")))

  # with database name
  g4 <- gatherIncidencePrevalenceResults(
    resultList = list(prev1, prev2, inc1, inc2),
    databaseName = "test_database"
  )
  expect_true(all(g4$prevalence_estimates$database_name == "test_database"))

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
  expect_error(gatherIncidencePrevalenceResults(resultList = list(prev, "a")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
