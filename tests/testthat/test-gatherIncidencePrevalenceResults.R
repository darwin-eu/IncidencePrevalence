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

  # adding outcome name
  g5 <- gatherIncidencePrevalenceResults(
    resultList = list(prev1, prev2, inc1, inc2),
    outcomeCohortId = 1,
    outcomeCohortName = "test_cohort",
    databaseName = "test_database"
  )
  expect_true(all(g5$prevalence_estimates$outcome_cohort_name == "test_cohort"))
  expect_true(all(g5$incidence_estimates$outcome_cohort_name == "test_cohort"))



  g6 <- gatherIncidencePrevalenceResults(
    resultList = list(inc1),
    outcomeCohortId = 1,
    outcomeCohortName = "test_cohort",
    databaseName = "test_database"
  )
  expect_true(all(names(g6) == c("incidence_estimates", "incidence_attrition")))

  g7 <- gatherIncidencePrevalenceResults(
    resultList = list(prev1),
    outcomeCohortId = 1,
    outcomeCohortName = "test_cohort",
    databaseName = "test_database"
  )
  expect_true(all(names(g7) == c("prevalence_estimates", "prevalence_attrition")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("multiple cohorts to rename", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  cdm$outcome <- dplyr::union_all(
    cdm$denominator$cohort_definition_id_1 %>%
      dplyr::slice_sample(n = 100) %>%
      dplyr::mutate(cohort_definition_id = 1),
    cdm$denominator$cohort_definition_id_1 %>%
      dplyr::slice_sample(n = 100) %>%
      dplyr::mutate(cohort_definition_id = 2)
  ) %>%
    dplyr::compute()

  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "years"
  )

  # adding outcome name
  g <- gatherIncidencePrevalenceResults(
    resultList = list(prev, inc),
    outcomeCohortId = c(1, 2),
    outcomeCohortName = c(
      "test_cohort_1",
      "test_cohort_2"
    ),
    databaseName = "test_database"
  )

  expect_true(all(unique(g$prevalence_estimates$outcome_cohort_id) == c(1, 2)))

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
