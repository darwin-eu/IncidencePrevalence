test_that("Spark prevalence test", {
  # This test takes ~10 minutes to run
  skip_on_ci()

  con <- DBI::dbConnect(odbc::odbc(), "Databricks")

  cdm <- CDMConnector::cdm_from_con(
    con = con,
    cdm_schema = "omop531"
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2007-01-01"),
    ageGroup = list(
      c(40, 150),
      c(40, 64)
    ),
    sex = c("Male", "Female"),
    daysPriorHistory = 365,
    sample = 1000000,
    verbose = TRUE
  )

  expect_s3_class(dplyr::collect(head(cdm$denominator)), "data.frame")

  cdm$outcome <- cdm$denominator %>% head(10000) %>% CDMConnector::computeQuery()

  point_prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )
  expect_s3_class(point_prev, "data.frame")

  period_prev <- estimatePeriodPrevalence(completeDatabaseIntervals = FALSE,
                                          cdm = cdm,
                                          denominatorTable = "denominator",
                                          outcomeTable = "outcome",
                                          verbose = TRUE
  )
  expect_s3_class(period_prev, "data.frame")

  DBI::dbDisconnect(con)
})

test_that("Spark Incidence test", {
  skip_on_ci()

  con <- DBI::dbConnect(odbc::odbc(), "Databricks", bigint = "numeric")

  cdm <- CDMConnector::cdm_from_con(
    con = con,
    cdm_schema = "omop531"
  )

  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2007-01-01"),
    daysPriorHistory = 365,
    sample = 1000000,
    verbose = TRUE
  )

  expect_s3_class(dplyr::collect(head(cdm$denominator)), "data.frame")

  cdm$outcome <- cdm$denominator %>% head(10000) %>% CDMConnector::computeQuery()

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )

  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    verbose = TRUE
  )

  results <- gatherIncidencePrevalenceResults(
    cdm = cdm,
    resultList = list(inc, inc2),
    databaseName = "test_database"
  )

  expect_s3_class(results, "IncidencePrevalenceGatheredResult")

  DBI::dbDisconnect(con)
})
