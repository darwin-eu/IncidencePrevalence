test_that("redshift test", {
  skip_if(Sys.getenv("CDM5_REDSHIFT_USER") == "")

  con <- DBI::dbConnect(RPostgres::Redshift(),
                        dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                        host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                        port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                        user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                        password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))

  cdm <- CDMConnector::cdm_from_con(
    con = con,
    cdm_schema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA")
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

  cdm$outcome <- cdm$denominator %>% head(10000) %>% CDMConnector::computeQuery()

  point_prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )

  expect_s3_class(point_prev, "IncidencePrevalenceResult")

  period_prev <- estimatePeriodPrevalence(completeDatabaseIntervals =FALSE,
                                          cdm = cdm,
                                          denominatorTable = "denominator",
                                          outcomeTable = "outcome",
                                          verbose = TRUE
  )

  expect_s3_class(period_prev, "IncidencePrevalenceResult")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )

  expect_s3_class(inc, "IncidencePrevalenceResult")

  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    verbose = TRUE
  )

  expect_s3_class(inc, "IncidencePrevalenceResult")

  DBI::dbDisconnect(con)
})


