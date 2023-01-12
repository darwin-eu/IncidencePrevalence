test_that("sql server test", {
  skip_if(Sys.getenv("CDM5_SQL_SERVER_USER") == "")

  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 18 for SQL Server",
                        Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                        Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                        UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                        PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                        TrustServerCertificate = "yes",
                        Port     = 1433)

  cdm <- CDMConnector::cdm_from_con(
    con = con,
    cdm_schema = c("CDMV5", "dbo"),
    cdm_tables = c(CDMConnector::tbl_group("default"), -visit_detail) # visit_detail missing in test server
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


