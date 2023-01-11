test_that("test methods against test server", {
  skip_if(Sys.getenv("TESTDB_USER") == "")
  db <- DBI::dbConnect(odbc::odbc(),
    Driver   = "ODBC Driver 11 for SQL Server",
    Server   = Sys.getenv("darwinDbDatabaseServer"),
    Database = "sql-synthea-1M",
    UID      = Sys.getenv("darwinDbUser"),
    PWD      = Sys.getenv("darwinDbPassword"),
    Port     = Sys.getenv("darwinDbDatabasePort")
  )
  cdm_database_schema <- "cdm_synthea_1M"
  cdm <- CDMConnector::cdm_from_con(db,
    cdm_schema = cdm_database_schema
  )
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    daysPriorHistory = 180,
    verbose = TRUE
  )
  cdm$outcome <- cdm$denominator %>% head(100)

  pont_prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )

  results <- gatherResults(
    resultList = list(pont_prev, inc),
    outcomeCohortName = "test_sample",
    outcomeCohortId = 1,
    databaseName = "test_database"
  )
  expect_true(all(names(results) == c(
    "prevalence_estimates",
    "incidence_estimates"
  )))
})


test_that("test methods against OHDSI test SQL Server", {
  skip_if(Sys.getenv("CDM5_SQL_SERVER_USER") == "")
  skip_if_not("ODBC Driver 18 for SQL Server" %in% odbc::odbcListDrivers()$name)

  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "ODBC Driver 18 for SQL Server",
                        Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                        Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                        UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                        PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                        TrustServerCertificate = "yes",
                        Port     = 1433)


  cdm_database_schema <- "cdmv5.dbo"
  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = cdm_database_schema,
                                    cdm_tables = c(CDMConnector::tbl_group("default"), -visit_detail) # visit_detail is missing on ohdsi sql server v5.3 test database
  )

  debugonce(getDenominatorCohorts)
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    daysPriorHistory = 180,
    verbose = TRUE
  )
  cdm$outcome <- cdm$denominator %>% head(100)

  pont_prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )

  results <- gatherResults(
    resultList = list(pont_prev, inc),
    outcomeCohortName = "test_sample",
    outcomeCohortId = 1,
    databaseName = "test_database"
  )
  expect_true(all(names(results) == c(
    "prevalence_estimates",
    "incidence_estimates"
  )))
})
