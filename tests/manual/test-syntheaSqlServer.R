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
