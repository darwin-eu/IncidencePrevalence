test_that("postgres test", {
  skip_if(Sys.getenv("DB_SERVER_cdmgold202007_dbi") == "")

  db <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = Sys.getenv("DB_SERVER_cdmgold202007_dbi"),
    port = Sys.getenv("DB_PORT"),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "public"
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

  pont_prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    verbose = TRUE
  )
  period_prev <- estimatePeriodPrevalence(completeDatabaseIntervals =FALSE,
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
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180,
    verbose = TRUE
  )

  # run for a random sample of 1000
  # if we want to run in full remove sample
  timings <- benchmarkIncidencePrevalence(cdm,
                                                    verbose = TRUE,
                                                    sample=1000)


})

