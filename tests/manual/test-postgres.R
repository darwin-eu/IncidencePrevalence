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

  # run for a random sample of 1000
  # if we want to run in full remove sample
  timings <- benchmarkIncidencePrevalence(cdm,
                                                    verbose = TRUE,
                                                    sample=1000)

})

