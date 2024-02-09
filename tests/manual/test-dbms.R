test_that("dbms test", {
  # Update  to your database details as appropriate here
  skip_if(Sys.getenv("DB_SERVER_cdmgold202007_dbi") == "")
  db <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = "cdm_gold_202207",
    port = Sys.getenv("DB_PORT"),
    host = Sys.getenv("DB_HOST"),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD")
  )
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "public_100k",
    write_schema = c(schema = "results", prefix = "incprev_bench_")
  )
  dplyr::count(cdm$person)

  timings_temp <- benchmarkIncidencePrevalence(cdm,
    nOutcomes = 1,
    prevOutcomes = 0.10
  )
  expect_true(tibble::is_tibble(timings_temp))


  # Drop any permanent tables created
  CDMConnector::listTables(attr(cdm, "dbcon"),
    schema = attr(cdm, "write_schema")
  )
  CDMConnector::dropTable(cdm = cdm, name = starts_with("incprev_bench_"))

  # 06 May 2023, CPRD GOLD, Postgres
  timings_temp %>% dplyr::select("task", "time_taken_mins")
  # task                                    time_taken_mins
  # <chr>                                             <dbl>
  # 1 generating denominator (8 cohorts)                 1.79
  # 2 yearly point prevalence, 1 outcome(s)              0.47
  # 3 monthly point prevalence, 1 outcome(s)             0.64
  # 4 yearly period prevalence, 1 outcome(s)             0.5
  # 5 monthly period prevalence, 1 outcome(s)            0.73
  # 6 yearly incidence, 1 outcome(s)                     0.26
  # 7 monthly incidence, 1 outcome(s)                    0.42

  timings_perm %>% dplyr::select("task", "time_taken_mins")
  # task                                    time_taken_mins
  # <chr>                                             <dbl>
  # 1 generating denominator (8 cohorts)                 1.67
  # 2 yearly point prevalence, 1 outcome(s)              0.53
  # 3 monthly point prevalence, 1 outcome(s)             0.69
  # 4 yearly period prevalence, 1 outcome(s)             0.62
  # 5 monthly period prevalence, 1 outcome(s)            0.83
  # 6 yearly incidence, 1 outcome(s)                     0.31
  # 7 monthly incidence, 1 outcome(s)                    0.48

  timings_perm2 %>% dplyr::select("task", "time_taken_mins")
  # task                                    time_taken_mins
  # <chr>                                             <dbl>
  # 1 generating denominator (8 cohorts)                 1.69
  # 2 yearly point prevalence, 1 outcome(s)              0.7
  # 3 monthly point prevalence, 1 outcome(s)             0.86
  # 4 yearly period prevalence, 1 outcome(s)             0.8
  # 5 monthly period prevalence, 1 outcome(s)            1.01
  # 6 yearly incidence, 1 outcome(s)                     0.48
  # 7 monthly incidence, 1 outcome(s)                    0.66
})


test_that("postgres test", {


 db <- DBI::dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
            host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
            user = Sys.getenv("CDM5_POSTGRESQL_USER"),
            password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
 cdm <- CDMConnector::cdm_from_con(
   con = db,
   cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
   write_schema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                    prefix = "incp_")
 )

 cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, name = "test_outcome",
                                               conceptSet = list("a" = 381566,
                                                                 "b" = 138525),
                                               overwrite = TRUE)

 cdm <- generateDenominatorCohortSet(
   cdm = cdm, name = "denominator_typical",
   overwrite =TRUE,
   cohortDateRange = c(
     as.Date("2000-01-01"),
     as.Date("2018-12-31")),
   daysPriorObservation = 180,
   sex = c("Male", "Female"),
   ageGroup = list(
     c(0, 25), c(26, 64),
     c(65, 79), c(80, 150)
   )
 )

 pp1 <- estimatePointPrevalence(cdm = cdm,
                         denominatorTable = "denominator_typical",
                         outcomeTable = "test_outcome")
 expect_true(tibble::is_tibble(pp1))

 pp2 <- estimatePeriodPrevalence(cdm = cdm,
                               denominatorTable = "denominator_typical",
                               outcomeTable = "test_outcome")
 expect_true(tibble::is_tibble(pp2))


 inc <- estimateIncidence(cdm = cdm,
                                 denominatorTable = "denominator_typical",
                                 outcomeTable = "test_outcome")
 expect_true(tibble::is_tibble(inc))

 CDMConnector::dropTable(cdm = cdm, name = starts_with("incp_"))

 CDMConnector::cdm_disconnect(cdm = cdm)

})

