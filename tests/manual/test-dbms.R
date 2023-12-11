test_that("dbms test", {
  # Update  to your database details as appropriate here
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
    cdm_schema = "public",
    write_schema = c(schema = "results", prefix = "incprev_bench_")
  )

  # run for a random sample of 1,000,000 people from the person table
  # this benchmarking hits most of the functionality of the package so
  # this should give some confidence it won´t fall over on the dbms

  # the outcome cohort used is a sample of the denominator cohort
  # (so as to ensure that)


  cdm <- CDMConnector::cdm_sample(cdm = cdm, n = 1000000)
  dplyr::count(cdm$person)

  # using temp tables
  timings_temp <- benchmarkIncidencePrevalence(cdm,
    cohortDateRange = c(
      as.Date("2012-01-01"),
      as.Date("2015-12-31")
    ),
    temporary = TRUE,
    nOutcomes = 1,
    prevOutcomes = 0.10
  )
  expect_true(tibble::is_tibble(timings_temp))

  # using permanent tables
  timings_perm <- benchmarkIncidencePrevalence(cdm,
    cohortDateRange = c(
      as.Date("2012-01-01"),
      as.Date("2015-12-31")
    ),
    temporary = FALSE,
    nOutcomes = 1,
    prevOutcomes = 0.10
  )
  expect_true(tibble::is_tibble(timings_perm))

  # returning participants
  timings_perm2 <- benchmarkIncidencePrevalence(cdm,
    cohortDateRange = c(
      as.Date("2012-01-01"),
      as.Date("2015-12-31")
    ),
    temporary = FALSE,
    nOutcomes = 1,
    prevOutcomes = 0.10,
    returnParticipants = TRUE
  )

  expect_true(tibble::is_tibble(timings_perm2))


  # Drop permanent tables created
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
