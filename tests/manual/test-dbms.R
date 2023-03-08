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
    write_schema = "results"
  )

  # run for a random sample of 1,000,000 people from the person table
  # this benchmarking hits most of the functionality of the package so
  # this should give some confidence it won´t fall over on the dbms

  # the outcome cohort used is a sample of the denominator cohort
  # (so as to ensure that)

  # using temp tables
  timings_temp <- benchmarkIncidencePrevalence(cdm,
                                               startDate = as.Date("2012-01-01"),
                                               endDate = as.Date("2015-12-31"),
                                               tablePrefix = NULL,
                                               sample=1000000,
                                               nOutcomes = 1,
                                               prevOutcomes = 0.10,
                                               verbose = TRUE)
  expect_true(tibble::is_tibble(timings_temp))

  # using permanent tables
  timings_perm <- benchmarkIncidencePrevalence(cdm,
                                               startDate = as.Date("2012-01-01"),
                                               endDate = as.Date("2015-12-31"),
                                               tablePrefix = "incprev_bench",
                                               sample=1000000,
                                               nOutcomes = 1,
                                               prevOutcomes = 0.10,
                                               verbose = TRUE)
  expect_true(tibble::is_tibble(timings_perm))

  # returning participants
  timings_perm <- benchmarkIncidencePrevalence(cdm,
                                               startDate = as.Date("2012-01-01"),
                                               endDate = as.Date("2015-12-31"),
                                               tablePrefix = "incprev_bench",
                                               returnParticipants = TRUE,
                                               sample=1000000,
                                               nOutcomes = 1,
                                               prevOutcomes = 0.10,
                                               verbose = TRUE)
  expect_true(tibble::is_tibble(timings_perm))

  # 23rd January 2023, CPRD GOLD, Postgres
  timings_temp
  # A tibble: 7 × 8
  # task                                                    time_taken_mins
  # <chr>                                                             <dbl>
  # 1 generating typical denominator (32 cohorts)                      1.30
  # 2 yearly point prevalence, one outcome                             1.14
  # 3 monthly point prevalence, one outcome                            1.94
  # 4 yearly period prevalence, one outcome                            1.31
  # 5 monthly period prevalence, one outcome                           2.10
  # 6 yearly incidence, one outcome                                    0.903
  # 7 monthly incidence, one outcome                                   1.84

  timings_perm
  # A tibble: 7 × 9
  # task                                                    time_taken_mins
  # <chr>                                                             <dbl>
  # 1 generating typical denominator (24 cohorts)                     1.03
  # 2 yearly point prevalence, 1 outcome(s)                           1.31
  # 3 monthly point prevalence, 1 outcome(s)                          2.18
  # 4 yearly period prevalence, 1 outcome(s)                          1.61
  # 5 monthly period prevalence, 1 outcome(s)                         2.29
  # 6 yearly incidence, 1 outcome(s)                                  1.05
  # 7 monthly incidence, 1 outcome(s)                                 1.95

})


