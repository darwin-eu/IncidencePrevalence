test_that("dbms test", {
  testthat::skip
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
    cdm_schema = "public",
    write_schema = c(schema ="results", prefix = "ip_b_")
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denom"
  )
  cdm <- CDMConnector::generate_concept_cohort_set(cdm,
                                                   concept_set =
                                                     list(a = 4050747),
                                                   name = "ip_outcome") # 3 mins and 36 secs

  expect_no_error(estimateIncidence(cdm,
                                    denominatorTable = "denom",
                                    outcomeTable = "ip_outcome")) #
  expect_no_error(estimatePointPrevalence(cdm,
                                          denominatorTable = "denom",
                                          outcomeTable = "ip_outcome")) #3 mins and 54 secs


  profvis::profvis({
    estimateIncidence(cdm,
                      denominatorTable = "denom",
                      outcomeTable = "ip_outcome")
  })

  profvis::profvis({
    estimatePointPrevalence(cdm,
                            denominatorTable = "denom",
                            outcomeTable = "ip_outcome")
  })




  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "public_100k",
    write_schema = c(schema ="results", prefix = "ip_b_")
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denom" ,
    cohortDateRange = c(as.Date("2000-01-01"), as.Date("2022-01-01")),
    ageGroup =list(
      c(0, 150),
      c(0, 150),
      c(0, 150)
    ),
    sex = c("Both", "Both", "Both"),
    daysPriorObservation = c(0, 365)
  )

  cdm <- CDMConnector::generate_concept_cohort_set(cdm,
                                                   concept_set =
                                                     list(a = 4050747,
                                                          b = 4077375),
                                                   name = "ip_outcome")



  expect_no_error(estimateIncidence(cdm,
                    denominatorTable = "denom",
                    outcomeTable = "ip_outcome")) # 1 mins and 28 secs
  expect_no_error(estimatePointPrevalence(cdm,
                    denominatorTable = "denom",
                    outcomeTable = "ip_outcome")) # 1 mins and 56 secs

  # Drop any permanent tables created
  CDMConnector::listTables(attr(cdm, "dbcon"),
    schema = attr(cdm, "write_schema")
  )
  CDMConnector::dropTable(cdm = cdm, name = tidyselect::starts_with("incprev_bench_"))

  # 06 May 2023, CPRD GOLD, Postgres
  timings %>% dplyr::select("task", "time_taken_mins")
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

 profvis::profvis({
   cdm <- generateDenominatorCohortSet(
     cdm = cdm,
     name = "denom",
     cohortDateRange = c(as.Date("2000-01-01"), as.Date("2022-01-01")),
     ageGroup =list(
       c(18, 150),
       c(18, 49),
       c(50, 59),
       c(60, 69),
       c(70, 79),
       c(80, 150)
     ),
     sex = c("Male", "Female", "Both"),
     daysPriorObservation = 365
   )
 })

 cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, name = "test_outcome",
                                               conceptSet = list("a" = 381566,
                                                                 "b" = 138525),
                                               overwrite = TRUE)


 timings <- benchmarkIncidencePrevalence(cdm,
                                         nOutcomes = 1,
                                         prevOutcomes = 0.10)

 CDMConnector::dropTable(cdm = cdm, name = tidyselect::starts_with("incp_"))

 CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("redshift test", {
  db <- DBI::dbConnect(RPostgres::Redshift(),
                       dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                       host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                       port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                       user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                       password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
    write_schema = c(schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
                     prefix = "incp_")
  )

  profvis::profvis({
    cdm <- generateDenominatorCohortSet(
      cdm = cdm,
      name = "denominator",
      cohortDateRange = as.Date(c("1990-01-01", "2020-12-31")),
      ageGroup = list(c(18,44), c(45,64),
                      c(65,74), c(75,100)),
      sex = c("Male", "Female", "Both"),
      daysPriorObservation = 1,
      requirementInteractions=FALSE
    )
  })


  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, name = "test_outcome",
                                                conceptSet = list("a" = 80502,
                                                                  "b" = 134736),
                                                overwrite = TRUE)

expect_no_error(estimateIncidence(
    cdm = a_cdm,
    returnParticipants = FALSE,
    denominatorTable = "denom",
    outcomeTable = "test_outcome",
    interval = "years"
  ))

  timings <- benchmarkIncidencePrevalence(cdm,
                                          nOutcomes = 1,
                                          prevOutcomes = 0.10)

  CDMConnector::dropTable(cdm = cdm, name = tidyselect::starts_with("incp_"))

  CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("sql server test", {
  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                       Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                       Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                       UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                       PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                       TrustServerCertificate="yes",
                       Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))

  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]],
    write_schema = c(schema =  strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]],
                     prefix = "incp_")
  )


  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denom" ,
    overwrite = TRUE
  )

  cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, name = "test_outcome",
                                                conceptSet = list("a" = 80502,
                                                                  "b" = 134736),
                                                overwrite = TRUE)

  estimateIncidence(
    cdm = cdm,
    returnParticipants = FALSE,
    denominatorTable = "denom",
    outcomeTable = "test_outcome",
    interval = "years"
  )

  timings <- benchmarkIncidencePrevalence(cdm,
                                               nOutcomes = 1,
                                               prevOutcomes = 0.10)

  CDMConnector::dropTable(cdm = cdm, name = tidyselect::starts_with("incp_"))

  CDMConnector::cdm_disconnect(cdm = cdm)

})

