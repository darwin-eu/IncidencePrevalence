test_that("mock db checks", {

# duckdb mock database
db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
person<-tibble(person_id="1",
       gender_concept_id="8507",
       year_of_birth=2000,
       month_of_birth=01,
       day_of_birth=01)
observation_period<-tibble(observation_period_id="1",
       person_id="1",
       observation_period_start_date=as.Date("2010-01-01"),
       observation_period_end_date=as.Date("2010-06-01"))
outcome<-tibble(cohort_definition_id="1",
                subject_id="1",
                cohort_start_date=c(as.Date("2010-02-05"),
                                    as.Date("2010-02-08"),
                                    as.Date("2010-02-20")),
                cohort_end_date=c(as.Date("2010-02-05"),
                                    as.Date("2010-02-08"),
                                    as.Date("2010-02-20")))
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
                      overwrite = TRUE)
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
                      overwrite = TRUE
    )
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome", outcome,
                      overwrite = TRUE
    )
  })
dpop<-collect_denominator_pops(db=db,
                    cdm_database_schema=NULL)
inc<-get_pop_incidence(db,
                  results_schema_outcome=NULL,
                  table_name_outcome="outcome",
                  cohort_id_outcome="1",
                  study_denominator_pop=dpop,
                  repetitive_events = FALSE)
expect_true(sum(inc$n_events)==1)
inc<-get_pop_incidence(db,
                  results_schema_outcome=NULL,
                  table_name_outcome="outcome",
                  cohort_id_outcome="1",
                  study_denominator_pop=dpop,
                  repetitive_events = TRUE,
                  prior_event_lookback=2)
expect_true(sum(inc$n_events)==3)
inc<-get_pop_incidence(db,
                  results_schema_outcome=NULL,
                  table_name_outcome="outcome",
                  cohort_id_outcome="1",
                  study_denominator_pop=dpop,
                  repetitive_events = TRUE,
                  prior_event_lookback=10)
expect_true(sum(inc$n_events)==2)





})


test_that("checks on working example", {
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(tibble)
  db <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = Sys.getenv("SERVER_DBI_TEST"),
    port = Sys.getenv("DB_PORT_TEST"),
    host = Sys.getenv("DB_HOST_TEST"),
    user = Sys.getenv("DB_USER_TEST"),
    password = Sys.getenv("DB_PASSWORD_TEST")
  )

  cdm_database_schema <- "omop21t2_test"
  results_schema_outcome <- "results21t2_test"
  table_name_outcome <- "cohorts"
study_pops<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2012-01-01"),
                         study_end_date=as.Date("2014-12-31"),
                         study_age_stratas = list(c(10,15), c(16,20), c(10,20)),
                         study_sex_stratas = c("Male", "Female", "Both"),
                         study_days_prior_history =c(0,365),
                         verbose = TRUE)

  result <- get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome="cohorts",
                                    cohort_id_outcome=1,
                                    study_denominator_pop=study_pops,
                                    cohort_id_denominator_pop="17",
                                    time_interval=c("Months"),
                                    prior_event_lookback=NULL,
                                    repetitive_events=FALSE,
                                    confidence_interval="exact",
                                    verbose=FALSE)
  # output format
  expect_true(tibble::is_tibble(result))
  # output variables
  expect_true(all(c(
    "n_persons",
    "person_days",
    "person_months",
    "person_years",
    "n_events",
    "ir",
    "ir_low",
    "ir_high",
    "calendar_month",
    "calendar_year",
    "age_strata",
    "sex_strata"
  ) %in%
    names(result)))

  # no missing values
  testthat::expect_true(!is.null(result$n_persons) &
    sum(is.na(result$n_persons)) == 0)
  testthat::expect_true(!is.null(result$person_days) &
    sum(is.na(result$person_days)) == 0)
  testthat::expect_true(!is.null(result$person_months) &
    sum(is.na(result$person_months)) == 0)
  testthat::expect_true(!is.null(result$person_years) &
    sum(is.na(result$person_years)) == 0)
  testthat::expect_true(!is.null(result$n_events) &
    sum(is.na(result$n_events)) == 0)
  testthat::expect_true(!is.null(result$ir_low) &
    sum(is.na(result$ir_low)) == 0)
  testthat::expect_true(!is.null(result$ir_high) &
    sum(is.na(result$ir_high)) == 0)
  # testthat::expect_true(!is.null(result$calendar_month) &
  #   sum(is.na(result$calendar_month)) == 0)
  testthat::expect_true(!is.null(result$calendar_year) &
    sum(is.na(result$calendar_year)) == 0)
  # testthat::expect_true(!is.null(result$strata) &
  #   sum(is.na(result$strata)) == 0)
  # testthat::expect_true(!is.null(result$strata_value) &
  #   sum(is.na(result$strata_value)) == 0)
})
