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
  working_denominator_pop <- collect_denominator_pops(db,
    cdm_database_schema,
    study_start_date = NULL,
    study_end_date = NULL,
    study_age_stratas = list(c(10, 15)),
    study_sex_stratas = "Male",
    study_days_prior_history = 365
  )


  result <- calculate_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome="cohorts",
                                    cohort_id_outcome=1,
                                    study_denominator_pop=study_pops,
                                    cohort_id_denominator_pop="17",
                                    time_interval=c("Months"),
                                    prior_event_lookback=NULL,
                                    repetitive_events=FALSE,
                                    confidence_intervals="exact",
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
  testthat::expect_true(!is.null(result$calendar_month) &
    sum(is.na(result$calendar_month)) == 0)
  testthat::expect_true(!is.null(result$calendar_year) &
    sum(is.na(result$calendar_year)) == 0)
  # testthat::expect_true(!is.null(result$strata) &
  #   sum(is.na(result$strata)) == 0)
  # testthat::expect_true(!is.null(result$strata_value) &
  #   sum(is.na(result$strata_value)) == 0)
})
