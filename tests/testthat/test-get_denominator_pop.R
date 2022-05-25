test_that("output format", {
  library(DBI)
  library(RPostgres)
  db <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = Sys.getenv("SERVER_DBI_TEST"),
    port = Sys.getenv("DB_PORT_TEST"),
    host = Sys.getenv("DB_HOST_TEST"),
    user = Sys.getenv("DB_USER_TEST"),
    password = Sys.getenv("DB_PASSWORD_TEST")
  )
  cdm_database_schema <- "omop21t2_test"

  result <- get_denominator_pop(
    db = db,
    cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  )
  # variable names
  expect_true(all(c(
    "person_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(result)))
  DBI::dbDisconnect(db)
})




test_that("output format", {
  library(DBI)
  library(RPostgres)
  db <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = Sys.getenv("SERVER_DBI_TEST"),
    port = Sys.getenv("DB_PORT_TEST"),
    host = Sys.getenv("DB_HOST_TEST"),
    user = Sys.getenv("DB_USER_TEST"),
    password = Sys.getenv("DB_PASSWORD_TEST")
  )
  cdm_database_schema <- "omop21t2_test"

  result <- get_denominator_pop(
    db = db,
    cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  )


  testthat::expect_true(!is.null(result$person_id) & sum(is.na(result$person_id)) == 0)
  testthat::expect_true(!is.null(result$cohort_start_date) & sum(is.na(result$cohort_start_date)) == 0)
  testthat::expect_true(!is.null(result$cohort_end_date) & sum(is.na(result$cohort_end_date)) == 0)

  DBI::dbDisconnect(db)
})
