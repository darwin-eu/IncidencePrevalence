test_that("various checks for working example", {
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
    cdm_database_schema=cdm_database_schema,
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

# no missing values
  testthat::expect_true(!is.null(result$person_id) & sum(is.na(result$person_id)) == 0)
  testthat::expect_true(!is.null(result$cohort_start_date) & sum(is.na(result$cohort_start_date)) == 0)
  testthat::expect_true(!is.null(result$cohort_end_date) & sum(is.na(result$cohort_end_date)) == 0)

# end date after start date
  testthat::expect_true(all(result$cohort_start_date <= result$cohort_end_date) == TRUE)

  DBI::dbDisconnect(db)
})

test_that("expected errors", {
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

  # not a dbi connection
 testthat::expect_error(get_denominator_pop(
    db = "a",
    cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

 # missing cdm_database_schema
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=NULL,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

   # start_date not a date
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = "a",
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

     # end_date not a date
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = "a",
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

       # min_age not one number
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = "a",
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = c(10,15),
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

         # max_age not one number
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = "a",
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = c(10,15),
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))


       # sex not only one of the options
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Male", "Female"),
    days_prior_history = 0,
    verbose = FALSE
  ))

    testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("women"),
    days_prior_history = 0,
    verbose = FALSE
  ))

       # days_prior_history not a number
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = "a",
    verbose = FALSE
  ))

         # days_prior_history should not be a negative number
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = -365,
    verbose = FALSE
  ))

  DBI::dbDisconnect(db)
})

test_that("edge cases where zero row result should be returned", {
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

  expect_true(nrow(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = as.Date("2100-01-01"),
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))==0)

    expect_true(nrow(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = as.Date("1800-01-01"),
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))==0)

     expect_true(nrow(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = 155,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))==0)


  expect_true(nrow(get_denominator_pop(
    db = db,
    cdm_database_schema=cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = 1,
    sex = c("Both"),
    days_prior_history = 1000,
    verbose = FALSE
  ))==0)


  DBI::dbDisconnect(db)
})
