test_that("mock db checks", {
library(tibble)
library(dplyr)
library(DBI)
# duckdb mock database
db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
# one person, one observation periods
person<-tibble(person_id="1",
       gender_concept_id="8507",
       year_of_birth=2000,
       month_of_birth=06,
       day_of_birth=01)
observation_period<-tibble(observation_period_id="1",
       person_id="1",
       observation_period_start_date=as.Date("2010-01-01"),
       observation_period_end_date=as.Date("2010-06-01"))
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
                      overwrite = TRUE)
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
                      overwrite = TRUE
    )
  })

dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL)
expect_true(nrow(dpop)==1)
expect_true(dpop$cohort_start_date==as.Date("2010-01-01"))
expect_true(dpop$cohort_end_date==as.Date("2010-06-01"))

expect_message(get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    verbose=TRUE))

dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    start_date=as.Date("2010-02-15"),
                    end_date = as.Date("2010-05-15"))
expect_true(nrow(dpop)==1)
expect_true(dpop$cohort_start_date==as.Date("2010-02-15"))
expect_true(dpop$cohort_end_date==as.Date("2010-05-15"))


dpop1<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    sex="Male")
dpop2<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    sex="Both")
dpop3<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    sex="Female")
expect_true(nrow(dpop1)==1)
expect_true(nrow(dpop2)==1)
expect_true(is.null(dpop3))

# check min_age
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    min_age=10)
expect_true(nrow(dpop)==1)
# start date is now date of 10th birthday
expect_true(dpop$cohort_start_date==as.Date("2010-06-01"))

# check max_age
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    max_age=10)
expect_true(nrow(dpop)==1)

# end date is now date of 10th birthday
## SHOULD IT NOT BE DAY BEFORE 11th BIRTHDAY?
expect_true(dpop$cohort_end_date==as.Date("2010-06-01"))
# expect_true(dpop$cohort_end_date==as.Date("2011-05-31"))







# one person, two observation periods
person<-tibble(person_id="1",
       gender_concept_id="8507",
       year_of_birth=2000,
       month_of_birth=06,
       day_of_birth=01)
observation_period<-tibble(observation_period_id=c("1","2"),
       person_id=rep("1",2),
       observation_period_start_date=c(as.Date("2010-01-01"),as.Date("2011-01-01")),
       observation_period_end_date=c(as.Date("2010-06-01"),as.Date("2011-06-01")))
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
                      overwrite = TRUE)
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
                      overwrite = TRUE
    )
  })
# expect two rows
# one per onservation period
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL)
expect_true(nrow(dpop)==2)
# expect one rows- onservation period in 2011
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    start_date=as.Date("2011-01-01"))
expect_true(nrow(dpop)==1)
expect_true(dpop$cohort_start_date==as.Date("2011-01-01"))
expect_true(dpop$cohort_end_date==as.Date("2011-06-01"))

# expect one rows- onservation period in 2010
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL,
                    end_date=as.Date("2010-12-31"))
expect_true(nrow(dpop)==1)
expect_true(dpop$cohort_start_date==as.Date("2010-01-01"))
expect_true(dpop$cohort_end_date==as.Date("2010-06-01"))


# missing month of birth
person<-tibble(person_id="1",
       gender_concept_id="8507",
       year_of_birth=2000,
       month_of_birth=NA,
       day_of_birth=01)
observation_period<-tibble(observation_period_id=c("1"),
       person_id=c("1"),
       observation_period_start_date=c(as.Date("2010-01-01")),
       observation_period_end_date=c(as.Date("2010-06-01")))
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
                      overwrite = TRUE)
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
                      overwrite = TRUE
    )
  })
# expect one row - dob should be imputed if missing only month
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL)
expect_true(nrow(dpop)==1)

# missing day of birth
person<-tibble(person_id="1",
       gender_concept_id="8507",
       year_of_birth=2000,
       month_of_birth=06,
       day_of_birth=NA)
observation_period<-tibble(observation_period_id=c("1"),
       person_id=c("1"),
       observation_period_start_date=c(as.Date("2010-01-01")),
       observation_period_end_date=c(as.Date("2010-06-01")))
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
                      overwrite = TRUE)
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
                      overwrite = TRUE
    )
  })
# expect one row - dob should be imputed if missing only day
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL)
expect_true(nrow(dpop)==1)


# missing day of birth and month of birth
person<-tibble(person_id="1",
       gender_concept_id="8507",
       year_of_birth=2000,
       month_of_birth=NA,
       day_of_birth=NA)
observation_period<-tibble(observation_period_id=c("1"),
       person_id=c("1"),
       observation_period_start_date=c(as.Date("2010-01-01")),
       observation_period_end_date=c(as.Date("2010-06-01")))
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
                      overwrite = TRUE)
  })
DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
                      overwrite = TRUE
    )
  })
# expect one row - dob should be imputed if missing month and day
dpop<-get_denominator_pop(db=db,
                    cdm_database_schema=NULL)
expect_true(nrow(dpop)==1)

DBI::dbDisconnect(db)

})

test_that("various checks for working example full db", {
# full database
  library(DBI)
  library(dplyr)
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
    cdm_database_schema = cdm_database_schema,
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
  testthat::expect_true(!is.null(result$person_id) &
    sum(is.na(result$person_id)) == 0)
  testthat::expect_true(!is.null(result$cohort_start_date) &
    sum(is.na(result$cohort_start_date)) == 0)
  testthat::expect_true(!is.null(result$cohort_end_date) &
    sum(is.na(result$cohort_end_date)) == 0)

  # end date after start date
  testthat::expect_true(all(result$cohort_start_date <=
    result$cohort_end_date) == TRUE)

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
    cdm_database_schema = NULL,
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
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = c(10, 15),
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))

  # max_age not one number
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = c(10, 15),
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  ))


  # sex not only one of the options
  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
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
    cdm_database_schema = cdm_database_schema,
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

test_that("edge cases where NULL result should be returned", {
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

  expect_true(is.null(get_denominator_pop(
    db = db,
    cdm_database_schema = cdm_database_schema,
    start_date = as.Date("2100-01-01"),
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  )))

  expect_true(is.null(get_denominator_pop(
    db = db,
    cdm_database_schema = cdm_database_schema,
    start_date = NULL,
    end_date = as.Date("1800-01-01"),
    min_age = NULL,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  )))

  expect_true(is.null(get_denominator_pop(
    db = db,
    cdm_database_schema = cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = 155,
    max_age = NULL,
    sex = c("Both"),
    days_prior_history = 0,
    verbose = FALSE
  )))


  expect_true(is.null(get_denominator_pop(
    db = db,
    cdm_database_schema = cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = 1,
    sex = c("Both"),
    days_prior_history = 1000,
    verbose = FALSE
  )))


  DBI::dbDisconnect(db)
})
