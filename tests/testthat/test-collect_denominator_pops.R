# test_that("checks on working example", {
#   library(DBI)
#   library(RPostgres)
#   library(dplyr)
#   db <- DBI::dbConnect(RPostgres::Postgres(),
#     dbname = Sys.getenv("SERVER_DBI_TEST"),
#     port = Sys.getenv("DB_PORT_TEST"),
#     host = Sys.getenv("DB_HOST_TEST"),
#     user = Sys.getenv("DB_USER_TEST"),
#     password = Sys.getenv("DB_PASSWORD_TEST")
#   )
#   cdm_database_schema <- "omop21t2_test"
#
#   # for one cohort,
#   # collect_denominator_pops should give the same as get_denominator_pop
#   expect_true(all(get_denominator_pop(db,
#     cdm_database_schema,
#     min_age = 10,
#     max_age = 15,
#     sex = c("Male"),
#     days_prior_history = 365
#   ) %>%
#     select(person_id) %>%
#     pull() ==
#     collect_denominator_pops(db,
#       cdm_database_schema,
#       study_start_date = NULL,
#       study_end_date = NULL,
#       study_age_stratas = list(c(10, 15)),
#       study_sex_stratas = "Male",
#       study_days_prior_history = 365
#     ) %>%
#       select(person_id) %>%
#       pull()))
#
#   # variable names
#   result <- collect_denominator_pops(db,
#     cdm_database_schema,
#     study_start_date = NULL,
#     study_end_date = NULL,
#     study_age_stratas = list(c(10, 15), c(16, 20)),
#     study_sex_stratas = c("Female", "Male", "Both"),
#     study_days_prior_history = c(0, 365)
#   )
#   expect_true(all(c(
#     "cohort_definition_id",
#     "person_id",
#     "cohort_start_date", "cohort_end_date",
#     "age_strata", "sex_strata", "required_days_prior_history"
#   ) %in%
#     names(result)))
#
#   # no missing values
#   testthat::expect_true(!is.null(result$cohort_definition_id) &
#     sum(is.na(result$cohort_definition_id)) == 0)
#   testthat::expect_true(!is.null(result$person_id) &
#     sum(is.na(result$person_id)) == 0)
#   testthat::expect_true(!is.null(result$cohort_start_date) &
#     sum(is.na(result$cohort_start_date)) == 0)
#   testthat::expect_true(!is.null(result$cohort_end_date) &
#     sum(is.na(result$cohort_end_date)) == 0)
#   testthat::expect_true(!is.null(result$age_strata) &
#     sum(is.na(result$age_strata)) == 0)
#   testthat::expect_true(!is.null(result$sex_strata) &
#     sum(is.na(result$sex_strata)) == 0)
#   testthat::expect_true(!is.null(result$required_days_prior_history) &
#     sum(is.na(result$required_days_prior_history)) == 0)
#
#   # testthat::expect_true(!is.null(result$study_start_date) &
#   #                         sum(is.na(result$study_start_date)) == 0)
#   # testthat::expect_true(!is.null(result$study_end_date) &
#   #                         sum(is.na(result$study_end_date)) == 0)
#
#   # end date after start date
#   testthat::expect_true(all(result$cohort_start_date <=
#     result$cohort_end_date) == TRUE)
#
#
#   DBI::dbDisconnect(db)
# })


test_that("mock db checks", {
library(DBI)
library(dplyr)
library(tibble)

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
       observation_period_end_date=as.Date("2012-06-01"))
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

# some pops with people, but some without
dpops <-collect_denominator_pops(db,
    cdm_database_schema=NULL,
    study_start_date = NULL,
    study_end_date = NULL,
    study_age_stratas = list(c(0, 59), c(60, 69)),
    study_sex_stratas = c("Female", "Male", "Both")
  )

# all pops without anyone
expect_message(collect_denominator_pops(db,
    cdm_database_schema=NULL,
    study_start_date = NULL,
    study_end_date = NULL,
    study_age_stratas = list(c(50, 59), c(60, 69)),
    study_days_prior_history = c(0, 365)
  ))


dbDisconnect(db)

})


# test_that("expected errors", {
#
#   library(DBI)
#   library(RPostgres)
#   db <- DBI::dbConnect(RPostgres::Postgres(),
#     dbname = Sys.getenv("SERVER_DBI_TEST"),
#     port = Sys.getenv("DB_PORT_TEST"),
#     host = Sys.getenv("DB_HOST_TEST"),
#     user = Sys.getenv("DB_USER_TEST"),
#     password = Sys.getenv("DB_PASSWORD_TEST")
#   )
#   cdm_database_schema <- "omop21t2_test"
#
#   # not a dbi connection
#   testthat::expect_error(collect_denominator_pops(db="a",
#     cdm_database_schema,
#     study_start_date = NULL,
#     study_end_date = NULL,
#     study_age_stratas = list(c(10, 15), c(16, 20)),
#     study_sex_stratas = c("Female", "Male", "Both"),
#     study_days_prior_history = c(0, 365)
#   ))
#
#   # not a dbi connection
#   testthat::expect_error(collect_denominator_pops(db="a",
#     cdm_database_schema,
#     study_start_date = NULL,
#     study_end_date = NULL,
#     study_age_stratas = list(c(10, 15), c(16, 20)),
#     study_sex_stratas = c("Women"),
#     study_days_prior_history = c(0, 365)
#   ))
#
#
# })
