test_that("checks on working example", {
  library(DBI)
  library(RPostgres)
  library(dplyr)
  db <- DBI::dbConnect(RPostgres::Postgres(),
    dbname = Sys.getenv("SERVER_DBI_TEST"),
    port = Sys.getenv("DB_PORT_TEST"),
    host = Sys.getenv("DB_HOST_TEST"),
    user = Sys.getenv("DB_USER_TEST"),
    password = Sys.getenv("DB_PASSWORD_TEST")
  )
  cdm_database_schema <- "omop21t2_test"

  # for one cohort,
  # collect_denominator_pops should give the same as get_denominator_pop
  expect_true(all(get_denominator_pop(db,
    cdm_database_schema,
    min_age = 10,
    max_age = 15,
    sex = c("Male"),
    days_prior_history = 365
  ) %>%
    select(person_id) %>%
    pull() ==
    collect_denominator_pops(db,
      cdm_database_schema,
      study_start_date = NULL,
      study_end_date = NULL,
      study_age_stratas = list(c(10, 15)),
      study_sex_stratas = "Male",
      study_days_prior_history = 365
    ) %>%
      select(person_id) %>%
      pull()))

  # variable names
  result <- collect_denominator_pops(db,
    cdm_database_schema,
    study_start_date = NULL,
    study_end_date = NULL,
    study_age_stratas = list(c(10, 15), c(16, 20)),
    study_sex_stratas = c("Female", "Male", "Both"),
    study_days_prior_history = c(0, 365)
  )
  expect_true(all(c(
    "cohort_definition_id",
    "person_id",
    "cohort_start_date", "cohort_end_date",
    "age_strata", "sex_strata", "required_days_prior_history"
  ) %in%
    names(result)))

  # no missing values
  testthat::expect_true(!is.null(result$cohort_definition_id) &
    sum(is.na(result$cohort_definition_id)) == 0)
  testthat::expect_true(!is.null(result$person_id) &
    sum(is.na(result$person_id)) == 0)
  testthat::expect_true(!is.null(result$cohort_start_date) &
    sum(is.na(result$cohort_start_date)) == 0)
  testthat::expect_true(!is.null(result$cohort_end_date) &
    sum(is.na(result$cohort_end_date)) == 0)
  testthat::expect_true(!is.null(result$age_strata) &
    sum(is.na(result$age_strata)) == 0)
  testthat::expect_true(!is.null(result$sex_strata) &
    sum(is.na(result$sex_strata)) == 0)
  testthat::expect_true(!is.null(result$required_days_prior_history) &
    sum(is.na(result$required_days_prior_history)) == 0)

  # testthat::expect_true(!is.null(result$study_start_date) &
  #                         sum(is.na(result$study_start_date)) == 0)
  # testthat::expect_true(!is.null(result$study_end_date) &
  #                         sum(is.na(result$study_end_date)) == 0)

  # end date after start date
  testthat::expect_true(all(result$cohort_start_date <=
    result$cohort_end_date) == TRUE)

  DBI::dbDisconnect(db)
})
