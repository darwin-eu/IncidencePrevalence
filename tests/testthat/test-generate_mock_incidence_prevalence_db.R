test_that("check working example with defaults", {
  library(DBI)
  library(duckdb)
  library(dplyr)

  db<-generate_mock_incidence_prevalence_db()

  db_inherits_check <- inherits(db, "DBIConnection")
  expect_true(db_inherits_check)

  expect_true(nrow(dplyr::tbl(db, "person") %>%
    collect())>=1)
  expect_true(nrow(dplyr::tbl(db, "observation_period") %>%
                     collect())>=1)


  person_db_names <- c(
    "person_id", "gender_concept_id", "year_of_birth",
    "month_of_birth", "day_of_birth"
  )
  person_db_names_check <- all(person_db_names %in%
                                 names(dplyr::tbl(db, "person") %>%
                                         utils::head(1) %>%
                                         dplyr::collect() %>%
                                         dplyr::rename_with(tolower)))
  expect_true(person_db_names_check)

  obs_period_db_names <- c(
    "observation_period_id", "person_id",
    "observation_period_start_date", "observation_period_end_date"
  )
  obs_period_db_names_check <- all(obs_period_db_names %in%
                                     names(dplyr::tbl(db, "observation_period") %>%
                                             utils::head(1) %>%
                                             dplyr::collect() %>%
                                             dplyr::rename_with(tolower)))
  expect_true(obs_period_db_names_check)

  dbDisconnect(db, shutdown=TRUE)


})

test_that("check working example with outcome table", {
  library(DBI)
  library(duckdb)
  library(dplyr)

  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05")
    )
  )

  db<-generate_mock_incidence_prevalence_db(outcome=outcome)

  expect_true(nrow(dplyr::tbl(db, "outcome") %>%
                     collect())==1)

  outcome_db_names <- c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  )
  outcome_db_names_check <- all(outcome_db_names %in%
                                 names(dplyr::tbl(db, "outcome") %>%
                                         utils::head(1) %>%
                                         dplyr::collect() %>%
                                         dplyr::rename_with(tolower)))
  expect_true(outcome_db_names_check)

  dbDisconnect(db, shutdown=TRUE)


})

test_that("check working example sample size and outcome prevalence option", {
  library(DBI)
  library(duckdb)
  library(dplyr)


  db<-generate_mock_incidence_prevalence_db(sample_size = 100, out_pre = 0.2)

  expect_true(nrow(dplyr::tbl(db, "person") %>%
                     collect())==100)

  expect_true(nrow(dplyr::tbl(db, "outcome") %>%
                     collect())==20)

  outcome_db_names <- c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  )
  outcome_db_names_check <- all(outcome_db_names %in%
                                  names(dplyr::tbl(db, "outcome") %>%
                                          utils::head(1) %>%
                                          dplyr::collect() %>%
                                          dplyr::rename_with(tolower)))
  expect_true(outcome_db_names_check)

  dbDisconnect(db, shutdown=TRUE)


})

test_that("check working example sample size and outcome varies by gender and age option", {
  library(DBI)
  library(duckdb)
  library(dplyr)


  db<-generate_mock_incidence_prevalence_db(sample_size = 100, out_pre = 0.2, gender_beta = -1, age_beta = 1, intercept = -1)

  db2<-generate_mock_incidence_prevalence_db(sample_size = 100, out_pre = 0.2, gender_beta = -1, age_beta = 1)

  expect_true(nrow(dplyr::tbl(db, "person") %>%
                     collect())==100)

  expect_true(nrow(dplyr::tbl(db2, "person") %>%
                     collect())==100)

  expect_true(nrow(dplyr::tbl(db, "outcome") %>%
                     collect())!=20)

  expect_true(nrow(dplyr::tbl(db2, "outcome") %>%
                     collect())==20)


  outcome_db_names <- c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  )
  outcome_db_names_check <- all(outcome_db_names %in%
                                  names(dplyr::tbl(db, "outcome") %>%
                                          utils::head(1) %>%
                                          dplyr::collect() %>%
                                          dplyr::rename_with(tolower)))
  expect_true(outcome_db_names_check)

  dbDisconnect(db, shutdown=TRUE)


})

test_that("check working example for multiple outcome options", {
  library(DBI)
  library(duckdb)
  library(dplyr)


  db <-
    generate_mock_incidence_prevalence_db(
      sample_size = 200,
      out_pre = 0.2,
      max_outcomes_per_person = 1
    )
  db2 <-
    generate_mock_incidence_prevalence_db(
      sample_size = 200,
      out_pre = 0.2,
      max_outcomes_per_person = 2
    )
  db3 <-
    generate_mock_incidence_prevalence_db(
      sample_size = 200,
      out_pre = 0.2,
      max_outcomes_per_person = 3
    )
  db4 <-
    generate_mock_incidence_prevalence_db(
      sample_size = 1,
      out_pre = 1,
      max_outcomes_per_person = 10
    )

  expect_true(nrow(dplyr::tbl(db, "outcome") %>%
                     collect()) == 40)

  expect_true(nrow(dplyr::tbl(db2, "outcome") %>%
                     collect()) > nrow(dplyr::tbl(db, "outcome") %>%
                                         collect()))

  expect_true(nrow(dplyr::tbl(db3, "outcome") %>%
                     collect()) > nrow(dplyr::tbl(db2, "outcome") %>%
                                         collect()))

  expect_true(
    nrow(
      dplyr::tbl(db, "outcome") %>% distinct(subject_id) %>% collect()
    ) ==
      nrow(
        dplyr::tbl(db2, "outcome") %>% distinct(subject_id) %>% collect()
      )
  )

  expect_true(
    nrow(
      dplyr::tbl(db2, "outcome") %>% distinct(subject_id) %>% collect()
    ) ==
      nrow(
        dplyr::tbl(db3, "outcome") %>% distinct(subject_id) %>% collect()
      )
  )


  #checking cohort_start_date of 2nd outcome comes after 1st outcome end date
  expect_true(
    dplyr::tbl(db4, "outcome") %>% filter(row_number() == 1) %>% select(cohort_end_date) %>% collect() <
      dplyr::tbl(db4, "outcome") %>% filter(row_number() == 2) %>% select(cohort_start_date) %>% collect()
  )



  outcome_db_names <- c("cohort_definition_id",
                        "subject_id",
                        "cohort_start_date",
                        "cohort_end_date")
  outcome_db_names_check <- all(
    outcome_db_names %in%
      names(
        dplyr::tbl(db, "outcome") %>%
          utils::head(1) %>%
          dplyr::collect() %>%
          dplyr::rename_with(tolower)
      )
  )
  expect_true(outcome_db_names_check)

  dbDisconnect(db, shutdown = TRUE)


})







test_that("check expected errors", {
  library(DBI)
  library(duckdb)
  library(dplyr)

  testthat::expect_error(
    generate_mock_incidence_prevalence_db(person = "x"))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(observation_period = "x"))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(outcome = "x"))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(sample_size = -1))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(sample_size = 100,
                                          out_pre = -0.2))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(
      earliest_date_of_birth = as.Date("2000-01-01"),
      latest_date_of_birth = as.Date("1999-01-01")
    ))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(
      earliest_observation_start_date = as.Date("2000-01-01"),
      latest_observation_start_date = as.Date("1999-01-01")
    ))
  testthat::expect_error(
    generate_mock_incidence_prevalence_db(
      min_days_to_observation_end = 10,
      max_days_to_observation_end = 1
    ))


})
