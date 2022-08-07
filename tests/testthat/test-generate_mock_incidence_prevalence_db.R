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


test_that("check expected errors", {
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
