
test_that("mock db: check output format", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  inc <- get_pop_incidence(db,
                           results_schema_outcome = NULL,
                           table_name_outcome = "outcome",
                           study_denominator_pop = dpop
  )

  expect_true(all(c(
    "n_persons",
    "person_days",
    "person_years",
    "n_events",
    "ir_100000_pys",
    "calendar_month",
    "calendar_year",
    "age_strata",
    "sex_strata"
  ) %in%
    names(inc)))

  dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check working example", {
  library(DBI)
  library(dplyr)
  library(tibble)

  person <- tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    )
  )

  db <- generate_mock_incidence_prevalence_db(outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  inc <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = FALSE,
    outcome_washout_window = 0
  )
  expect_true(sum(inc$n_events) == 1)

  inc <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = 2
  )
  expect_true(sum(inc$n_events) == 3)

  inc <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = 10
  )
  expect_true(sum(inc$n_events) == 2)

  # even if repetitive_events = TRUE,
  # if outcome_washout_window=NULL (all of history)
  # then it won´t be possible to have any recurrent events
  inc <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = NULL
  )
  expect_true(sum(inc$n_events) == 1)


  dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check study periods ", {
  library(DBI)
  library(dplyr)
  library(tibble)

  person <- tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2010-12-31")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    )
  )

  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

 inc<- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop
  )

   # we expect 12 months of which the last in december
   # the last month should also be included
   # as the person goes up to the last day of the month
   expect_true(length(inc$calendar_year)==12)
   expect_true(any(inc$calendar_month %in% 12))

  dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check washout windows", {
  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-12-31")
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-06-01"),
      # more than six months since the last event
      as.Date("2011-01-13"),
      # two days since the end of the last event
      as.Date("2011-01-16"),
      # one day since the end of the last event
      as.Date("2011-01-18")
    ),
    cohort_end_date = c(
      as.Date("2010-06-02"),
      as.Date("2011-01-14"),
      as.Date("2011-01-17"),
      as.Date("2011-01-19")
    )
  )

  db <- generate_mock_incidence_prevalence_db(outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  inc_w0 <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = 0
  )
  # expect all events if we have zero days washout
  expect_true(sum(inc_w0$n_events)==4)

  inc_w1 <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = 1
  )
    # expect three events if we have one days washout
    expect_true(sum(inc_w1$n_events)==3)

  inc_w2 <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = 2
  )
  # expect two events if we have two days washout
  expect_true(sum(inc_w2$n_events)==2)

  inc_w365 <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = 365
  )
  # expect one event if we have 365 days washout
  expect_true(sum(inc_w365$n_events)==1)

  inc_null <- get_pop_incidence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    repetitive_events = TRUE,
    outcome_washout_window = NULL
  )
  # expect one event if we have NULL (all history washout)
  expect_true(sum(inc_null$n_events)==1)

  # but, we will have move days when using the 365 day washout
  # as the person came back to contribute more time at risk
  expect_true(sum(inc_null$person_days)<sum(inc_w365$person_days))


})

test_that("mock db: compare results from months and years", {
  library(DBI)
  library(dplyr)
  library(tibble)

  person <- tibble(
    person_id = c("1","2"),
    gender_concept_id = rep("8507",2),
    year_of_birth = rep(2000,2),
    month_of_birth = rep(01,2),
    day_of_birth = rep(01,2)
  )
  observation_period <- tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = c(as.Date("2010-01-01"),
                                      as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2012-01-01"),
                                    as.Date("2012-01-01"))
  )
  outcome <- tibble(
    cohort_definition_id = c("1"),
    subject_id = c("1"),
    cohort_start_date = c(
      as.Date("2011-07-01")
    ),
    cohort_end_date = c(
      as.Date("2011-07-01")
    )
  )
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)
  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL,
    study_start_date = as.Date("2010-01-01"),
    study_end_date = as.Date("2011-12-31")
  )
  inc_months <- get_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    study_denominator_pop = dpop,
    time_interval = c("Months"),
  )
  inc_years <- get_pop_incidence(
    db = db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    study_denominator_pop = dpop,
    time_interval = c("Years"),
  )

# consistent results for months and years
# expect_true(sum(inc_months$n_events)==sum(inc_years$n_events))
# expect_equal(sum(inc_months$person_days),
#              sum(inc_years$person_days),
#              tolerance=1e-10)
# expect_equal(sum(inc_months$person_years),
#              sum(inc_years$person_years),
#              tolerance=1e-10)

DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check entry and event on same day", {

db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
person <- tibble::tibble(
person_id = "1",
gender_concept_id = "8507",
year_of_birth = 2000,
month_of_birth = 01,
day_of_birth = 01
)
observation_period <- tibble::tibble(
observation_period_id = "1",
person_id = "1",
observation_period_start_date = as.Date("2010-01-28"),
observation_period_end_date = as.Date("2012-12-31")
)
outcome <- tibble::tibble(
cohort_definition_id = "1",
subject_id = "1",
cohort_start_date = c(
as.Date("2010-01-28")
),
cohort_end_date = c(
as.Date("2010-01-28")
)
)

db <- generate_mock_incidence_prevalence_db(outcome=outcome)

dpop <- collect_denominator_pops(
db = db,
cdm_database_schema = NULL
)

inc_without_rep <- get_pop_incidence(db,
results_schema_outcome = NULL,
table_name_outcome = "outcome",
cohort_id_outcome = "1",
study_denominator_pop = dpop,
repetitive_events = TRUE,
outcome_washout_window = NULL,
time_interval = "Years"
)
expect_true(sum(inc_without_rep$n_events)==1)

inc_with_rep <- get_pop_incidence(db,
results_schema_outcome = NULL,
table_name_outcome = "outcome",
cohort_id_outcome = "1",
study_denominator_pop = dpop,
repetitive_events = TRUE,
outcome_washout_window = NULL,
time_interval = "Years"
)
expect_true(sum(inc_with_rep$n_events)==1)

})

test_that("mock db: check conversion of user inputs", {
  library(DBI)
  library(dplyr)
  library(tibble)

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

  db <- generate_mock_incidence_prevalence_db(outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  inc <- get_pop_incidence(db,
                           results_schema_outcome = NULL,
                           table_name_outcome = "outcome",
                           cohort_id_outcome = 1, # to character in function
                           study_denominator_pop = dpop
  )
  expect_true(nrow(inc)>=0)

  inc <- get_pop_incidence(db,
                           results_schema_outcome = NULL,
                           table_name_outcome = "outcome",
                           cohort_id_denominator_pop = 1, # to character in function
                           study_denominator_pop = dpop
  )
  expect_true(nrow(inc)>=0)

  inc <- get_pop_incidence(db,
                           results_schema_outcome = NULL,
                           # gets changed to NULL (to help collect)
                           outcome_washout_window = NA,
                           table_name_outcome = "outcome",
                           cohort_id_outcome = "1",
                           study_denominator_pop = dpop
  )
  expect_true(nrow(inc)>=0)


  inc <- get_pop_incidence(db,
                           results_schema_outcome = NULL,
                           table_name_outcome = "outcome",
                           # numeric id gets converted to character
                           cohort_id_outcome = 1,
                           study_denominator_pop = dpop
  )
  expect_true(nrow(inc)>=0)

  dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check messages when vebose is true", {
  library(DBI)
  library(dplyr)
  library(tibble)

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

  db <- generate_mock_incidence_prevalence_db(outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  expect_message(get_pop_incidence(db,
                         results_schema_outcome = NULL,
                         table_name_outcome = "outcome",
                         cohort_id_outcome = "1",
                         study_denominator_pop = dpop,
                         verbose = TRUE
))

  expect_message(get_pop_incidence(db,
                         results_schema_outcome = NULL,
                         table_name_outcome = "outcome",
                         cohort_id_outcome = "1",
                         time_interval = "years",
                         study_denominator_pop = dpop,
                         verbose = TRUE
))

expect_message(get_pop_incidence(db,
                         results_schema_outcome = NULL,
                         table_name_outcome = "outcome",
                         cohort_id_outcome = "1",
                         study_denominator_pop = dpop,
                         verbose = TRUE
))

dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check expected errors", {
  library(DBI)
  library(dplyr)
  library(tibble)

  db <- generate_mock_incidence_prevalence_db()

  # not a db connection
  expect_error(get_pop_incidence(db="a",
                                 results_schema_outcome = NULL,
                                 table_name_outcome = "outcome",
                                 time_interval = c("Months"),
                                 study_denominator_pop = dpop
  ))

  # no study pop
  expect_error(get_pop_incidence(db,
                    results_schema_outcome = NULL,
                    table_name_outcome = "outcome",
                    time_interval = c("Months"),
                    study_denominator_pop = dpop,
                    cohort_id_denominator_pop="999"
  ))

  # no outcomes
  expect_error(get_pop_incidence(db,
                    results_schema_outcome = NULL,
                    table_name_outcome = "outcome",
                    time_interval = c("Months"),
                    cohort_id_outcome="999",
                    study_denominator_pop = dpop
  ))


  person <- tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2010-01-05")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(as.Date("2010-01-04")),
    cohort_end_date = c(as.Date("2010-01-04"))
  )

  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period,
                                              outcome=outcome)

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

  # expect error because less than one month between
  # cohort_start_date and cohort_end_date among dpop
  expect_error(get_pop_incidence(db,
                                 results_schema_outcome = NULL,
                                 table_name_outcome = "outcome",
                                 time_interval = c("Months"),
                                 study_denominator_pop = dpop
  ))
  expect_error(get_pop_incidence(db,
                                 results_schema_outcome = NULL,
                                 table_name_outcome = "outcome",
                                 time_interval = c("Years"),
                                 cohort_id_outcome = "1",
                                 study_denominator_pop = dpop
  ))

  dbDisconnect(db)
})



# test_that("checks on working example", {
#   library(DBI)
#   library(RPostgres)
#   library(dplyr)
#   library(tibble)
#   db <- DBI::dbConnect(RPostgres::Postgres(),
#     dbname = Sys.getenv("SERVER_DBI_TEST"),
#     port = Sys.getenv("DB_PORT_TEST"),
#     host = Sys.getenv("DB_HOST_TEST"),
#     user = Sys.getenv("DB_USER_TEST"),
#     password = Sys.getenv("DB_PASSWORD_TEST")
#   )
#
#   cdm_database_schema <- "omop21t2_test"
#   results_schema_outcome <- "results21t2_test"
#   table_name_outcome <- "cohorts"
# study_pops<-collect_denominator_pops(db,
#                          cdm_database_schema,
#                          study_start_date=as.Date("2012-01-01"),
#                          study_end_date=as.Date("2014-12-31"),
#                          study_age_stratas = list(c(10,15), c(16,20), c(10,20)),
#                          study_sex_stratas = c("Male", "Female", "Both"),
#                          study_days_prior_history =c(0,365),
#                          verbose = TRUE)
#
# result <- get_pop_incidence(db=db,
#                         results_schema_outcome="results21t2_test",
#                         table_name_outcome="cohorts",
#                                     cohort_id_outcome=1,
#                                     study_denominator_pop=study_pops,
#                                     cohort_id_denominator_pop="17",
#                                     time_interval=c("Months"),
#                                     outcome_washout_window=NULL,
#                                     repetitive_events=FALSE,
#                                     verbose=FALSE)
#   # output format
#   expect_true(tibble::is_tibble(result))
#   # output variables
#   expect_true(all(c(
#     "n_persons",
#     "person_days",
#     "person_years",
#     "n_events",
#     "ir_100000_pys",
#     "ir_100000_pys_low",
#     "ir_100000_pys_high",
#     "calendar_month",
#     "calendar_year",
#     "age_strata",
#     "sex_strata"
#   ) %in%
#     names(result)))
#
#   # no missing values
#   testthat::expect_true(!is.null(result$n_persons) &
#     sum(is.na(result$n_persons)) == 0)
#   testthat::expect_true(!is.null(result$person_days) &
#     sum(is.na(result$person_days)) == 0)
#   testthat::expect_true(!is.null(result$person_years) &
#     sum(is.na(result$person_years)) == 0)
#   testthat::expect_true(!is.null(result$n_events) &
#     sum(is.na(result$n_events)) == 0)
#
#   testthat::expect_true(!is.null(result$ir_100000_pys_low) &
#     sum(is.na(result$ir_100000_pys_low)) == 0)
#   testthat::expect_true(!is.null(result$ir_100000_pys_high) &
#     sum(is.na(result$ir_100000_pys_high)) == 0)
#
#   # testthat::expect_true(!is.null(result$calendar_month) &
#   #   sum(is.na(result$calendar_month)) == 0)
#   testthat::expect_true(!is.null(result$calendar_year) &
#     sum(is.na(result$calendar_year)) == 0)
#   # testthat::expect_true(!is.null(result$strata) &
#   #   sum(is.na(result$strata)) == 0)
#   # testthat::expect_true(!is.null(result$strata_value) &
#   #   sum(is.na(result$strata_value)) == 0)
#
#   dbDisconnect(db)
#
# })

# test_that("live db expected errors", {
#   library(DBI)
#   library(RPostgres)
#   library(dplyr)
#   library(tibble)
#   db <- DBI::dbConnect(RPostgres::Postgres(),
#     dbname = Sys.getenv("SERVER_DBI_TEST"),
#     port = Sys.getenv("DB_PORT_TEST"),
#     host = Sys.getenv("DB_HOST_TEST"),
#     user = Sys.getenv("DB_USER_TEST"),
#     password = Sys.getenv("DB_PASSWORD_TEST")
#   )
#
#   cdm_database_schema <- "omop21t2_test"
#   results_schema_outcome <- "results21t2_test"
#   table_name_outcome <- "cohorts"
# study_pops<-collect_denominator_pops(db,
#                          cdm_database_schema,
#                          study_start_date=as.Date("2017-01-01"),
#                          study_end_date=as.Date("2018-12-31"))
# expect_error(get_pop_incidence(db="a",
#                         results_schema_outcome="results21t2_test",
#                         table_name_outcome="cohorts",
#                         cohort_id_outcome=1,
#                         study_denominator_pop=study_pops,
#                         verbose=TRUE))
# expect_error(get_pop_incidence(db=db,
#                         results_schema_outcome="results21t2_test",
#                         table_name_outcome="cohorts",
#                         cohort_id_outcome=1,
#                         study_denominator_pop=study_pops %>% filter(person_id==0),
#                         verbose=TRUE))
# expect_error(get_pop_incidence(db=db,
#                         results_schema_outcome="results21t2_test",
#                         table_name_outcome="cohorts",
#                         cohort_id_outcome=3,
#                         study_denominator_pop=study_pops %>% filter(person_id==0),
#                         verbose=TRUE))
#
# expect_error(get_pop_incidence(db=db,
#                         results_schema_outcome="results21t2_test",
#                         table_name_outcome="cohorts",
#                         cohort_id_denominator_pop=3,
#                         study_denominator_pop=study_pops,
#                         verbose=TRUE))
# expect_error(get_pop_incidence(db=db,
#                         results_schema_outcome="results21t2_test",
#                         table_name_outcome="cohorts",
#                         cohort_id_outcome=10,
#                         study_denominator_pop=study_pops))
#
#   dbDisconnect(db)
#
# })
