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
#   results_schema_outcome <- "results21t2_cmbd_test"
#
#   working_denominator_pop <- collect_denominator_pops(db,
#     cdm_database_schema,
#     study_start_date = NULL,
#     study_end_date = NULL,
#     age_groups = list(c(10, 15)),
#     sex = "Male",
#     days_prior_history = 365
#   )
#   result <- calculate_pop_incidence(
#     db = db,
#     results_schema_outcome = "results_schema_outcome",
#     table_name_outcome = "results_table_name",
#     cohort_id_outcome = 1,
#     study_denominator_pop = working_denominator_pop,
#     cohort_id_denominator_pop = NULL,
#     time_interval = c("Months"),
#     prior_event_lookback = NULL,
#     repetitive_events = FALSE,
#     confidence_intervals = "exact",
#     verbose = FALSE
#   )
#   # output format
#   expect_true(tibble::is_tibble(result))
#   # output variables
#   expect_true(all(c(
#     "n_persons",
#     "person_days",
#     "person_months",
#     "person_years",
#     "n_events",
#     "ir",
#     "ir_low",
#     "ir_high",
#     "calendar_month",
#     "calendar_year",
#     "min_age",
#     "max_age",
#     "sex",
#     "pop_strata",
#     "pop_strata_value"
#   ) %in%
#     names(result)))
#
#   # no missing values
#   testthat::expect_true(!is.null(result$n) &
#     sum(is.na(result$n)) == 0)
#   testthat::expect_true(!is.null(result$days) &
#     sum(is.na(result$days)) == 0)
#   testthat::expect_true(!is.null(result$months) &
#     sum(is.na(result$months)) == 0)
#   testthat::expect_true(!is.null(result$years) &
#     sum(is.na(result$years)) == 0)
#   testthat::expect_true(!is.null(result$events) &
#     sum(is.na(result$events)) == 0)
#   testthat::expect_true(!is.null(result$ci_low) &
#     sum(is.na(result$ci_low)) == 0)
#   testthat::expect_true(!is.null(result$ci_high) &
#     sum(is.na(result$ci_high)) == 0)
#   testthat::expect_true(!is.null(result$month) &
#     sum(is.na(result$month)) == 0)
#   testthat::expect_true(!is.null(result$year) &
#     sum(is.na(result$year)) == 0)
#   testthat::expect_true(!is.null(result$strata) &
#     sum(is.na(result$strata)) == 0)
#   testthat::expect_true(!is.null(result$strata_value) &
#     sum(is.na(result$strata_value)) == 0)
# })
