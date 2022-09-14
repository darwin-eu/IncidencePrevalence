
test_that("mock db: check output format", {
  db <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )

    expect_true(all(c(
      "cohort_definition_id",
      "person_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in%
      names(dpop$denominator_populations)))

    expect_true(all(c(
      "cohort_definition_id",
      "study_start_date",
      "study_end_date",
      "age_strata",
      "sex_strata",
      "required_days_prior_history"
    ) %in%
      names(dpop$denominator_settings)))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: checks on working example", {
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
    observation_period_end_date = as.Date("2012-06-01")
  )

  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)
  # some pops with people, but some without
  dpops <- collect_denominator_pops(db,
    cdm_database_schema = NULL,
    study_start_date = NULL,
    study_end_date = NULL,
    study_age_stratas = list(c(0, 59), c(60, 69)),
    study_sex_stratas = c("Female", "Male", "Both"),
    verbose = TRUE
  )
  expect_true(nrow(dpops$denominator_populations)>=1)

  # all pops without anyone
  expect_message(dpops <- collect_denominator_pops(db,
    cdm_database_schema = NULL,
    study_start_date = NULL,
    study_end_date = NULL,
    study_age_stratas = list(c(50, 59), c(60, 69)),
    study_days_prior_history = c(0, 365)
  ))
  expect_true(nrow(dpops$denominator_population)==0)


  # using cohort strata
  # add stratifying cohort
  strata_cohort<-  tibble(
    cohort_definition_id="1",
    subject_id=c("1","2"),
    cohort_start_date=as.Date("2010-03-15"),
    cohort_end_date=as.Date("2012-03-15")
  )
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "strata_cohort",
                      strata_cohort,
                      overwrite = TRUE
    )})

  # using strata cohort
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    strata_schema =  NULL,
    table_name_strata = "strata_cohort",
    strata_cohort_id = "1"
  )
  expect_true(dpop$denominator_population$cohort_start_date ==
                "2010-03-15")
  expect_true(dpop$denominator_population$cohort_end_date ==
                "2012-03-15")

  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db check age strata entry and exit", {
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
    observation_period_start_date = as.Date("2008-01-01"),
    observation_period_end_date = as.Date("2018-06-01")
  )
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

  # if we have two age groups 1) 11 to 12, and 2) 13 to 14
  # we expect the person to be in the first cohort up
  # to the day before their 13th birthday
  # and in the second from their 13th birthday
  # up to the day before their 15th birthday
dpops <- collect_denominator_pops(db = db,
cdm_database_schema = NULL,
study_age_stratas = list(c(11, 12),
                         c(13,14))
)
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id==1) %>%
  dplyr::select(cohort_start_date) %>%
  dplyr::pull() == as.Date("2011-01-01"))
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id==1) %>%
  dplyr::select(cohort_end_date) %>%
  dplyr::pull() == as.Date("2012-12-31"))
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id==2) %>%
  dplyr::select(cohort_start_date) %>%
  dplyr::pull() == as.Date("2013-01-01"))
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id==2) %>%
  dplyr::select(cohort_end_date) %>%
  dplyr::pull() == as.Date("2014-12-31"))

  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check expected errors", {
  db <- generate_mock_incidence_prevalence_db()



# not a dbi connection
expect_error(collect_denominator_pops(db="a",
                                      cdm_database_schema,
                                      study_start_date = NULL,
                                      study_end_date = NULL,
                                      study_age_stratas = list(c(10, 15), c(16, 20)),
                                      study_sex_stratas = c("Female", "Male", "Both"),
                                      study_days_prior_history = c(0, 365)
))


# not an availabe study_sex_stratas
expect_error(collect_denominator_pops(db,
                                      cdm_database_schema = NULL,
                                      study_sex_stratas = "Men"
))



DBI::dbDisconnect(db, shutdown=TRUE)

})



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

