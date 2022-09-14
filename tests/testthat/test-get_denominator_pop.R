test_that("mock db: check output format", {
  # mock database
  db <- generate_mock_incidence_prevalence_db()

  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )

    # variable names
    expect_true(length(names(dpop$denominator_population))==3)
    expect_true(all(c(
      "person_id",
      "cohort_start_date", "cohort_end_date"
    ) %in%
      names(dpop$denominator_population)))
    # types
    expect_true(class(dpop$denominator_population$person_id) == "character")
    expect_true(class(dpop$denominator_population$cohort_start_date) == "Date")
    expect_true(class(dpop$denominator_population$cohort_end_date) == "Date")

    # no missing values
     testthat::expect_true(!is.null(dpop$denominator_population$person_id) &
         (sum(is.na(dpop$denominator_population$person_id)) == 0))
     testthat::expect_true(!is.null(dpop$denominator_population$cohort_start_date) &
         (sum(is.na(dpop$denominator_population$cohort_start_date)) == 0))
     testthat::expect_true(!is.null(dpop$denominator_population$cohort_end_date) &
         (sum(is.na(dpop$denominator_population$cohort_end_date)) == 0))

     # check verbose
     expect_message(get_denominator_pop(
       db = db,
       cdm_database_schema = NULL,
       verbose = TRUE
     ))

     DBI::dbDisconnect(db, shutdown=TRUE)

  })

test_that("mock db: check example we expect to work", {
  # one person, one observation periods
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )
  expect_true(nrow(dpop$denominator_population) == 1)
  expect_true(dpop$denominator_population$cohort_start_date == as.Date("2010-01-01"))
  expect_true(dpop$denominator_population$cohort_end_date == as.Date("2015-06-01"))


  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    start_date = as.Date("2010-02-15"),
    end_date = as.Date("2010-05-15")
  )
  expect_true(nrow(dpop$denominator_population) == 1)
  expect_true(dpop$denominator_population$cohort_start_date == as.Date("2010-02-15"))
  expect_true(dpop$denominator_population$cohort_end_date == as.Date("2010-05-15"))

  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check example with multiple observation periods", {
  # one person, two observation periods
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = rep("1", 2),
    observation_period_start_date = c(as.Date("2010-01-01"),
                                      as.Date("2011-01-01")),
    observation_period_end_date = c(as.Date("2010-06-01"),
                                    as.Date("2011-06-01"))
  )
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

  # expect two rows
  # one per observation period
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )
  expect_true(nrow(dpop$denominator_population) == 2)

  # expect one rows- if start date is 1st Jan 2011
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    start_date = as.Date("2011-01-01")
  )
  expect_true(nrow(dpop$denominator_population) == 1)
  expect_true(dpop$denominator_population$cohort_start_date == as.Date("2011-01-01"))
  expect_true(dpop$denominator_population$cohort_end_date == as.Date("2011-06-01"))

  # expect one rows- if start date is end of 2020
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    end_date = as.Date("2010-12-31")
  )
  expect_true(nrow(dpop$denominator_population) == 1)
  expect_true(dpop$denominator_population$cohort_start_date == as.Date("2010-01-01"))
  expect_true(dpop$denominator_population$cohort_end_date == as.Date("2010-06-01"))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check example with restriction on sex", {
  # two male, one female
  person <- tibble::tibble(
    person_id = c("1","2","3"),
    gender_concept_id = c("8507","8507", "8532"),
    year_of_birth = rep(2000,3),
    month_of_birth = rep(06,3),
    day_of_birth = rep(01,3)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3"),
    person_id = c("1","2","3"),
    observation_period_start_date = rep(as.Date("2010-01-01"),3),
    observation_period_end_date = rep(as.Date("2015-06-01"),3)
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)


dpop1 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  sex = "Male"
)
dpop2 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  sex = "Both"
)
dpop3 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  sex = "Female"
)
expect_true(nrow(dpop1$denominator_population) == 2)
expect_true(nrow(dpop2$denominator_population) == 3)
expect_true(nrow(dpop3$denominator_population) == 1)


# one male only
person <- tibble::tibble(
  person_id = "1",
  gender_concept_id = "8507",
  year_of_birth = 2000,
  month_of_birth = 06,
  day_of_birth = 01)
observation_period <- tibble::tibble(
  observation_period_id = "1",
  person_id = "1",
  observation_period_start_date = as.Date("2010-01-01"),
  observation_period_end_date = as.Date("2015-06-01")
)
# mock database
db <- generate_mock_incidence_prevalence_db(person=person,
                                            observation_period=observation_period)

dpop1 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  sex = "Male"
)
dpop2 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  sex = "Both"
)
dpop3 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  sex = "Female"
)
expect_true(nrow(dpop1$denominator_population) == 1)
expect_true(nrow(dpop2$denominator_population) == 1)
expect_true(is.null(dpop3$denominator_population))

DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check example with restriction on age", {
  # three people, born in 2000, 2005, and 2010
  person <- tibble::tibble(
    person_id = c("1","2","3"),
    gender_concept_id = rep("8507",3),
    year_of_birth = c(2000,2005, 2010),
    month_of_birth = rep(06,3),
    day_of_birth = rep(01,3)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3"),
    person_id = c("1","2","3"),
    observation_period_start_date = rep(as.Date("2010-01-01"),3),
    observation_period_end_date = rep(as.Date("2015-06-01"),3)
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

# check min_age
dpop1 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 0
)
dpop2 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 8
)
dpop3 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 12
)
dpop4 <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 40
)

expect_true(nrow(dpop1$denominator_population) == 3)
expect_true(nrow(dpop2$denominator_population) == 2)
expect_true(nrow(dpop3$denominator_population) == 1)
expect_true(is.null(dpop4$denominator_population))



# one person, born in 2000
person <- tibble::tibble(
  person_id = "1",
  gender_concept_id = "8507",
  year_of_birth = 2000,
  month_of_birth = 06,
  day_of_birth = 01)
observation_period <- tibble::tibble(
  observation_period_id = "1",
  person_id = "1",
  observation_period_start_date = as.Date("2010-01-01"),
  observation_period_end_date = as.Date("2015-06-01"))

# mock database
db <- generate_mock_incidence_prevalence_db(person, observation_period)

# entry once they reach the min age criteria
dpop <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 10
)
# start date is now date of 10th birthday
expect_true(dpop$denominator_population$cohort_start_date == as.Date("2010-06-01"))


# exit once they reach the max age criteria
dpop <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  max_age = 10
)
# end date is the day before their 11th birthday
expect_true(dpop$denominator_population$cohort_end_date==as.Date("2011-05-31"))

DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check imputation of date of birth", {
# one person with all info, one missing month, one missing day, and one both
person <- tibble::tibble(
  person_id = c("1","2","3","4"),
  gender_concept_id = rep("8507",4),
  year_of_birth = rep(2000,4),
  month_of_birth = c(03,NA,03, NA),
  day_of_birth = c(03, 03, NA, NA))
observation_period <- tibble::tibble(
  observation_period_id = c("1","2","3","4"),
  person_id = c("1","2","3","4"),
  observation_period_start_date = rep(as.Date("2010-01-01"),4),
  observation_period_end_date = rep(as.Date("2015-06-01"),4)
)

db <- generate_mock_incidence_prevalence_db(person=person,
                                            observation_period=observation_period)

dpop <- get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 10
)
expect_true(nrow(dpop$denominator_population) == 4)

expect_true(dpop$denominator_population %>%
              dplyr::filter(person_id=="1") %>%
              dplyr::summarise(check=cohort_start_date==as.Date("2010-03-03")) %>%
              dplyr::pull())
expect_true(dpop$denominator_population %>%
              dplyr::filter(person_id=="2") %>%
              dplyr::summarise(check=cohort_start_date==as.Date("2010-01-03")) %>%
              dplyr::pull())
expect_true(dpop$denominator_population %>%
              dplyr::filter(person_id=="3") %>%
              dplyr::summarise(check=cohort_start_date==as.Date("2010-03-01")) %>%
              dplyr::pull())
expect_true(dpop$denominator_population %>%
              dplyr::filter(person_id=="4") %>%
              dplyr::summarise(check=cohort_start_date==as.Date("2010-01-01")) %>%
              dplyr::pull())

DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check edge cases (zero results expected)", {
  # one person, one observation periods
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

 dpop<- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    start_date = as.Date("2100-01-01")
  )
expect_true(is.null(dpop$denominator_population))

dpop<-get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  end_date = as.Date("1800-01-01")
)
expect_true(is.null(dpop$denominator_population))

dpop<-get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  min_age = 155
)
expect_true(is.null(dpop$denominator_population))

# note could include people as it would go up to day before first birthday
# but given observation period, here we would expect a null
dpop<-get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  max_age = 0
)
expect_true(is.null(dpop$denominator_population))

dpop<-get_denominator_pop(
  db = db,
  cdm_database_schema = NULL,
  max_age = 15,
  days_prior_history = 365*50,
  verbose = FALSE
)
expect_true(is.null(dpop$denominator_population))

DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: expected errors", {
  # one person, one observation periods
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person, observation_period)

  # not a dbi connection
  testthat::expect_error(get_denominator_pop(
    db = "a",
    cdm_database_schema = NULL,
    verbose = FALSE))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    min_age = c(0,1),
    verbose = FALSE))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    max_age = c(100,110),
    verbose = FALSE))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    sex = "Men",
    verbose = FALSE))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    sex = c("Male","Female"),
    verbose = FALSE))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    days_prior_history = -30,
    verbose = FALSE))

  testthat::expect_error(get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    min_age = -5,
    verbose = FALSE))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: check example #2 we expect to work", {
  # 5 person, 1 observation periods
  person <- tibble::tibble(
    person_id = c("1","2","3","4","5"),
    gender_concept_id = c("8507","8532","8507", "8532","8532"),
    year_of_birth = c(1995,1993,1994,1996,NA),
    month_of_birth = c(07,NA,06,05,04),
    day_of_birth = c(25,NA,01,02,03)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3","4","5"),
    person_id = c("1","2","3","4","5"),
    observation_period_start_date = rep(as.Date("2000-01-01"),5),
    observation_period_end_date = rep(as.Date("2015-06-01"),5)
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )

  #throw year NA row
  expect_true(nrow(dpop$denominator_population) == 4)
  expect_true(all(dpop$denominator_population$cohort_start_date == as.Date("2000-01-01")))
  expect_true(all(dpop$denominator_population$cohort_end_date == as.Date("2015-06-01")))




  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    min_age = 10
  )
  #check min age change cohort start date
  #check imputation
  dpop$denominator_population %>%
    dplyr::filter(person_id=="1") %>%
    dplyr::summarise(check=cohort_start_date==as.Date("2005-07-25"))
  dpop$denominator_population %>%
    dplyr::filter(person_id=="2") %>%
    dplyr::summarise(check=cohort_start_date==as.Date("2003-01-01"))
  dpop$denominator_population %>%
    dplyr::filter(person_id=="3") %>%
    dplyr::summarise(check=cohort_start_date==as.Date("2004-06-01"))
  dpop$denominator_population %>%
    dplyr::filter(person_id=="4") %>%
    dplyr::summarise(check=cohort_start_date==as.Date("2006-05-02"))

  #check max age change cohort start date
  #check imputation
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    max_age = 10
  )
  dpop$denominator_population %>%
    dplyr::filter(person_id=="1") %>%
    dplyr::summarise(check=cohort_end_date==as.Date("2006-07-24"))
  dpop$denominator_population %>%
    dplyr::filter(person_id=="2") %>%
    dplyr::summarise(check=cohort_end_date==as.Date("2003-12-31"))
  dpop$denominator_population %>%
    dplyr::filter(person_id=="3") %>%
    dplyr::summarise(check=cohort_end_date==as.Date("2005-05-31"))
  dpop$denominator_population %>%
    dplyr::filter(person_id=="4") %>%
    dplyr::summarise(check=cohort_end_date==as.Date("2007-05-01"))

  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    start_date = as.Date("2010-02-15"),
    end_date = as.Date("2010-05-15")
  )
  expect_true(nrow(dpop$denominator_population) == 4)
  expect_true(all(dpop$denominator_population$cohort_start_date == as.Date("2010-02-15")))
  expect_true(all(dpop$denominator_population$cohort_end_date == as.Date("2010-05-15")))

  DBI::dbDisconnect(db, shutdown=TRUE)
})

test_that("mock db: check attrition table", {
  # 7 person, 1 observation periods
  person <- tibble::tibble(
    person_id = c("1","2","3","4","5","6","7"),
    gender_concept_id = c("8507","8532","8507", "8532","8532", "8507", NA),
    year_of_birth = c(1995,1993,1994,1996,1998, NA, 1993),
    month_of_birth = c(07,02,06,05,04,10,01),
    day_of_birth = c(25,14,01,02,03,10,12)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3","4","5","6","7"),
    person_id = c("1","2","3","4","5","6","7"),
    observation_period_start_date = c(as.Date("2017-01-01"),rep(as.Date("2000-01-01"),3),rep(as.Date("2016-01-01"),3)),
    observation_period_end_date = c(as.Date("2020-06-01"), rep(as.Date("2017-06-01"),3),rep(as.Date("2020-06-01"),3))
  )

  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)
  # check number of rows
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )
  expect_true(nrow(dpop$attrition) == 7)

  # check last n_current equals the number of rows of the denominator pop
  expect_true(nrow(dpop$denominator_population)==dpop$attrition$current_n[7])

  # check names
  expect_true(all(c(NA, "Missing year of birth",
                    "Missing gender",
                    "Doesn't satisfy the sex criteria",
                    "No observation time available during study period",
                    "Doesn't satisfy age criteria during the study period" ,
                    "Prior history requirement not fullfilled during study period"
          ) %in%
        dpop$attrition$reason))


  # check missings
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )
  expect_true(dpop$attrition$excluded[2]== 1)
  expect_true(dpop$attrition$excluded[3]== 1)

  # check sex criteria
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    sex= "Female"
  )
  expect_true(dpop$attrition$excluded[4] == 2)

  # check age criteria
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    min_age = 24,
    max_age = 25
  )
  expect_true(dpop$attrition$excluded[6] == 1)

  # check observation criteria
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    start_date = as.Date("2010-01-01"),
    end_date = as.Date("2012-01-01")
  )
  expect_true(dpop$attrition$excluded[5] == 2)

  # check prior observation criteria
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    start_date = as.Date("2015-01-01"),
    end_date = as.Date("2016-06-30"),
    days_prior_history = 365
  )
  expect_true(dpop$attrition$excluded[7] == 1)

  # multiple observation periods per person
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3"),
    person_id = c("1"),
    observation_period_start_date = c(as.Date("2008-01-01"),
                                      as.Date("2009-01-01"),
                                      as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2008-06-01"),
                                    as.Date("2009-06-01"),
                                    as.Date("2010-06-01"))
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL)
 expect_true(all(dpop$attrition$current_n == 1))

  DBI::dbDisconnect(db, shutdown=TRUE)

})

test_that("mock db: subset denominator by cohort", {
  # one person, one observation periods
  person <- tibble::tibble(
    person_id = c("1","2","3"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3"),
    person_id = c("1","2","3"),
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

  # add stratifying cohort
  strata_cohort<-  tibble::tibble(
    cohort_definition_id="1",
      subject_id=c("1","2"),
    cohort_start_date=as.Date("2012-06-06"),
    cohort_end_date=as.Date("2013-06-06")
  )
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "strata_cohort",
                      strata_cohort,
                      overwrite = TRUE
    )})

  # without using strata cohort
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL
  )
  expect_true(all(dpop$denominator_population$person_id %in%
                    c("1", "2", "3")))
  expect_true(all(dpop$denominator_population$cohort_start_date ==
                    "2010-01-01"))
  expect_true(all(dpop$denominator_population$cohort_end_date ==
                    "2015-06-01"))

  # using strata cohort
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    strata_schema = NULL,
    table_name_strata = "strata_cohort",
    strata_cohort_id = "1",
  )
  expect_true(all(dpop$denominator_population$person_id %in%
                    c("1", "2")))
  expect_true(all(!dpop$denominator_population$person_id  %in%
                    c("3")))
  expect_true(all(dpop$denominator_population$cohort_start_date ==
                    "2012-06-06"))
  expect_true(all(dpop$denominator_population$cohort_end_date ==
                    "2013-06-06"))


  # stratifying cohort multiple events per person
  strata_cohort<-  tibble::tibble(
    cohort_definition_id="1",
    subject_id=c("1","2","2"),
    cohort_start_date=c(as.Date("2012-06-06"),
                        as.Date("2012-06-06"),
                        as.Date("2013-11-01")),
    cohort_end_date=c(as.Date("2013-06-06"),
                      as.Date("2013-06-06"),
                      as.Date("2014-02-01"))
  )
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "strata_cohort",
                      strata_cohort,
                      overwrite = TRUE
    )})
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    strata_schema = NULL,
    table_name_strata = "strata_cohort",
    strata_cohort_id = "1",
  )
  expect_true(all(dpop$denominator_population$person_id %in%
                    c("1", "2")))
  expect_true(all(!dpop$denominator_population$person_id  %in%
                    c("3")))
  expect_true(sum(dpop$denominator_population$person_id=="1")==1)
  expect_true(sum(dpop$denominator_population$person_id=="2")==2)

  expect_true(all(dpop$denominator_population$cohort_start_date %in%
                    as.Date(c("2012-06-06", "2013-11-01"))))
  expect_true(all(dpop$denominator_population$cohort_end_date %in%
                    as.Date(c("2013-06-06", "2014-02-01"))))


  # multiple observation periods and multiple outcomes for a person
  # one person, one observation periods
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2","3"),
    person_id = c("1"),
    observation_period_start_date = c(as.Date("2008-01-01"),
                                      as.Date("2009-01-01"),
                                      as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2008-06-01"),
                                    as.Date("2009-06-01"),
                                    as.Date("2010-06-01"))
  )
  # mock database
  db <- generate_mock_incidence_prevalence_db(person=person,
                                              observation_period=observation_period)

  # add stratifying cohort
  strata_cohort<-  tibble::tibble(
    cohort_definition_id="1",
    subject_id=c("1","1","1"),
    cohort_start_date=c(as.Date("2008-02-01"),
                        as.Date("2009-02-01"),
                        as.Date("2010-02-01")),
    cohort_end_date=c(as.Date("2008-04-01"),
                      as.Date("2009-04-01"),
                      as.Date("2010-04-01"))
  )
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "strata_cohort",
                      strata_cohort,
                      overwrite = TRUE
    )})
  dpop <- get_denominator_pop(
    db = db,
    cdm_database_schema = NULL,
    strata_schema = NULL,
    table_name_strata = "strata_cohort",
    strata_cohort_id = "1",
  )
  expect_true(sum(dpop$denominator_population$person_id=="1")==3)
  expect_true(dpop$denominator_population$cohort_start_date[1]=="2008-02-01")
  expect_true(dpop$denominator_population$cohort_start_date[2]=="2009-02-01")
  expect_true(dpop$denominator_population$cohort_start_date[3]=="2010-02-01")

  expect_true(dpop$denominator_population$cohort_end_date[1]=="2008-04-01")
  expect_true(dpop$denominator_population$cohort_end_date[2]=="2009-04-01")
  expect_true(dpop$denominator_population$cohort_end_date[3]=="2010-04-01")


  DBI::dbDisconnect(db, shutdown=TRUE)

})


# test_that("various checks for working example full db", {
# # full database
#   library(DBI)
#   library(dplyr)
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
#   result <- get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   )
#
#   # variable names
#   expect_true(all(c(
#     "person_id",
#     "cohort_start_date", "cohort_end_date"
#   ) %in%
#     names(result)))
#
#   # no missing values
#   testthat::expect_true(!is.null(result$person_id) &
#     sum(is.na(result$person_id)) == 0)
#   testthat::expect_true(!is.null(result$cohort_start_date) &
#     sum(is.na(result$cohort_start_date)) == 0)
#   testthat::expect_true(!is.null(result$cohort_end_date) &
#     sum(is.na(result$cohort_end_date)) == 0)
#
#   # end date after start date
#   testthat::expect_true(all(result$cohort_start_date <=
#     result$cohort_end_date) == TRUE)
#
#   DBI::dbDisconnect(db)
# })
#
# test_that("expected errors", {
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
#   testthat::expect_error(get_denominator_pop(
#     db = "a",
#     cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   # missing cdm_database_schema
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = NULL,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   # start_date not a date
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = "a",
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   # end_date not a date
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = "a",
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   # min_age not one number
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = "a",
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = c(10, 15),
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   # max_age not one number
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = "a",
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = c(10, 15),
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#
#   # sex not only one of the options
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Male", "Female"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("women"),
#     days_prior_history = 0,
#     verbose = FALSE
#   ))
#
#   # days_prior_history not a number
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = "a",
#     verbose = FALSE
#   ))
#
#   # days_prior_history should not be a negative number
#   testthat::expect_error(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = -365,
#     verbose = FALSE
#   ))
#
#   DBI::dbDisconnect(db)
# })
#
# test_that("edge cases where NULL result should be returned", {
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
#   expect_true(is.null(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = as.Date("2100-01-01"),
#     end_date = NULL,
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   )))
#
#   expect_true(is.null(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = as.Date("1800-01-01"),
#     min_age = NULL,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   )))
#
#   expect_true(is.null(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = 155,
#     max_age = NULL,
#     sex = c("Both"),
#     days_prior_history = 0,
#     verbose = FALSE
#   )))
#
#
#   expect_true(is.null(get_denominator_pop(
#     db = db,
#     cdm_database_schema = cdm_database_schema,
#     start_date = NULL,
#     end_date = NULL,
#     min_age = NULL,
#     max_age = 1,
#     sex = c("Both"),
#     days_prior_history = 1000,
#     verbose = FALSE
#   )))
#
#
#   DBI::dbDisconnect(db)
# })
