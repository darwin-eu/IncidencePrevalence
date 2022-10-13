
test_that("mock db: check output format", {
  cdm <- generate_mock_incidence_prevalence_db()

  dpop <- collect_denominator_pops(cdm = cdm)

  expect_true(all(c(
      "cohort_definition_id",
      "subject_id",
      "cohort_start_date",
      "cohort_end_date"
    ) %in%
      names(dpop$denominator_populations %>% dplyr::collect())))

    expect_true(all(c(
      "age_strata","min_age","max_age",
      "sex_strata",
      "study_start_date",
      "study_end_date",
      "study_days_prior_history",
      "cohort_definition_id"
    ) %in%
      names(dpop$denominator_settings)))

    # variable names
    expect_true(length(names(dpop$denominator_population %>% dplyr::collect())) == 4)
    expect_true(all(c(
      "cohort_definition_id", "subject_id",
      "cohort_start_date", "cohort_end_date"
    ) %in%
      names(dpop$denominator_population %>% dplyr::collect())))
    # types
    expect_true(class(dpop$denominator_population %>%
                        dplyr::collect() %>% dplyr::select(cohort_definition_id) %>%
                        dplyr::pull()) == "character")
    expect_true(class(dpop$denominator_population %>%
                        dplyr::collect() %>% dplyr::select(subject_id) %>%
                        dplyr::pull()) == "character")
    expect_true(class(dpop$denominator_population %>%
                        dplyr::collect() %>% dplyr::select(cohort_start_date) %>%
                        dplyr::pull())== "Date")
    expect_true(class(dpop$denominator_population %>%
                        dplyr::collect() %>% dplyr::select(cohort_end_date) %>%
                        dplyr::pull())== "Date")

    # check verbose
    expect_message(collect_denominator_pops(
      cdm = cdm,
      verbose = TRUE
    ))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                               observation_period = observation_period)
  # some pops with people, but some without
  dpops <- collect_denominator_pops(cdm,
                                    study_start_date = NULL,
                                    study_end_date = NULL,
                                    study_age_stratas = list(c(0, 59), c(60, 69)),
                                    study_sex_stratas = c("Female", "Male", "Both"),
                                    verbose = TRUE
  )
  expect_true(dpops$denominator_populations %>% dplyr::count() %>% dplyr::pull() >= 1)

  # all pops without anyone
  expect_message(dpops <- collect_denominator_pops(cdm,
                                                   study_start_date = NULL,
                                                   study_end_date = NULL,
                                                   study_age_stratas = list(c(50, 59), c(60, 69)),
                                                   study_days_prior_history = c(0, 365)
  ))
  expect_true(dpops$denominator_populations %>% dplyr::count() %>% dplyr::pull() == 0)


  # using cohort strata
  # add stratifying cohort
  strata <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1","2"),
    cohort_start_date = as.Date("2010-03-15"),
    cohort_end_date = as.Date("2012-03-15")
  )
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   strata = strata)

  # using strata cohort
  dpop <- collect_denominator_pops(
    cdm = cdm,
    table_name_strata = "strata",
    strata_cohort_id = "1"
  )
  expect_true(dpop$denominator_population %>%
                dplyr::select(cohort_start_date) %>% dplyr::pull() ==
                "2010-03-15")
  expect_true(dpop$denominator_population %>%
                dplyr::select(cohort_end_date) %>% dplyr::pull() ==
                "2012-03-15")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  dpop <- collect_denominator_pops(cdm = cdm)
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 1)
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2010-01-01"))
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2015-06-01"))


  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2010-02-15"),
    study_end_date = as.Date("2010-05-15")
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 1)
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2010-02-15"))
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2010-05-15"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check another example we expect to work", {
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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  dpop <- collect_denominator_pops(cdm = cdm)

  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 4)
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2000-01-01")))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2015-06-01")))


  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(10,100))
  )
  #check min age change cohort start date
  #check imputation
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "1") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2005-07-25")) %>%
    dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "2") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2003-01-01")) %>%
      dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "3") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2004-06-01")) %>%
      dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "4") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2006-05-02")) %>%
      dplyr::pull())

  #check max age change cohort start date
  #check imputation
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(0,10))
  )
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "1") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2006-07-24")) %>%
      dplyr::pull())
   expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "2") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2003-12-31")) %>%
      dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "3") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2005-05-31")) %>%
      dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
    dplyr::filter(subject_id == "4") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2007-05-01")) %>%
      dplyr::pull())

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2010-02-15"),
    study_end_date = as.Date("2010-05-15")
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 4)
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2010-02-15")))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2010-05-15")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: mock example 10000", {
  cdm <- generate_mock_incidence_prevalence_db(sample_size = 10000)
  # all options being used except study start and end
  dpops <- collect_denominator_pops(cdm,
                                    study_start_date = NULL,
                                    study_end_date = NULL,
                                    study_age_stratas = list(c(0, 59), c(60, 69)),
                                    study_sex_stratas = c("Female", "Male", "Both"),
                                    study_days_prior_history = c(0,180),
                                    verbose = TRUE
  )
  expect_true(nrow(dpops$denominator_population %>% dplyr::collect())>0)

  # all options being used
  dpops <- collect_denominator_pops(cdm,
                                    study_start_date = as.Date("2011-01-01"),
                                    study_end_date = as.Date("2013-06-15"),
                                    study_age_stratas = list(c(0, 59), c(60, 69)),
                                    study_sex_stratas = c("Female", "Male", "Both"),
                                    study_days_prior_history = c(0,180),
                                    verbose = TRUE
  )
  expect_true(nrow(dpops$denominator_population %>% dplyr::collect())>0)
  expect_true(min(dpops$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date)) >=
                as.Date("2011-01-01"))
  expect_true(max(dpops$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date)) <=
                as.Date("2013-06-15"))

  # with sampling
  dpops <- collect_denominator_pops(cdm,
                                    sample = 55)
  expect_true(nrow(dpops$denominator_populations %>% dplyr::collect())==55)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check subgroups", {
  cdm <- generate_mock_incidence_prevalence_db(sample_size = 100)

  # all being used
  dpops_combine_subgroups <- collect_denominator_pops(cdm,
                                    study_start_date = NULL,
                                    study_end_date = NULL,
                                    study_age_stratas = list(c(0, 59), c(60, 69)),
                                    study_sex_stratas = c("Female", "Male", "Both"),
                                    study_days_prior_history = c(0,180),
                                    combine_subgroups = TRUE
  )
  dpops_independent_subgroups <- collect_denominator_pops(cdm,
                                                      study_start_date = NULL,
                                                      study_end_date = NULL,
                                                      study_age_stratas = list(c(0, 59), c(60, 69)),
                                                      study_sex_stratas = c("Female", "Male", "Both"),
                                                      study_days_prior_history = c(0,180),
                                                      combine_subgroups = FALSE
  )

  expect_true(length(dpops_combine_subgroups$denominator_populations %>%
    dplyr::select("cohort_definition_id") %>%
    dplyr::distinct() %>%
    dplyr::collect()%>%
    dplyr::pull()) == 12)
  expect_true(nrow(dpops_combine_subgroups$denominator_settings) == 12)

  expect_true(length(dpops_independent_subgroups$denominator_populations %>%
                       dplyr::select("cohort_definition_id") %>%
                       dplyr::distinct() %>%
                       dplyr::collect()%>%
                       dplyr::pull()) == 6)
  expect_true(nrow(dpops_independent_subgroups$denominator_settings) == 6)



  # age and sex
  dpops_combine_subgroups <- collect_denominator_pops(cdm,
                                                      study_start_date = NULL,
                                                      study_end_date = NULL,
                                                      study_age_stratas = list(c(0, 59), c(60, 69)),
                                                      study_sex_stratas = c("Female", "Male", "Both"),
                                                      combine_subgroups = TRUE
  )
  dpops_independent_subgroups <- collect_denominator_pops(cdm,
                                                          study_start_date = NULL,
                                                          study_end_date = NULL,
                                                          study_age_stratas = list(c(0, 59), c(60, 69)),
                                                          study_sex_stratas = c("Female", "Male", "Both"),
                                                          combine_subgroups = FALSE
  )

  expect_true(length(dpops_combine_subgroups$denominator_populations %>%
                       dplyr::select("cohort_definition_id") %>%
                       dplyr::distinct() %>%
                       dplyr::collect()%>%
                       dplyr::pull()) == 6)
  expect_true(nrow(dpops_combine_subgroups$denominator_settings) == 6)

  expect_true(length(dpops_independent_subgroups$denominator_populations %>%
                       dplyr::select("cohort_definition_id") %>%
                       dplyr::distinct() %>%
                       dplyr::collect()%>%
                       dplyr::pull()) == 5)
  expect_true(nrow(dpops_independent_subgroups$denominator_settings) == 5)



  # sex and prior history
  dpops_combine_subgroups <- collect_denominator_pops(cdm,
                                                      study_start_date = NULL,
                                                      study_end_date = NULL,
                                                      study_sex_stratas = c("Female", "Male", "Both"),
                                                      study_days_prior_history = c(10,180),
                                                      combine_subgroups = TRUE
  )
  dpops_independent_subgroups <- collect_denominator_pops(cdm,
                                                          study_start_date = NULL,
                                                          study_end_date = NULL,
                                                          study_sex_stratas = c("Female", "Male", "Both"),
                                                          study_days_prior_history = c(10,180),
                                                          combine_subgroups = FALSE
  )

  expect_true(length(dpops_combine_subgroups$denominator_populations %>%
                       dplyr::select("cohort_definition_id") %>%
                       dplyr::distinct() %>%
                       dplyr::collect()%>%
                       dplyr::pull()) == 6)
  expect_true(nrow(dpops_combine_subgroups$denominator_settings) == 6)

  # note there will be 5,
  # because we still have one with styudy day prior = 0 (the default value)
  expect_true(length(dpops_independent_subgroups$denominator_populations %>%
                       dplyr::select("cohort_definition_id") %>%
                       dplyr::distinct() %>%
                       dplyr::collect()%>%
                       dplyr::pull()) == 5)
  expect_true(nrow(dpops_independent_subgroups$denominator_settings) == 5)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
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
  strata <-  tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1","2"),
    cohort_start_date = as.Date("2012-06-06"),
    cohort_end_date = as.Date("2013-06-06")
  )

  # mock database
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   strata=strata)

  # without using strata cohort
  dpop <- collect_denominator_pops(cdm = cdm)
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) %in%
                    c("1", "2", "3")))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) ==
                    "2010-01-01"))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) ==
                    "2015-06-01"))

  # using strata cohort
  dpop <- collect_denominator_pops(
    cdm = cdm,
    table_name_strata = "strata",
    strata_cohort_id = "1",
  )
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) %in%
                    c("1", "2")))
  expect_true(all(!dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id)  %in%
                    c("3")))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) ==
                    "2012-06-06"))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) ==
                    "2013-06-06"))


  # stratifying cohort multiple events per person
  strata <-  tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1","2","2"),
    cohort_start_date = c(as.Date("2012-06-06"),
                          as.Date("2012-06-06"),
                          as.Date("2013-11-01")),
    cohort_end_date = c(as.Date("2013-06-06"),
                        as.Date("2013-06-06"),
                        as.Date("2014-02-01"))
  )

  # mock database
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   strata=strata)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    table_name_strata = "strata",
    strata_cohort_id = "1",
  )
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) %in%
                    c("1", "2")))
  expect_true(all(!dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id)  %in%
                    c("3")))
  expect_true(sum(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) == "1") == 1)
  expect_true(sum(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) == "2") == 2)

  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) %in%
                    as.Date(c("2012-06-06", "2013-11-01"))))
  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) %in%
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
  # add stratifying cohort
  strata <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1","1","1"),
    cohort_start_date = c(as.Date("2008-02-01"),
                          as.Date("2009-02-01"),
                          as.Date("2010-02-01")),
    cohort_end_date = c(as.Date("2008-04-01"),
                        as.Date("2009-04-01"),
                        as.Date("2010-04-01"))
  )
  # mock database
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   strata=strata)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    table_name_strata = "strata",
    strata_cohort_id = "1",
  )
  expect_true(sum(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) == "1") == 3)

  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) %in%
    as.Date(c("2010-02-01","2009-02-01", "2008-02-01"))))

  expect_true(all(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) %in%
                    as.Date(c("2008-04-01","2009-04-01", "2010-04-01"))))


  # should allow strata cohort to have any name
  cdm$condition_cohort<-cdm$strata
  cdm$strata<-NULL
  dpop <- collect_denominator_pops(
    cdm = cdm,
    table_name_strata = "condition_cohort",
    strata_cohort_id = "1",
  )
  expect_true(sum(dpop$denominator_populations %>% dplyr::collect() %>%
                    dplyr::pull(subject_id) == "1") == 3)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: one male, one female", {
  person <- tibble::tibble(
    person_id = c("1","2"),
    gender_concept_id = c("8507","8532"),
    year_of_birth = rep(2000,2),
    month_of_birth = rep(01,2),
    day_of_birth = rep(01,2)
  )
  observation_period <- tibble::tibble(
    observation_period_id = c("1","2"),
    person_id = c("1","2"),
    observation_period_start_date = rep(as.Date("2010-01-01"),2),
    observation_period_end_date = rep(as.Date("2012-06-01"),2)
  )

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)
  # male only
  dpops <- collect_denominator_pops(cdm,
                                    study_sex_stratas = c("Male")
  )
  expect_true(dpops$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) == "1")

  # female only
  dpops <- collect_denominator_pops(cdm,
                                    study_sex_stratas = c("Female")
  )
  expect_true(dpops$denominator_populations %>% dplyr::collect() %>% dplyr::pull(subject_id) == "2")

  # both
  dpops <- collect_denominator_pops(cdm,
                                    study_sex_stratas = c("Both")
  )
  expect_true(all(dpops$denominator_populations %>% dplyr::collect() %>%
                    dplyr::pull(subject_id) %in% c("1","2")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  dpop1 <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Male"
  )
  dpop2 <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Both"
  )
  dpop3 <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Female"
  )
  expect_true(nrow(dpop1$denominator_population %>% dplyr::collect()) == 2)
  expect_true(nrow(dpop2$denominator_population %>% dplyr::collect()) == 3)
  expect_true(nrow(dpop3$denominator_population %>% dplyr::collect()) == 1)


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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  dpop1 <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Male"
  )
  dpop2 <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Both"
  )
  dpop3 <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Female"
  )
  expect_true(nrow(dpop1$denominator_population %>% dplyr::collect()) == 1)
  expect_true(nrow(dpop2$denominator_population %>% dplyr::collect()) == 1)
  expect_true(nrow(dpop3$denominator_population %>% dplyr::collect()) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  # check min_age
  dpop1 <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(0,150))
  )
  dpop2 <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(8,150))
  )
  dpop3 <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(12,150))
  )
  dpop4 <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(40,150))
  )

  expect_true(nrow(dpop1$denominator_population %>% dplyr::collect()) == 3)
  expect_true(nrow(dpop2$denominator_population %>% dplyr::collect()) == 2)
  expect_true(nrow(dpop3$denominator_population %>% dplyr::collect()) == 1)
  expect_true(nrow(dpop4$denominator_population %>% dplyr::collect()) == 0)

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
  cdm <- generate_mock_incidence_prevalence_db(person, observation_period)

  # entry once they reach the min age criteria
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(10,150))
  )
  # start date is now date of 10th birthday
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2010-06-01"))


  # exit once they reach the max age criteria
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(0,10))
  )
  # end date is the day before their 11th birthday
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2011-05-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                              observation_period = observation_period)

  # if we have two age groups 1) 11 to 12, and 2) 13 to 14
  # we expect the person to be in the first cohort up
  # to the day before their 13th birthday
  # and in the second from their 13th birthday
  # up to the day before their 15th birthday
dpops <- collect_denominator_pops(cdm = cdm,
study_age_stratas = list(c(11, 12),
                         c(13,14))
)
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id == 1) %>%
  dplyr::select(cohort_start_date) %>%
  dplyr::pull() == as.Date("2011-01-01"))
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id == 1) %>%
  dplyr::select(cohort_end_date) %>%
  dplyr::pull() == as.Date("2012-12-31"))
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id == 2) %>%
  dplyr::select(cohort_start_date) %>%
  dplyr::pull() == as.Date("2013-01-01"))
expect_true(dpops$denominator_populations %>%
  dplyr::filter(cohort_definition_id == 2) %>%
  dplyr::select(cohort_end_date) %>%
  dplyr::pull() == as.Date("2014-12-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  # expect two rows
  # one per observation period
  dpop <- collect_denominator_pops(cdm = cdm)
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 2)

  # expect one rows- if start date is 1st Jan 2011
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2011-01-01")
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 1)
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2011-01-01"))
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2011-06-01"))

  # expect one rows- if start date is end of 2020
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_end_date = as.Date("2010-12-31")
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 1)
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_start_date) == as.Date("2010-01-01"))
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>% dplyr::pull(cohort_end_date) == as.Date("2010-06-01"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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

  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(10,100))
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) == 4)

  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
                dplyr::filter(subject_id == "1") %>%
                dplyr::summarise(check = cohort_start_date == as.Date("2010-03-03")) %>%
                dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
                dplyr::filter(subject_id == "2") %>%
                dplyr::summarise(check = cohort_start_date == as.Date("2010-01-03")) %>%
                dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
                dplyr::filter(subject_id == "3") %>%
                dplyr::summarise(check = cohort_start_date == as.Date("2010-03-01")) %>%
                dplyr::pull())
  expect_true(dpop$denominator_populations %>% dplyr::collect() %>%
                dplyr::filter(subject_id == "4") %>%
                dplyr::summarise(check = cohort_start_date == as.Date("2010-01-01")) %>%
                dplyr::pull())

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2100-01-01")
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect())==0)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_end_date = as.Date("1800-01-01")
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect())==0)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(155,200))
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect())==0)

  # note could include people as it would go up to day before first birthday
  # but given observation period, here we would expect a null
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(0,1))
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect())==0)

  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(0,15)),
    study_days_prior_history = 365000,
    verbose = FALSE
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect())==0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})

test_that("mock db: check expected errors", {

  cdm <- generate_mock_incidence_prevalence_db()

  # not a cdm reference
  testthat::expect_error(collect_denominator_pops(
    cdm = "a"))

  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(10,10))))
  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(-2,1))))
  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(0,-1))))

  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    max_age = c(100,110),
    verbose = FALSE))

  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Men",
    verbose = FALSE))

  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    study_days_prior_history = -30,
    verbose = FALSE))

  # no person table
  cdm1<-cdm
  cdm1$person<-NULL
  testthat::expect_error(collect_denominator_pops(
    cdm = cdm1))

  # no observation_period table
  cdm1<-cdm
  cdm1$observation_period<-NULL
  testthat::expect_error(collect_denominator_pops(
    cdm = cdm1))

  # no strata table
  cdm1<-cdm
  cdm1$strata<-NULL
  testthat::expect_error(collect_denominator_pops(
    cdm = cdm1,
    table_name_strata="strata"
  ))

  # strata table doesn´t conform
  strata <-  tibble::tibble(
    cohort_id = "1",
    id = c("1","2"),
    start_date = as.Date("2012-06-06"),
    end_date = as.Date("2013-06-06")
  )
  cdm <- generate_mock_incidence_prevalence_db(strata=strata)
  testthat::expect_error(collect_denominator_pops(
    cdm = cdm,
    table_name_strata="strata"
  ))

DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)
  dpop <- collect_denominator_pops(cdm = cdm)

  # check last n_current equals the number of rows of the denominator pop
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) ==
                dpop$attrition$current_n[7])

  # check missings
  dpop <- collect_denominator_pops(cdm = cdm)
  expect_true(dpop$attrition$excluded[2] == 1)
  expect_true(dpop$attrition$excluded[3] == 1)

  # check sex criteria
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_sex_stratas = "Female"
  )
  expect_true(nrow(dpop$denominator_populations %>% dplyr::collect()) ==
                dpop$attrition$current_n[8])
  expect_true(dpop$attrition$excluded[8] == 2)

  # check age criteria
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_age_stratas = list(c(24,25))
  )
  expect_true(dpop$attrition$excluded[3] == 1)

  # check observation criteria
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2010-01-01"),
    study_end_date = as.Date("2012-01-01")
  )
  expect_true(dpop$attrition$excluded[5] == 2)

  # check prior observation criteria
  dpop <- collect_denominator_pops(
    cdm = cdm,
    study_start_date = as.Date("2015-01-01"),
    study_end_date = as.Date("2016-06-30"),
    study_days_prior_history = 365
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
  cdm <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period)
  dpop <- collect_denominator_pops(cdm = cdm)
  expect_true(all(dpop$attrition$current_n == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})


