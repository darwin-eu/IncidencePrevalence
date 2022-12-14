
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()

  dpop <- generateDenominatorCohortSet(cdm = cdm)

  expect_true(all(c(
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %in%
    names(dpop %>% dplyr::collect())))

  expect_true(all(c(
    "age_group",
    "sex",
    "start_date",
    "end_date",
    "days_prior_history",
    "cohort_definition_id"
  ) %in%
    names(settings(dpop))))

  # variable names
  expect_true(length(names(dpop %>%
                             dplyr::collect())) == 4)
  expect_true(all(c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(dpop %>% dplyr::collect())))

  expect_true(tibble::is_tibble(attrition(dpop)))
  expect_true(!is.null(sqlTrace(dpop)))

  # check verbose
  expect_message(generateDenominatorCohortSet(
    cdm = cdm,
    verbose = TRUE
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: checks on working example", {
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )
  # some pops with people, but some without
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    ageGroups = list(c(0, 59), c(60, 69)),
    sex = c("Female", "Male", "Both"),
    verbose = TRUE
  )
  expect_true(cdm$dpop %>%
    dplyr::count() %>%
      dplyr::pull() >= 1)

  # all pops without anyone
  expect_message(cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    ageGroups = list(c(50, 59), c(60, 69)),
    daysPriorHistory = c(0, 365)
  ))
  expect_true(cdm$dpop %>%
    dplyr::count() %>%
      dplyr::pull() == 0)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # using cohort strata
  # add stratifying cohort
  strataTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1", "2"),
    cohort_start_date = as.Date("2010-03-15"),
    cohort_end_date = as.Date("2012-03-15")
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    strataTable = strataTable
  )

  # using strata cohort
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1"
  )
  expect_true(cdm$dpop %>%
    dplyr::select(cohort_start_date) %>%
      dplyr::pull() == "2010-03-15")
  expect_true(cdm$dpop %>%
    dplyr::select(cohort_end_date) %>%
      dplyr::pull() == "2012-03-15")

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example we expect to work", {
  # one person, one observation periods
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    person = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)
  expect_true(nrow(cdm$dpop %>%
                     dplyr::collect()) == 1)
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_start_date) == as.Date("2010-01-01"))
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_end_date) == as.Date("2015-06-01"))


  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-02-15"),
    endDate = as.Date("2010-05-15")
  )
  expect_true(nrow(cdm$dpop %>%
                     dplyr::collect()) == 1)
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_start_date) == as.Date("2010-02-15"))
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_end_date) == as.Date("2010-05-15"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check another example we expect to work", {
  # 5 person, 1 observation periods
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3", "4", "5"),
    gender_concept_id = c("8507", "8532", "8507", "8532", "8532"),
    year_of_birth = c(1995, 1993, 1994, 1996, NA),
    month_of_birth = c(07, NA, 06, 05, 04),
    day_of_birth = c(25, NA, 01, 02, 03)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4", "5"),
    person_id = c("1", "2", "3", "4", "5"),
    observation_period_start_date = rep(as.Date("2000-01-01"), 5),
    observation_period_end_date = rep(as.Date("2015-06-01"), 5)
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)

  expect_true(nrow(cdm$dpop %>%
                     dplyr::collect()) == 4)
  expect_true(all(cdm$dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_start_date) == as.Date("2000-01-01")))
  expect_true(all(cdm$dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_end_date) == as.Date("2015-06-01")))


  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(10, 100))
  )
  # check min age change cohort start date
  # check imputation
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "1") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2005-07-25")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "2") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2003-01-01")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "3") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2004-06-01")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "4") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2006-05-02")) %>%
    dplyr::pull())

  # check max age change cohort start date
  # check imputation
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(0, 10))
  )
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "1") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2006-07-24")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "2") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2003-12-31")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "3") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2005-05-31")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == "4") %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2007-05-01")) %>%
    dplyr::pull())

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-02-15"),
    endDate = as.Date("2010-05-15")
  )
  expect_true(nrow(cdm$dpop %>%
    dplyr::collect()) == 4)
  expect_true(all(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-02-15")))
  expect_true(all(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2010-05-15")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: mock example 1000", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
  # all options being used except study start and end
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    ageGroups = list(c(0, 5),c(6, 10),
                     c(11, 15),c(16, 20),
                     c(21, 25),c(26, 30),
                     c(31, 35),c(36, 40),
                     c(41, 45),c(46, 50),
                     c(51, 55),c(56, 60),
                     c(61, 100)),
    sex = c("Female", "Male", "Both"),
    daysPriorHistory = c(0,30,60, 90,120,150, 180),
    verbose = TRUE
  )
  expect_true(nrow(cdm$dpop %>%
    dplyr::collect()) > 0)

  # all options being used
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = as.Date("2011-01-01"),
    endDate = as.Date("2013-06-15"),
    ageGroups = list(c(0, 59), c(60, 69)),
    sex = c("Female", "Male", "Both"),
    daysPriorHistory = c(0, 180),
    verbose = TRUE
  )
  expect_true(nrow(cdm$dpop %>%
    dplyr::collect()) > 0)
  expect_true(min(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date)) >=
    as.Date("2011-01-01"))
  expect_true(max(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date)) <=
    as.Date("2013-06-15"))

  # with sampling
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    sample = 55
  )
  expect_true(nrow(cdm$dpop%>%
                     dplyr::collect()) == 55)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: subset denominator by cohort", {
  # one person, one observation periods
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  strataTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1", "2"),
    cohort_start_date = as.Date("2012-06-06"),
    cohort_end_date = as.Date("2013-06-06")
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    strataTable = strataTable
  )

  # without using strata cohort
  dpop <- generateDenominatorCohortSet(cdm = cdm)
  expect_true(all(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c("1", "2", "3")))
  expect_true(all(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) ==
    "2010-01-01"))
  expect_true(all(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) ==
    "2015-06-01"))

  # using strata cohort
  dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1",
  )
  expect_true(all(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c("1", "2")))
  expect_true(all(!dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c("3")))
  expect_true(all(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) ==
    "2012-06-06"))
  expect_true(all(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) ==
    "2013-06-06"))


  # stratifying cohort multiple events per person
  strataTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1", "2", "2"),
    cohort_start_date = c(
      as.Date("2012-06-06"),
      as.Date("2012-06-06"),
      as.Date("2013-11-01")
    ),
    cohort_end_date = c(
      as.Date("2013-06-06"),
      as.Date("2013-06-06"),
      as.Date("2014-02-01")
    )
  )
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    strataTable = strataTable
  )

  dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1",
  )
  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(subject_id) %in%
    c("1", "2")))
  expect_true(all(!dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c("3")))
  expect_true(sum(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(subject_id) == "1") == 1)
  expect_true(sum(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(subject_id) == "2") == 2)

  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_start_date) %in%
    as.Date(c("2012-06-06", "2013-11-01"))))
  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_end_date) %in%
    as.Date(c("2013-06-06", "2014-02-01"))))


  # multiple observation periods and multiple outcomes for a person
  # one person, one observation periods
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1"),
    observation_period_start_date = c(
      as.Date("2008-01-01"),
      as.Date("2009-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2008-06-01"),
      as.Date("2009-06-01"),
      as.Date("2010-06-01")
    )
  )
  # add stratifying cohort
  strataTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = c("1", "1", "1"),
    cohort_start_date = c(
      as.Date("2008-02-01"),
      as.Date("2009-02-01"),
      as.Date("2010-02-01")
    ),
    cohort_end_date = c(
      as.Date("2008-04-01"),
      as.Date("2009-04-01"),
      as.Date("2010-04-01")
    )
  )
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    strataTable = strataTable
  )

  dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = "1",
  )
  expect_true(sum(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(subject_id) == "1") == 3)

  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_start_date) %in%
    as.Date(c("2010-02-01", "2009-02-01", "2008-02-01"))))

  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_end_date) %in%
    as.Date(c("2008-04-01", "2009-04-01", "2010-04-01"))))


  # should allow strata cohort to have any name
  cdm$condition_cohort <- cdm$strata
  cdm$strata <- NULL
  dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "condition_cohort",
    strataCohortId = "1",
  )
  expect_true(sum(dpop %>%
                    dplyr::collect() %>%
    dplyr::pull(subject_id) == "1") == 3)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: one male, one female", {
  personTable <- tibble::tibble(
    person_id = c("1", "2"),
    gender_concept_id = c("8507", "8532"),
    year_of_birth = rep(2000, 2),
    month_of_birth = rep(01, 2),
    day_of_birth = rep(01, 2)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = c("1", "2"),
    observation_period_start_date = rep(as.Date("2010-01-01"), 2),
    observation_period_end_date = rep(as.Date("2012-06-01"), 2)
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )
  # male only
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    sex = c("Male")
  )
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(subject_id) == "1")

  # female only
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    sex = c("Female")
  )
  expect_true(cdm$dpop%>%
                dplyr::collect() %>%
                dplyr::pull(subject_id) == "2")

  # both
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    sex = c("Both")
  )
  expect_true(all(cdm$dpop%>%
                    dplyr::collect() %>%
    dplyr::pull(subject_id) %in% c("1", "2")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example with restriction on sex", {
  # two male, one female
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = c("8507", "8507", "8532"),
    year_of_birth = rep(2000, 3),
    month_of_birth = rep(06, 3),
    day_of_birth = rep(01, 3)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = rep(as.Date("2010-01-01"), 3),
    observation_period_end_date = rep(as.Date("2015-06-01"), 3)
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm$dpop1 <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Male"
  )
  cdm$dpop2 <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Both"
  )
  cdm$dpop3 <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Female"
  )
  expect_true(nrow(cdm$dpop1 %>% dplyr::collect()) == 2)
  expect_true(nrow(cdm$dpop2 %>% dplyr::collect()) == 3)
  expect_true(nrow(cdm$dpop3 %>% dplyr::collect()) == 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # one male only
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm$dpop1 <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Male"
  )
  cdm$dpop2 <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Both"
  )
  cdm$dpop3 <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Female"
  )
  expect_true(nrow(cdm$dpop1 %>% dplyr::collect()) == 1)
  expect_true(nrow(cdm$dpop2 %>% dplyr::collect()) == 1)
  expect_true(nrow(cdm$dpop3 %>% dplyr::collect()) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example with restriction on age", {
  # three people, born in 2000, 2005, and 2010
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3"),
    gender_concept_id = rep("8507", 3),
    year_of_birth = c(2000, 2005, 2010),
    month_of_birth = rep(06, 3),
    day_of_birth = rep(01, 3)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1", "2", "3"),
    observation_period_start_date = rep(as.Date("2010-01-01"), 3),
    observation_period_end_date = rep(as.Date("2015-06-01"), 3)
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # check min_age
  cdm$dpop1 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(0, 150))
  )
  cdm$dpop2 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(8, 150))
  )
  cdm$dpop3 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(12, 150))
  )
  cdm$dpop4 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(40, 150))
  )

  expect_true(nrow(cdm$dpop1 %>% dplyr::collect()) == 3)
  expect_true(nrow(cdm$dpop2 %>% dplyr::collect()) == 2)
  expect_true(nrow(cdm$dpop3 %>% dplyr::collect()) == 1)
  expect_true(nrow(cdm$dpop4 %>% dplyr::collect()) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # one person, born in 2000
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(personTable = personTable,
                  observationPeriodTable = observationPeriodTable)

  # entry once they reach the min age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(10, 150))
  )
  # start date is now date of 10th birthday
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_start_date) == as.Date("2010-06-01"))


  # exit once they reach the max age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(0, 10))
  )
  # end date is the day before their 11th birthday
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_end_date) == as.Date("2011-05-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check age edge cases", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)

  # same min and max
  # one person, born in 2000
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(personTable = personTable,
                                    observationPeriodTable = observationPeriodTable)

  # entry once they reach the min age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(10, 10))
  )
  # start date is now date of 10th birthday
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_start_date) == as.Date("2010-06-01"))
  # end date is the day before their 11th birthday
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_end_date) == as.Date("2011-05-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db check age strata entry and exit", {
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2008-01-01"),
    observation_period_end_date = as.Date("2018-06-01")
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # if we have two age groups 1) 11 to 12, and 2) 13 to 14
  # we expect the person to be in the first cohort up
  # to the day before their 13th birthday
  # and in the second from their 13th birthday
  # up to the day before their 15th birthday
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(
      c(11, 12),
      c(13, 14)
    )
  )
  expect_true(cdm$dpop%>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2011-01-01"))
  expect_true(cdm$dpop%>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == as.Date("2012-12-31"))
  expect_true(cdm$dpop%>%
    dplyr::filter(cohort_definition_id == 2) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2013-01-01"))
  expect_true(cdm$dpop%>%
    dplyr::filter(cohort_definition_id == 2) %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == as.Date("2014-12-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example with multiple observation periods", {
  # one person, two observation periods
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2"),
    person_id = rep("1", 2),
    observation_period_start_date = c(
      as.Date("2010-01-01"),
      as.Date("2011-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2010-06-01"),
      as.Date("2011-06-01")
    )
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # expect two rows
  # one per observation period
  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 2)

  # expect one rows- if start date is 1st Jan 2011
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2011-01-01")
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 1)
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_start_date) == as.Date("2011-01-01"))
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_end_date) == as.Date("2011-06-01"))

  # expect one rows- if start date is end of 2020
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    endDate = as.Date("2010-12-31")
  )
  expect_true(nrow(cdm$dpop %>%
                     dplyr::collect()) == 1)
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_start_date) == as.Date("2010-01-01"))
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
                dplyr::pull(cohort_end_date) == as.Date("2010-06-01"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check imputation of date of birth", {
  # one person with all info, one missing month, one missing day, and one both
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3", "4"),
    gender_concept_id = rep("8507", 4),
    year_of_birth = rep(2000, 4),
    month_of_birth = c(03, NA, 03, NA),
    day_of_birth = c(03, 03, NA, NA)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4"),
    person_id = c("1", "2", "3", "4"),
    observation_period_start_date = rep(as.Date("2010-01-01"), 4),
    observation_period_end_date = rep(as.Date("2015-06-01"), 4)
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(10, 100))
  )
  expect_true(nrow(cdm$dpop %>%
                     dplyr::collect()) == 4)

  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
    dplyr::filter(subject_id == "1") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-03-03")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
    dplyr::filter(subject_id == "2") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-01-03")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
    dplyr::filter(subject_id == "3") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-03-01")) %>%
    dplyr::pull())
  expect_true(cdm$dpop %>%
                dplyr::collect() %>%
    dplyr::filter(subject_id == "4") %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-01-01")) %>%
    dplyr::pull())

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check edge cases (zero results expected)", {
  # one person, one observation periods
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2100-01-01")
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 0)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    endDate = as.Date("1800-01-01")
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 0)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(155, 200))
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 0)

  # note could include people as it would go up to day before first birthday
  # but given observation period, here we would expect a null
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(0, 1))
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 0)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(0, 15)),
    daysPriorHistory = 365000,
    verbose = FALSE
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check expected errors", {
  cdm <- mockIncidencePrevalenceRef()

  # not a cdm reference
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = "a"
  ))

  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(10, 10))
  ))
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(-2, 1))
  ))
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(0, -1))
  ))

  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    max_age = c(100, 110),
    verbose = FALSE
  ))

  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Men",
    verbose = FALSE
  ))

  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    daysPriorHistory = -30,
    verbose = FALSE
  ))

  # no person table
  cdm1 <- cdm
  cdm1$person <- NULL
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm1
  ))

  # no observation_period table
  cdm1 <- cdm
  cdm1$observation_period <- NULL
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm1
  ))

  # no strata table
  cdm1 <- cdm
  cdm1$strata <- NULL
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm1,
    strataTable = "strata"
  ))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # strata table doesn´t conform
  strataTable <- tibble::tibble(
    cohort_id = "1",
    id = c("1", "2"),
    start_date = as.Date("2012-06-06"),
    end_date = as.Date("2013-06-06")
  )
  cdm <- mockIncidencePrevalenceRef(strataTable = strataTable)
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata"
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check attrition table", {
  # 7 person, 1 observation periods
  personTable <- tibble::tibble(
    person_id = c("1", "2", "3", "4", "5", "6", "7"),
    gender_concept_id = c("8507", "8532", "8507", "8532", "8532", "8507", NA),
    year_of_birth = c(1995, 1993, 1994, 1996, 1998, NA, 1993),
    month_of_birth = c(07, 02, 06, 05, 04, 10, 01),
    day_of_birth = c(25, 14, 01, 02, 03, 10, 12)
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3", "4", "5", "6", "7"),
    person_id = c("1", "2", "3", "4", "5", "6", "7"),
    observation_period_start_date = c(as.Date("2017-01-01"),
                                      rep(as.Date("2000-01-01"), 3),
                                      rep(as.Date("2016-01-01"), 3)),
    observation_period_end_date = c(as.Date("2020-06-01"),
                                    rep(as.Date("2017-06-01"), 3),
                                    rep(as.Date("2020-06-01"), 3))
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )
  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)

  # check last n_current equals the number of rows of the denominator pop
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) ==
    attrition(cdm$dpop)$current_n[7])

  # check missings
  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)
  expect_true(attrition(cdm$dpop)$excluded[2] == 1)
  expect_true(attrition(cdm$dpop)$excluded[3] == 1)

  # check sex criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Female"
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) ==
                attrition(cdm$dpop)$current_n[8])
  expect_true(attrition(cdm$dpop)$excluded[8] == 2)

  # check age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroups = list(c(24, 25))
  )
  expect_true(attrition(cdm$dpop)$excluded[3] == 1)

  # check observation criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-01-01"),
    endDate = as.Date("2012-01-01")
  )
  expect_true(attrition(cdm$dpop)$excluded[5] == 2)

  # check prior observation criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2015-01-01"),
    endDate = as.Date("2016-06-30"),
    daysPriorHistory = 365
  )
  expect_true(attrition(cdm$dpop)$excluded[7] == 1)
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # multiple observation periods per person
  personTable <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 06,
    day_of_birth = 01
  )
  observationPeriodTable <- tibble::tibble(
    observation_period_id = c("1", "2", "3"),
    person_id = c("1"),
    observation_period_start_date = c(
      as.Date("2008-01-01"),
      as.Date("2009-01-01"),
      as.Date("2010-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2008-06-01"),
      as.Date("2009-06-01"),
      as.Date("2010-06-01")
    )
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )
  dpop <- generateDenominatorCohortSet(cdm = cdm)
  expect_true(all(attrition(dpop)$current_n == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
