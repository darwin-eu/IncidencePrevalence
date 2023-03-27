
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
    "cohort_definition_id", "cohort_name",
    "age_group",
    "sex",
    "start_date",
    "end_date",
    "days_prior_history"
  ) %in%
    names(CDMConnector::cohortSet(dpop))))

  expect_true(all(c(
    "cohort_definition_id",
    "number_records",
    "number_subjects"
  ) %in%
    names(CDMConnector::cohortCount(dpop))))

  # variable names
  expect_true(all(c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(dpop %>% dplyr::collect())))

  expect_true(all(c(
    "cohort_definition_id", "number_records", "number_subjects",
    "reason_id","reason",
    "excluded_records", "excluded_subjects"
  ) %in%
  names(CDMConnector::cohortAttrition(dpop))))

  expect_true(tibble::is_tibble(CDMConnector::cohortAttrition(dpop)))
  expect_true(CDMConnector::cohortCount(dpop)$number_records ==1)
  expect_true(CDMConnector::cohortCount(dpop) %>%
                dplyr::filter(cohort_definition_id == 1) %>%
                dplyr::pull("number_records") == 1)

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
    ageGroup = list(c(0, 59), c(60, 69)),
    sex = c("Female", "Male"),
    verbose = TRUE
  )

  femaleCohortIds <- CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::pull("cohort_definition_id")
  maleCohortIds <- CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::pull("cohort_definition_id")

  # Female cohorts should be empty
  expect_true(CDMConnector::cohortCount(cdm$dpop) %>%
    dplyr::filter(cohort_definition_id %in% femaleCohortIds) %>%
    dplyr::summarise(n = sum(.data$number_records)) == 0)
  # We should have people in male cohorts
  expect_true(CDMConnector::cohortCount(cdm$dpop) %>%
                dplyr::filter(cohort_definition_id %in% maleCohortIds) %>%
                dplyr::summarise(n = sum(.data$number_records)) > 0)

 # all pops without anyone
  expect_message(cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    ageGroup = list(c(50, 59), c(60, 69)),
    daysPriorHistory = c(0, 365)
  ))
  expect_true(all(CDMConnector::cohortCount(cdm$dpop)$number_records == 0))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # using cohort strata
  # add stratifying cohort
  strataTable <- tibble::tibble(
    cohort_definition_id = 1,
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
    strataCohortId = 1
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
  skip_on_cran()
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
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 1)
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
  skip_on_cran()
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
    ageGroup = list(c(10, 100))
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
    ageGroup = list(c(0, 10))
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
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
  # all options being used except study start and end
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    ageGroup = list(
      c(0, 5), c(6, 10),
      c(11, 15), c(16, 20),
      c(21, 25), c(26, 30),
      c(31, 35), c(36, 40),
      c(41, 45), c(46, 50),
      c(51, 55), c(56, 60),
      c(61, 100)
    ),
    sex = c("Female", "Male", "Both"),
    daysPriorHistory = c(0, 30, 60, 90, 120, 150, 180),
    verbose=TRUE
  )
  expect_true(any(CDMConnector::cohortCount(cdm$dpop)$number_records > 0))

  # all options being used
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = as.Date("2011-01-01"),
    endDate = as.Date("2013-06-15"),
    ageGroup = list(c(0, 59), c(60, 69)),
    sex = c("Female", "Male", "Both"),
    daysPriorHistory = c(0, 180),
    verbose = TRUE
  )
  expect_true(any(CDMConnector::cohortCount(cdm$dpop)$number_records > 0))
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
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 55)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: subset denominator by cohort", {
  skip_on_cran()
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
    cohort_definition_id = c(1,1,2),
    subject_id = c("1", "2", "2"),
    cohort_start_date = as.Date(c("2012-06-06", "2012-06-06", "2012-09-01")),
    cohort_end_date = as.Date(c("2013-06-06", "2013-06-06", "2013-02-01"))
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

  # using strata cohort id 1
  dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = 1,
    strataCohortName="test_strata"
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

  # using strata cohort id 2
  dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = 2,
  )
  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(subject_id) %in%
                    c("2")))
  expect_true(all(!dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(subject_id) %in%
                    c("1")))
  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_start_date) ==
                    "2012-09-01"))
  expect_true(all(dpop %>%
                    dplyr::collect() %>%
                    dplyr::pull(cohort_end_date) ==
                    "2013-02-01"))

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
    strataCohortId = 1,
    tablePrefix = "stratified"
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
    cohort_definition_id = 1,
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
    strataCohortId = 1,
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
    strataCohortId = 1,
  )
  expect_true(sum(dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == "1") == 3)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: one male, one female", {
  skip_on_cran()
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
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == "2")

  # both
  cdm$dpop <- generateDenominatorCohortSet(cdm,
    sex = c("Both")
  )
  expect_true(all(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in% c("1", "2")))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example with restriction on sex", {
  skip_on_cran()
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
  expect_true(CDMConnector::cohortCount(cdm$dpop1)$number_records == 2)
  expect_true(CDMConnector::cohortCount(cdm$dpop2)$number_records == 3)
  expect_true(CDMConnector::cohortCount(cdm$dpop3)$number_records == 1)
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
  expect_true(CDMConnector::cohortCount(cdm$dpop1)$number_records == 1)
  expect_true(CDMConnector::cohortCount(cdm$dpop2)$number_records == 1)
  expect_true(CDMConnector::cohortCount(cdm$dpop3)$number_records == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example with restriction on age", {
  skip_on_cran()
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
    ageGroup = list(c(0, 150))
  )
  cdm$dpop2 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(8, 150))
  )
  cdm$dpop3 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(12, 150))
  )
  cdm$dpop4 <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(40, 150))
  )

  expect_true(CDMConnector::cohortCount(cdm$dpop1)$number_records == 3)
  expect_true(CDMConnector::cohortCount(cdm$dpop2)$number_records == 2)
  expect_true(CDMConnector::cohortCount(cdm$dpop3)$number_records == 1)
  expect_true(CDMConnector::cohortCount(cdm$dpop4)$number_records == 0)

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
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # entry once they reach the min age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(10, 150))
  )
  # start date is now date of 10th birthday
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-06-01"))


  # exit once they reach the max age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(0, 10))
  )
  # end date is the day before their 11th birthday
  expect_true(cdm$dpop %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2011-05-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check age edge cases", {
  skip_on_cran()
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
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # entry once they reach the min age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(10, 10))
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
  skip_on_cran()
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
    ageGroup = list(
      c(11, 12),
      c(13, 14)
    )
  )
  expect_true(cdm$dpop %>%
    dplyr::filter(cohort_definition_id==1) %>%
      dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2011-01-01"))
  expect_true(cdm$dpop %>%
    dplyr::filter(cohort_definition_id==1) %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == as.Date("2012-12-31"))
  expect_true(cdm$dpop %>%
    dplyr::filter(cohort_definition_id==2) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2013-01-01"))
  expect_true(cdm$dpop %>%
    dplyr::filter(cohort_definition_id==2) %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == as.Date("2014-12-31"))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check example with multiple observation periods", {
  skip_on_cran()
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
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 2)
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_subjects == 1)

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

  # expect one row- if start date is end of 2020
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
  skip_on_cran()
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
    ageGroup = list(c(10, 100))
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
  skip_on_cran()
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
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 0)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    endDate = as.Date("1800-01-01")
  )
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 0)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(155, 200))
  )
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 0)

  # note could include people as it would go up to day before first birthday
  # but given observation period, here we would expect a null
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(0, 1))
  )
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 0)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(0, 15)),
    daysPriorHistory = 365000,
    verbose = FALSE
  )
  expect_true(CDMConnector::cohortCount(cdm$dpop)$number_records == 0)

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check expected errors", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef()

  # not a cdm reference
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = "a"
  ))
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(-2, 1))
  ))
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(0, -1))
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
  testthat::expect_error(generateDenominatorCohortSet(
    cdm = cdm,
    strataTable = "strata",
    strataCohortId = c(1, 2)
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
  testthat::expect_error(mockIncidencePrevalenceRef(strataTable = strataTable))
})

test_that("mock db: check attrition table logic", {
  skip_on_cran()
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
    observation_period_start_date = c(
      as.Date("2017-01-01"),
      rep(as.Date("2000-01-01"), 3),
      rep(as.Date("2016-01-01"), 3)
    ),
    observation_period_end_date = c(
      as.Date("2020-06-01"),
      rep(as.Date("2017-06-01"), 3),
      rep(as.Date("2020-06-01"), 3)
    )
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )
  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)

  # check last n_current equals the number of rows of the denominator pop
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) ==
    CDMConnector::cohortAttrition(cdm$dpop)$number_records[7])

  # check missings
  cdm$dpop <- generateDenominatorCohortSet(cdm = cdm)
  expect_true(CDMConnector::cohortAttrition(cdm$dpop)$excluded_records[2] == 1)
  expect_true(CDMConnector::cohortAttrition(cdm$dpop)$excluded_records[3] == 1)

  # check sex criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Male"
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) ==
                tail(CDMConnector::cohortAttrition(cdm$dpop)$number_records, 1))
  expect_true(CDMConnector::cohortAttrition(cdm$dpop) %>%
                dplyr::filter(reason == "Not Male") %>%
                dplyr::pull("excluded_records") == 3)

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    sex = "Female"
  )
  expect_true(nrow(cdm$dpop %>% dplyr::collect()) ==
    tail(CDMConnector::cohortAttrition(cdm$dpop)$number_records, 1))
  expect_true(CDMConnector::cohortAttrition(cdm$dpop) %>%
                dplyr::filter(reason == "Not Female") %>%
                dplyr::pull("excluded_records") == 2)

  # check age criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(c(24, 25))
  )
  expect_true(CDMConnector::cohortAttrition(cdm$dpop)$excluded_records[3] == 1)

  # check observation criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2010-01-01"),
    endDate = as.Date("2012-01-01")
  )
  expect_true(CDMConnector::cohortAttrition(cdm$dpop)$excluded_records[5] == 2)

  # check prior observation criteria
  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2015-01-01"),
    endDate = as.Date("2016-06-30"),
    daysPriorHistory = 365
  )
  expect_true(CDMConnector::cohortAttrition(cdm$dpop)$excluded_records[7] == 1)
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
  expect_true(all(CDMConnector::cohortAttrition(dpop)$number_records == 3))
  expect_true(all(CDMConnector::cohortAttrition(dpop)$number_subjects == 1))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check attrition with multiple cohorts", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)

  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    sex = c("Male", "Female", "Both"),
    verbose = TRUE
  )
  # for male cohort we should have a row for those excluded for not being male
  expect_true(any("Not Male" == CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == TRUE)
  expect_true(any("Not Female" == CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)
  # for female cohort we should have a row for those excluded for not being male
  expect_true(any("Not Male" == CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)
  expect_true(any("Not Female" == CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == TRUE)
  # for both cohort we should have a row for those excluded for not being male
  expect_true(any("Not Male" == CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)
  expect_true(any("Not Female" == CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)

  cdm$dpop <- generateDenominatorCohortSet(cdm,
    startDate = NULL,
    endDate = NULL,
    daysPriorHistory = c(0, 365),
    verbose = TRUE
  )

  # nobody dropped for prior hist when req is 0
  expect_true(CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::filter(days_prior_history == 0) %>%
    dplyr::filter(reason == "No observation time available after applying age and prior history criteria") %>%
    dplyr::pull(.data$excluded_records) == 0)
  # some people dropped for prior hist when req is 365
  expect_true(CDMConnector::cohortSet(cdm$dpop) %>%
    dplyr::inner_join(CDMConnector::cohortAttrition(cdm$dpop),multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::filter(days_prior_history == 365) %>%
    dplyr::filter(reason == "No observation time available after applying age and prior history criteria") %>%
    dplyr::pull(.data$excluded_records) > 0)


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("mock db: check compute permanent", {
  skip_on_cran()

  # using temp
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"
  cdm$dpop_temp <- generateDenominatorCohortSet(cdm = cdm,sample = 1000,
                                                ageGroup = list(c(0,10), c(11,20),
                                                                c(21,30), c(31,40),
                                                                c(41,50), c(51,60)),
                                                daysPriorHistory = c(0,1,2),
                                                tablePrefix = NULL)
  # if using temp tables
  # we have temp tables created by dbplyr
  expect_true(any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                          "dbplyr_")))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  # using permanent
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"

  cdm$dpop_perm <- generateDenominatorCohortSet(cdm = cdm,sample = 1000,
                                                ageGroup = list(c(0,10), c(11,20),
                                                                c(21,30), c(31,40),
                                                                c(41,50), c(51,60)),
                                                daysPriorHistory = c(0,1,2),
                                                tablePrefix = "example")
  # we´ll now have the stem table
  expect_true(any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
                             schema = attr(cdm, "write_schema")),
                          "example")))
  # with no temp tables created by dbplyr
  expect_true(any(stringr::str_starts(CDMConnector::listTables(attr(cdm, "dbcon")),
                          "dbplyr_",
                          negate = TRUE)))


  expect_true(tibble::is_tibble(CDMConnector::cohortSet(cdm$dpop_perm)))
  expect_true(tibble::is_tibble(CDMConnector::cohortCount(cdm$dpop_perm)))
  expect_true(tibble::is_tibble(CDMConnector::cohortAttrition(cdm$dpop_perm)))

  # # reconnect
  # cdmReconn <- CDMConnector::cdm_from_con(con = attr(cdm, "dbcon"),
  #                    cdm_tables = c("person", "observation_period",
  #                                   "cdm_source","vocabulary"),
  #                    cohort_tables = c("example_denominator"),
  #                    write_schema = "main"
  # )
  #
  # expect_true(tibble::is_tibble(CDMConnector::cohortSet(cdmReconn$example_denominator)))
  # expect_true(tibble::is_tibble(CDMConnector::cohortCount(cdmReconn$example_denominator)))
  # expect_true(tibble::is_tibble(CDMConnector::cohortAttrition(cdmReconn$example_denominator)))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
