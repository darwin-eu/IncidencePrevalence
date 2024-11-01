
test_that("mock db: check output format", {
  cdm <- mockIncidencePrevalenceRef()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  expect_true(all(c(
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %in%
    names(cdm$denominator %>% dplyr::collect())))

  expect_true(all(c(
    "cohort_definition_id", "cohort_name",
    "age_group",
    "sex",
    "start_date",
    "end_date",
    "days_prior_observation"
  ) %in%
    names(omopgenerics::settings(cdm$denominator))))

  expect_true(all(c(
    "cohort_definition_id",
    "number_records",
    "number_subjects"
  ) %in%
    names(omopgenerics::cohortCount(cdm$denominator))))

  # variable names
  expect_true(all(c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(cdm$denominator %>% dplyr::collect())))

  expect_true(all(c(
    "cohort_definition_id", "number_records", "number_subjects",
    "reason_id", "reason",
    "excluded_records", "excluded_subjects"
  ) %in%
    names(omopgenerics::attrition(cdm$denominator))))

  expect_true(is.data.frame(omopgenerics::attrition(cdm$denominator)))
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 1)
  expect_true(omopgenerics::cohortCount(cdm$denominator) %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::pull("number_records") == 1)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: checks on working example", {
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

    # some pops with people, but some without
  cdm <- generateDenominatorCohortSet(cdm, name = "denominator",
    ageGroup = list(c(0, 59), c(60, 69)),
    sex = c("Female", "Male")
  )

  femaleCohortIds <- omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::pull("cohort_definition_id")
  maleCohortIds <- omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::pull("cohort_definition_id")

  # Female cohorts should be empty
  expect_true(omopgenerics::cohortCount(cdm$denominator) %>%
    dplyr::filter(cohort_definition_id %in% femaleCohortIds) %>%
    dplyr::summarise(n = sum(.data$number_records)) == 0)
  # We should have people in male cohorts
  expect_true(omopgenerics::cohortCount(cdm$denominator) %>%
    dplyr::filter(cohort_definition_id %in% maleCohortIds) %>%
    dplyr::summarise(n = sum(.data$number_records)) > 0)

  # all pops without anyone
  expect_warning(cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    ageGroup = list(c(50, 59), c(60, 69)),
    daysPriorObservation = c(0, 365)
  ))
  expect_true(all(omopgenerics::cohortCount(cdm$denominator)$number_records == 0))
  CDMConnector::cdm_disconnect(cdm)

  # using cohort target
  # add stratifying cohort
  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2010-03-15"),
    cohort_end_date = as.Date("2012-03-15")
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )

  # using target cohort
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1
  )
  expect_true(cdm$denominator %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == "2010-03-15")
  expect_true(cdm$denominator %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == "2012-03-15")

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check example we expect to work", {
  skip_on_cran()
  # one person, one observation periods
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1,
    person_id = 1,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    person = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 1)
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-01-01"))
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2015-06-01"))

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2010-02-15"), as.Date("2010-05-15"))
  )
  expect_true(nrow(cdm$denominator %>%
    dplyr::collect()) == 1)
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-02-15"))
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2010-05-15"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check another example we expect to work", {
  skip_on_cran()
  # 5 person, 1 observation periods
  personTable <- dplyr::tibble(
    person_id = as.integer(c(1, 2, 3, 4, 5)),
    gender_concept_id = as.integer(c(8507, 8532, 8507, 8532, 8532)),
    year_of_birth = as.integer(c(1995, 1993, 1994, 1996, NA)),
    month_of_birth = as.integer(c(07, NA, 06, 05, 04)),
    day_of_birth = as.integer(c(25, NA, 01, 02, 03))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = as.integer(c(1, 2, 3, 4, 5)),
    person_id = as.integer(c(1, 2, 3, 4, 5)),
    observation_period_start_date = rep(as.Date("2000-01-01"), 5),
    observation_period_end_date = rep(as.Date("2015-06-01"), 5)
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  expect_true(nrow(cdm$denominator %>%
    dplyr::collect()) == 4)
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2000-01-01")))
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2015-06-01")))


  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(10, 100))
  )
  # check min age change cohort start date
  # check imputation
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2005-07-25")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2003-01-01")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 3) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2004-06-01")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 4) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2006-05-02")) %>%
    dplyr::pull())

  # check max age change cohort start date
  # check imputation
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(0, 10))
  )
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2006-07-24")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2003-12-31")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 3) %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2005-05-31")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 4) %>%
    dplyr::summarise(check = cohort_end_date == as.Date("2007-05-01")) %>%
    dplyr::pull())

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2010-02-15"), as.Date("2010-05-15"))
  )
  expect_true(nrow(cdm$denominator %>%
    dplyr::collect()) == 4)
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-02-15")))
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2010-05-15")))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: mock example 1000", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
  # all options being used except study start and end
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    ageGroup = list(
      c(0, 5), c(6, 10), c(11, 15), c(16, 20), c(21, 25), c(26, 30), c(31, 35),
      c(36, 40), c(41, 45), c(46, 50), c(51, 55), c(56, 60), c(61, 100)
    ),
    sex = c("Female", "Male", "Both"),
    daysPriorObservation = c(0, 30, 60, 90, 120, 150, 180)
  )
  expect_true(any(omopgenerics::cohortCount(cdm$denominator)$number_records > 0))

  # all options being used
  cdm <- generateDenominatorCohortSet(cdm, name = "denominator",
    cohortDateRange = c(as.Date("2011-01-01"), as.Date("2013-06-15")),
    ageGroup = list(c(0, 59), c(60, 69)),
    sex = c("Female", "Male", "Both"),
    daysPriorObservation = c(0, 180)
  )
  expect_true(any(omopgenerics::cohortCount(cdm$denominator)$number_records > 0))
  expect_true(min(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date)) >=
    as.Date("2011-01-01"))
  expect_true(max(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date)) <=
    as.Date("2013-06-15"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: subset denominator by cohort", {
  skip_on_cran()
  # one person, one observation periods
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L, 3L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L, 2L, 3L),
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L, 2L),
    subject_id = c(1L, 2L, 2L),
    cohort_start_date = as.Date(c("2012-06-06", "2012-06-06", "2012-09-01")),
    cohort_end_date = as.Date(c("2013-06-06", "2013-06-06", "2013-02-01"))
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )

  # without using target cohort
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(1, 2, 3)))
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) ==
    "2010-01-01"))
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) ==
    "2015-06-01"))

  # using target cohort id 1
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort",
    targetCohortTable = "target",
    targetCohortId = 1
  )
  expect_identical(names(settings(cdm$denominator)),
                   names(settings(cdm$target_cohort)))

  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(1, 2)))
  expect_true(all(!cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(3)))
  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) ==
    "2012-06-06"))
  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) ==
    "2013-06-06"))

  # using target cohort id 2
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort_2",
    targetCohortTable = "target",
    targetCohortId = 2
  )
  expect_true(all(cdm$target_cohort_2 %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(2)))
  expect_true(all(!cdm$target_cohort_2 %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(1)))
  expect_true(all(cdm$target_cohort_2 %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) ==
    "2012-09-01"))
  expect_true(all(cdm$target_cohort_2 %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) ==
    "2013-02-01"))


  # multiple stratification cohorts
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort_mult1",
    targetCohortTable = "target",
    targetCohortId = c(1,2),
  )
  expect_true(nrow(omopgenerics::settings(cdm$target_cohort_mult1))==2)
  # without specifying target, should run for both
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort_mult2",
    targetCohortTable = "target"
  )
  expect_true(nrow(omopgenerics::settings(cdm$target_cohort_mult2))==2)

  CDMConnector::cdm_disconnect(cdm)

  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = as.integer(c(1, 1, 2, 3,4,5,6,7,8,9,10,11,12)),
    subject_id = as.integer(c(1,2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)),
    cohort_start_date = as.Date(c("2012-06-06", "2012-06-06", "2012-09-01", "2012-09-01",
                                  "2012-09-01", "2012-09-01", "2012-09-01", "2012-09-01",
                                  "2012-09-01", "2012-09-01", "2012-09-01", "2012-09-01", "2012-09-01")),
    cohort_end_date = as.Date(c("2013-06-06", "2013-06-06", "2013-02-01", "2012-09-01",
                                "2012-09-01", "2012-09-01", "2012-09-01", "2012-09-01",
                                "2012-09-01", "2012-09-01", "2012-09-01", "2012-09-01", "2012-09-01"))
  )
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort_mult1",
    targetCohortTable = "target",
  )
  expect_true(nrow(omopgenerics::settings(cdm$target_cohort_mult1))==12)



  # stratifying cohort multiple events per person
  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = as.integer(c(1, 2, 2)),
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

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort",
    targetCohortTable = "target",
    targetCohortId = 1
  )
  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(1, 2)))
  expect_true(all(!cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in%
    c(3)))
  expect_true(sum(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == 1) == 1)
  expect_true(sum(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == 2) == 2)

  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) %in%
    as.Date(c("2012-06-06", "2013-11-01"))))
  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) %in%
    as.Date(c("2013-06-06", "2014-02-01"))))
  CDMConnector::cdm_disconnect(cdm)


  # multiple observation periods and multiple outcomes for a person
  # one person, one observation periods
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L),
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
  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1L, 1L, 1L),
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

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "target_cohort",
    targetCohortTable = "target",
    targetCohortId = 1,
  )
  expect_true(sum(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == 1) == 3)

  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) %in%
    as.Date(c("2010-02-01", "2009-02-01", "2008-02-01"))))

  expect_true(all(cdm$target_cohort %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) %in%
    as.Date(c("2008-04-01", "2009-04-01", "2010-04-01"))))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: one male, one female", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = as.integer(c(8507, 8532)),
    year_of_birth = as.integer(rep(2000, 2)),
    month_of_birth = as.integer(rep(01, 2)),
    day_of_birth = as.integer(rep(01, 2))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = rep(as.Date("2010-01-01"), 2),
    observation_period_end_date = rep(as.Date("2012-06-01"), 2)
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )
  # male only
  cdm <- generateDenominatorCohortSet(cdm, name = "denominator",
    sex = c("Male")
  )
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == 1)

  # female only
  cdm <- generateDenominatorCohortSet(cdm, name = "denominator",
    sex = c("Female")
  )
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) == 2)

  # both
  cdm <- generateDenominatorCohortSet(cdm, name = "denominator",
    sex = c("Both")
  )
  expect_true(all(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(subject_id) %in% c(1, 2)))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check example with restriction on sex", {

  # two male, one female
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L, 3L),
    gender_concept_id = as.integer(c(8507, 8507, 8532)),
    year_of_birth = as.integer(rep(2000, 3)),
    month_of_birth = as.integer(rep(06, 3)),
    day_of_birth = as.integer(rep(01, 3))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L, 2L, 3L),
    observation_period_start_date = rep(as.Date("2010-01-01"), 3),
    observation_period_end_date = rep(as.Date("2015-06-01"), 3)
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator1",
    sex = "Male"
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator2",
    sex = "Both"
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator3",
    sex = "Female"
  )
  expect_true(omopgenerics::cohortCount(cdm$denominator1)$number_records == 2)
  expect_true(omopgenerics::cohortCount(cdm$denominator2)$number_records == 3)
  expect_true(omopgenerics::cohortCount(cdm$denominator3)$number_records == 1)
  CDMConnector::cdm_disconnect(cdm)

  # one male only
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator1",
    sex = "Male"
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator2",
    sex = "Both"
  )
  # TODO expect warning
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator3",
    sex = "Female"
  )
  expect_true(omopgenerics::cohortCount(cdm$denominator1)$number_records == 1)
  expect_true(omopgenerics::cohortCount(cdm$denominator2)$number_records == 1)
  expect_true(omopgenerics::cohortCount(cdm$denominator3)$number_records == 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check example with restriction on age", {

  # three people, born in 2000, 2005, and 2010
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L, 3L),
    gender_concept_id = as.integer(rep(8507, 3)),
    year_of_birth = as.integer(c(2000, 2005, 2010)),
    month_of_birth = as.integer(rep(06, 3)),
    day_of_birth = as.integer(rep(01, 3))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = c(1L, 2L, 3L),
    observation_period_start_date = rep(as.Date("2010-01-01"), 3),
    observation_period_end_date = rep(as.Date("2015-06-01"), 3)
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # check min_age
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator_a",
    ageGroup = list(c(0, 150))
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator_b",
    ageGroup = list(c(8, 150))
  )
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator_c",
    ageGroup = list(c(12, 150))
  )
 expect_warning(cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator_d",
    ageGroup = list(c(40, 150))
  ))

  expect_true(omopgenerics::cohortCount(cdm$denominator_a)$number_records == 3)
  expect_true(omopgenerics::cohortCount(cdm$denominator_b)$number_records == 2)
  expect_true(omopgenerics::cohortCount(cdm$denominator_c)$number_records == 1)
  expect_true(omopgenerics::cohortCount(cdm$denominator_d)$number_records == 0)

  CDMConnector::cdm_disconnect(cdm)

  # one person, born in 2000
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # entry once they reach the min age criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(10, 150))
  )
  # start date is now date of 10th birthday
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-06-01"))


  # exit once they reach the max age criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(0, 10))
  )
  # end date is the day before their 11th birthday
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2011-05-31"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check age edge cases", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)

  # same min and max
  # one person, born in 2000
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )

  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  # entry once they reach the min age criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(10, 10))
  )
  # start date is now date of 10th birthday
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-06-01"))
  # end date is the day before their 11th birthday
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2011-05-31"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db check age target entry and exit", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
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
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(
      c(11, 12),
      c(13, 14)
    )
  )
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2011-01-01"))
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == as.Date("2012-12-31"))
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 2) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2013-01-01"))
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 2) %>%
    dplyr::select(cohort_end_date) %>%
    dplyr::pull() == as.Date("2014-12-31"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db check target prior observation requirement", {
  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-07-01"),
    observation_period_end_date = as.Date("2018-06-01")
  )
  targetCohortTable <- dplyr::tibble( # same as obs period
    cohort_definition_id = 1L,
    subject_id = 1L,
    cohort_start_date = as.Date("2012-01-01"),
    cohort_end_date = as.Date("2018-06-01")
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )

  # if we have one age group 11 to 12
  # we expect the person to be in the first cohort up
  # to the day before their 13th birthday
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(
      c(11, 12)
    )
  )
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2011-01-01"))
  # add prior observation requirement
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(
      c(11, 12)
    ), daysPriorObservation = 365
  )
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2011-07-01"))

  # with target cohort
  # result should be unaffected
  # (as prior observation based on obs period achieved before target cohort start)
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    targetCohortId = 1,
    ageGroup = list(
      c(11, 12)
    ), daysPriorObservation = 0
  )
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2012-01-01"))

 expect_message(cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator", targetCohortTable = "target",
    targetCohortId = 1,
    ageGroup = list(
      c(11, 12)
    ), daysPriorObservation = 365
  ))
  expect_true(cdm$denominator %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::select(cohort_start_date) %>%
    dplyr::pull() == as.Date("2012-01-01"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: targetRequirementsAtEntry", {
  skip_on_cran()

  ## Prior observation
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id =  c(1L, 2L),
    observation_period_start_date = c(as.Date("2012-01-01"),
                                      as.Date("2013-01-04")),
    observation_period_end_date = as.Date("2018-06-01")
  )
  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1L, 2L),
    cohort_start_date = c(as.Date("2012-01-05"),
                          as.Date("2013-01-05")),
    cohort_end_date = as.Date("2018-06-01"),
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = "denom_reqs_any_time",
    daysPriorObservation = c(0,2,4,10),
    targetCohortTable = "target",
    targetCohortId = 1
  )
  # enter when they satisfy prior hist reqs
  # subject 1 should be in both cohorts, subject 2 only in first with 0 day req
  expect_true(omopgenerics::cohortCount(cdm$denom_reqs_any_time) %>%
    dplyr::filter(cohort_definition_id == 1) %>%
    dplyr::pull("number_records") == 2)
  expect_true(omopgenerics::cohortCount(cdm$denom_reqs_any_time) %>%
                dplyr::filter(cohort_definition_id == 2) %>%
                dplyr::pull("number_records") == 1)
  expect_true(omopgenerics::cohortCount(cdm$denom_reqs_any_time) %>%
                dplyr::filter(cohort_definition_id == 3) %>%
                dplyr::pull("number_records") == 1)
  expect_true(omopgenerics::cohortCount(cdm$denom_reqs_any_time) %>%
                dplyr::filter(cohort_definition_id == 4) %>%
                dplyr::pull("number_records") == 0)

 # in all cases subject 1 should start on their target start "2012-01-05"
  expect_true(all(cdm$denom_reqs_any_time %>%
  dplyr::filter(subject_id == 1) %>%
  dplyr::pull("cohort_start_date") == as.Date("2012-01-05")))
  # in all cases subject 2 should start on their target start "2013-01-05"
  expect_true(all(cdm$denom_reqs_any_time %>%
                    dplyr::filter(subject_id ==2) %>%
                    dplyr::pull("cohort_start_date") == as.Date("2013-01-05")))


  ## Age
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 02L,
    day_of_birth = 02L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2005-01-01"),
    observation_period_end_date = as.Date("2018-06-01")
  )
  targetCohortTable <- dplyr::tibble(
    cohort_definition_id = 1L,
    subject_id = c(1L),
    cohort_start_date = as.Date("2010-01-01"),
    cohort_end_date = as.Date("2018-06-01")
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = targetCohortTable
  )
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = "denom_reqs_cohort_entry",
    ageGroup = list(c(10,100)),
    targetCohortTable = "target",
    targetCohortId = 1
  )
  # don´t enter
  # they don´t satisfy age req on cohort start date
  expect_true(cdm$denom_reqs_cohort_entry %>%
                dplyr::tally() %>%
                dplyr::pull("n") == 0)

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm,
    name = "denom_reqs_cohort_entry",
    ageGroup = list(c(09,100), c(10,100)),
    targetCohortTable = "target",
    targetCohortId = 1
  )
  # does enter
  # they satisfy age on cohort start date
  expect_true(omopgenerics::cohortCount(cdm$denom_reqs_cohort_entry) %>%
                dplyr::filter(cohort_definition_id == 1) %>%
                dplyr::pull("number_records") == 1)
  # but they won´t contribute to the next age cohort
  expect_true(omopgenerics::cohortCount(cdm$denom_reqs_cohort_entry) %>%
                dplyr::filter(cohort_definition_id == 2) %>%
                dplyr::pull("number_records") == 0)

  CDMConnector::cdmDisconnect(cdm)
})

test_that("mock db: check example with multiple observation periods", {
  skip_on_cran()
  # one person, two observation periods
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = rep(1L, 2L),
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
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  expect_true(nrow(cdm$denominator %>% dplyr::collect()) == 2)
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 2)
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_subjects == 1)

  # expect one rows- if start date is 1st Jan 2011
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2011-01-01"), as.Date(NA))
  )
  expect_true(nrow(cdm$denominator %>% dplyr::collect()) == 1)
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2011-01-01"))
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2011-06-01"))

  # expect one row- if start date is end of 2020
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date(NA), as.Date("2010-12-31"))
  )
  expect_true(nrow(cdm$denominator %>%
    dplyr::collect()) == 1)
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_start_date) == as.Date("2010-01-01"))
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::pull(cohort_end_date) == as.Date("2010-06-01"))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check imputation of date of birth", {
  skip_on_cran()
  # one person with all info, one missing month, one missing day, and one both
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L, 3L, 4L),
    gender_concept_id = as.integer(rep(8507, 4)),
    year_of_birth = as.integer(rep(2000, 4)),
    month_of_birth = as.integer(c(03, NA, 03, NA)),
    day_of_birth = as.integer(c(03, 03, NA, NA))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L, 4L),
    person_id = c(1L, 2L, 3L, 4L),
    observation_period_start_date = rep(as.Date("2010-01-01"), 4),
    observation_period_end_date = rep(as.Date("2015-06-01"), 4)
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(10, 100))
  )
  expect_true(nrow(cdm$denominator %>%
    dplyr::collect()) == 4)

  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 1) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-03-03")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 2) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-01-03")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 3) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-03-01")) %>%
    dplyr::pull())
  expect_true(cdm$denominator %>%
    dplyr::collect() %>%
    dplyr::filter(subject_id == 4) %>%
    dplyr::summarise(check = cohort_start_date == as.Date("2010-01-01")) %>%
    dplyr::pull())

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check edge cases (zero results expected)", {
  skip_on_cran()
  # one person, one observation periods
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = 1L,
    person_id = 1L,
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2015-06-01")
  )
  # mock database
  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  expect_warning(cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    cohortDateRange = c(as.Date("2100-01-01"), as.Date(NA))
  ))
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 0)

  expect_warning( cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    cohortDateRange = c(as.Date(NA), as.Date("1800-01-01"))
  ))
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 0)

  expect_warning(cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(c(155, 200))
  ))
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 0)

  # note could include people as it would go up to day before first birthday
  # but given observation period, here we would expect a null
  expect_warning(cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(c(0, 1))
  ))
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 0)

  expect_warning(cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(c(0, 15)),
    daysPriorObservation = 365000
  ))
  expect_true(omopgenerics::cohortCount(cdm$denominator)$number_records == 0)

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check expected errors", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef()

  # not a cdm reference
  expect_error(generateDenominatorCohortSet(
    cdm = "a", name = "denominator"
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(-2, 1))
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(0, -1))
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    max_age = c(100, 110)
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = "Men"
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    daysPriorObservation = -30
  ))

  # name must be lower snake case
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "DENOM"
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "MyDenom"
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm, name = "denom!!"
  ))

  # no person table
  cdm1 <- cdm
  cdm1$person <- NULL
  expect_error(generateDenominatorCohortSet(
    cdm = cdm1, name = "denominator"
  ))
  # no observation_period table
  cdm1 <- cdm
  cdm1$observation_period <- NULL
  expect_error(generateDenominatorCohortSet(
    cdm = cdm1, name = "denominator"
  ))
  expect_error(generateDenominatorCohortSet(
    cdm = cdm1, name = "denominator"
  ))
  # no target table
  cdm1 <- cdm
  cdm1$target <- NULL
  expect_error(generateDenominatorCohortSet(
    cdm = cdm1, name = "denominator",
    targetCohortTable = "target"
  ))

  # target table doesn´t conform
  targetCohortTable <- dplyr::tibble(
    cohort_id = "1",
    id = c("1", "2"),
    start_date = as.Date("2012-06-06"),
    end_date = as.Date("2013-06-06")
  )
  expect_error(mockIncidencePrevalenceRef(targetCohortTable = targetCohortTable))

  CDMConnector::cdm_disconnect(cdm)

})

test_that("mock db: check attrition table logic", {
  skip_on_cran()
  # 7 person, 1 observation periods
  personTable <- dplyr::tibble(
    person_id = as.integer(c(1, 2, 3, 4, 5, 6, 7)),
    gender_concept_id = as.integer(c(8507, 8532, 8507, 8532, 8532, 8507, NA)),
    year_of_birth = as.integer(c(1995, 1993, 1994, 1996, 1998, NA, 1993)),
    month_of_birth = as.integer(c(07, 02, 06, 05, 04, 10, 01)),
    day_of_birth = as.integer(c(25, 14, 01, 02, 03, 10, 12))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = as.integer(c(1, 2, 3, 4, 5, 6, 7)),
    person_id = as.integer(c(1, 2, 3, 4, 5, 6, 7)),
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
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  # check last n_current equals the number of rows of the denominator pop
  expect_true(nrow(cdm$denominator %>% dplyr::collect()) ==
    omopgenerics::attrition(cdm$denominator)$number_records[7])

  # check missings
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  expect_true(omopgenerics::attrition(cdm$denominator)$excluded_records[2] == 1)
  expect_true(omopgenerics::attrition(cdm$denominator)$excluded_records[3] == 1)

  # check sex criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = "Male"
  )
  expect_true(nrow(cdm$denominator %>% dplyr::collect()) ==
    tail(omopgenerics::attrition(cdm$denominator)$number_records, 1))
  expect_true(omopgenerics::attrition(cdm$denominator) %>%
    dplyr::filter(reason == "Not Male") %>%
    dplyr::pull("excluded_records") == 3)

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    sex = "Female"
  )
  expect_true(nrow(cdm$denominator %>% dplyr::collect()) ==
    tail(omopgenerics::attrition(cdm$denominator)$number_records, 1))
  expect_true(omopgenerics::attrition(cdm$denominator) %>%
    dplyr::filter(reason == "Not Female") %>%
    dplyr::pull("excluded_records") == 2)

  # check age criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(c(24, 25))
  )
  expect_true(omopgenerics::attrition(cdm$denominator)$excluded_records[3] == 1)

  # check observation criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2010-01-01"), as.Date("2012-01-01"))
  )
  expect_true(omopgenerics::attrition(cdm$denominator)$excluded_records[5] == 2)

  # check prior observation criteria
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2015-01-01"), as.Date("2016-06-30")),
    daysPriorObservation = 365
  )
  expect_true(omopgenerics::attrition(cdm$denominator)$excluded_records[7] == 1)
  CDMConnector::cdm_disconnect(cdm)

  # multiple observation periods per person
  personTable <- dplyr::tibble(
    person_id = 1L,
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 06L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L, 3L),
    person_id = 1L,
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
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  expect_true(all(
    omopgenerics::attrition(cdm$denominator)$number_records == 3))
  expect_true(all(
    omopgenerics::attrition(cdm$denominator)$number_subjects == 1))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check attrition with multiple cohorts", {
  skip_on_cran()

  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = c(8507L, 8532L),
    year_of_birth = as.integer(rep(2000, 2)),
    month_of_birth = as.integer(rep(01, 2)),
    day_of_birth = as.integer(rep(01, 2))
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(as.Date("2000-01-01"), as.Date("2010-01-01")),
    observation_period_end_date = c(as.Date("2012-06-01"), as.Date("2012-06-01"))
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable
  )

  cdm <- generateDenominatorCohortSet(cdm,name = "denominator",
    sex = c("Male", "Female", "Both")
  )

  # for male cohort we should have a row for those excluded for not being male
  expect_true(any("Not Male" == omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == TRUE)
  expect_true(any("Not Female" == omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)
  # for female cohort we should have a row for those excluded for not being male
  expect_true(any("Not Male" == omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)
  expect_true(any("Not Female" == omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == TRUE)
  # for both cohort we should have a row for those excluded for not being male
  expect_true(any("Not Male" == omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)
  expect_true(any("Not Female" == omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::pull(.data$reason)) == FALSE)

  cdm <- generateDenominatorCohortSet(cdm,name = "denominator",
    daysPriorObservation = c(0, 1000)
  )

  # nobody dropped for prior hist when req is 0
  expect_true(omopgenerics::settings(cdm$denominator) %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::filter(days_prior_observation == 0) %>%
    dplyr::filter(reason == "No observation time available after applying age, prior observation and, if applicable, target criteria") %>%
    dplyr::pull(.data$excluded_records) == 0)
  # some people dropped for prior hist when req is 1000
  expect_true(omopgenerics::settings(cdm$denominator) %>%
    dplyr::inner_join(omopgenerics::attrition(cdm$denominator),
      multiple = "all",
      by = "cohort_definition_id"
    ) %>%
    dplyr::filter(days_prior_observation == 1000) %>%
    dplyr::filter(reason == "No observation time available after applying age, prior observation and, if applicable, target criteria") %>%
    dplyr::pull(.data$excluded_records) > 0)


  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: check tables were cleaned up", {
  skip_on_cran()

  cdm <- mockIncidencePrevalenceRef(sampleSize = 10)

  startTables <- CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
                                          schema = attr(cdm, "write_schema")
  )

  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "my_denominator",
    ageGroup = list(
      c(0, 10), c(11, 20),
      c(21, 30), c(31, 40),
      c(41, 50), c(51, 60)
    ),
    daysPriorObservation = c(0, 1, 2)
  )
  start_cohort_set <- omopgenerics::settings(cdm$my_denominator)
  endTables <- CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
                                        schema = attr(cdm, "write_schema")
  )

  # we should only have added one temp table that contains our cohorts
  # all intermediate permanent tables created along the way should have been dropped

  expect_identical(
    sort(c(
      startTables, "my_denominator", "my_denominator_set",
      "my_denominator_attrition", "my_denominator_codelist"
    )),
    sort(c(endTables))
  )

  # reconnect
  cdmReconn <- CDMConnector::cdm_from_con(
    con = attr(attr(cdm, "cdm_source"), "dbcon"),
    cohort_tables = c("my_denominator"),
    cdm_schema = "main",
    write_schema = "main", cdm_name = "mock"
  )
  expect_true(is.data.frame(omopgenerics::settings(cdmReconn$my_denominator)))
  expect_true(is.data.frame(omopgenerics::cohortCount(cdmReconn$my_denominator)))
  expect_true(is.data.frame(omopgenerics::attrition(cdmReconn$my_denominator)))

  expect_equal(start_cohort_set,
               omopgenerics::settings(cdmReconn$my_denominator))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: requirement interactions", {
  skip_on_cran()
  cdm <- mockIncidencePrevalenceRef(sampleSize = 100)

  cdm <- generateDenominatorCohortSet(cdm,
                                      name = "denominator",
    ageGroup = list(
      c(0, 100), c(0, 10),
      c(11, 15), c(16, 20)
    ),
    sex = c("Both", "Female", "Male"),
    daysPriorObservation = c(0, 30),
    requirementInteractions = TRUE
  )
  expect_true(nrow(omopgenerics::settings(cdm$denominator)) == 4 * 3 * 2)

  cdm <- generateDenominatorCohortSet(cdm,name = "denominator",
    ageGroup = list(
      c(0, 100), c(0, 10),
      c(11, 15), c(16, 20)
    ),
    sex = c("Both", "Female", "Male"),
    daysPriorObservation = c(0, 30),
    requirementInteractions = FALSE
  )
  expect_true(nrow(omopgenerics::settings(cdm$denominator)) == 7)

  # order matters
  # will use first value
  nrow(omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::filter(age_group == "0 to 100") %>%
    dplyr::filter(days_prior_observation == 0)) == 1

  nrow(omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::filter(age_group == "0 to 100") %>%
    dplyr::filter(days_prior_observation == 0)) == 1

  nrow(omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::filter(age_group == "11 to 15") %>%
    dplyr::filter(days_prior_observation == 0)) == 1
  nrow(omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Both") %>%
    dplyr::filter(age_group == "0 to 100") %>%
    dplyr::filter(days_prior_observation == 30)) == 1


  nrow(omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Female") %>%
    dplyr::filter(age_group == "11 to 15") %>%
    dplyr::filter(days_prior_observation == 0)) == 0

  nrow(omopgenerics::settings(cdm$denominator) %>%
    dplyr::filter(sex == "Male") %>%
    dplyr::filter(age_group == "0 to 100") %>%
    dplyr::filter(days_prior_observation == 30)) == 0

  CDMConnector::cdm_disconnect(cdm)
})

test_that("mock db: target time at risk", {

  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2005-01-01"),
      as.Date("2005-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2005-01-30"),
      as.Date("2005-01-30")
    )
  )

  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 2L),
    cohort_start_date = c(
      as.Date("2005-01-01"),
      as.Date("2005-01-01")
    ),
    cohort_end_date = c(
      as.Date("2005-01-30"),
      as.Date("2005-01-30")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX
  )

  # full time at risk
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(0, Inf)
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 2)
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_start_date") == "2005-01-01"))
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_end_date") == "2005-01-30"))
  expect_true(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk") == "0 to Inf")

  # specify time at risk
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(0, 15)) # could also be in a list
  )
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_start_date") == "2005-01-01"))
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_end_date") == "2005-01-16"))
  expect_true(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk") == "0 to 15")

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(20, Inf)
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 2)
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_start_date") == "2005-01-21"))
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_end_date") == "2005-01-30"))
  expect_true(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk") == "20 to Inf")

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(20, 25))
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 2)
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_start_date") == "2005-01-21"))
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_end_date") == "2005-01-26"))
  expect_true(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk") == "20 to 25")

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(20, 50))
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 2)
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_start_date") == "2005-01-21"))
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_end_date") == "2005-01-30"))
  expect_true(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk") == "20 to 50")

  # nobody in time at risk window
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(40, 50)
  )
  expect_true(nrow(cdm$denominator |>
         dplyr::collect()) == 0)
  expect_true(omopgenerics::cohortCount(cdm$denominator) |>
    dplyr::pull("number_records") == 0)

  # multiple inputs
  startTbl<- names(cdm)
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(0, 15),
                      c(16, 40),
                      c(41, Inf))
  )
  # endTbl <- names(cdm)
  # expect_identical(sort(startTbl), sort(c(endTbl, "denominator")))
  expect_identical(sort(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk")),
                sort(c("0 to 15", "16 to 40", "41 to Inf")))
  expect_true(nrow(omopgenerics::cohortCount(cdm$denominator)) == 3)
  expect_true(nrow(cdm$denominator |>
    dplyr::collect()) == 4)

  # with other inputs
  # requirement interactions only applies to demographics, not tar
  # so 6 cohorts whether true or false
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(0, 15),
                      c(16, 40),
                      c(41, Inf)),
    ageGroup = list(c(0,100),
                    c(0,110)),
    requirementInteractions = TRUE
  )
  expect_true(nrow(omopgenerics::settings(cdm$denominator)) == 6)

  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(0, 15),
                      c(16, 40),
                      c(41, Inf)),
    ageGroup = list(c(0,100),
                    c(0,110)),
    requirementInteractions = FALSE
  )
  expect_true(nrow(omopgenerics::settings(cdm$denominator)) == 6)



  # same test as before - should produce same result (no tables overwritten etc)
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(20, Inf)
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 2)
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_start_date") == "2005-01-21"))
  expect_true(all(cdm$denominator |>
                    dplyr::pull("cohort_end_date") == "2005-01-30"))
  expect_true(omopgenerics::settings(cdm$denominator) |>
                dplyr::pull("time_at_risk") == "20 to Inf")



  # input validation - expected errors
  expect_error(generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(40, NA)
  ))
  expect_error(generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(-10, 10)
  ))
  expect_error(generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(10, 5)
  ))
  expect_error(generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c(40)
  ))
  expect_error(generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = c("a", "b")
  ))


  CDMConnector::cdm_disconnect(cdm)

})

test_that("mock db: target time at risk - requirements applied at original index", {

  skip_on_cran()
  personTable <- dplyr::tibble(
    person_id = c(1L, 2L),
    gender_concept_id = 8507L,
    year_of_birth = 2000L,
    month_of_birth = 01L,
    day_of_birth = 01L
  )
  observationPeriodTable <- dplyr::tibble(
    observation_period_id = c(1L, 2L),
    person_id = c(1L, 2L),
    observation_period_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2000-01-01")
    ),
    observation_period_end_date = c(
      as.Date("2010-01-30"),
      as.Date("2010-01-30")
    )
  )

  # person 1 target cohort starts in 2000 and ends in 2005
  # person 2 target cohort starts in 2003 and ends in 2008
  conditionX <- dplyr::tibble(
    cohort_definition_id = c(1L, 1L),
    subject_id = c(1L, 2L),
    cohort_start_date = c(
      as.Date("2000-01-01"),
      as.Date("2003-01-01")
    ),
    cohort_end_date = c(
      as.Date("2005-01-30"),
      as.Date("2008-01-30")
    )
  )

  cdm <- mockIncidencePrevalenceRef(
    personTable = personTable,
    observationPeriodTable = observationPeriodTable,
    targetCohortTable = conditionX
  )

  # require 180 days prior obs
  # the prior obs requirement should be applied relative to target start
  # so only have the second individual
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(0, Inf)),
    daysPriorObservation = 180
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 1)
  expect_true(cdm$denominator |>
    dplyr::pull("subject_id") == 2)

  # time at risk is from one year onwards
  # we should still exclude subject 1, because we care about their original
  # cohort start date
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(365, Inf)),
    daysPriorObservation = 180
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 1)
  expect_true(cdm$denominator |>
                dplyr::pull("subject_id") == 2)

  # similarly, date requirement should apply relative to initial cohort start
  cdm <- generateTargetDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    targetCohortTable = "target",
    timeAtRisk = list(c(365, Inf)),
    cohortDateRange = as.Date(c("2000-06-01", NA))
  )
  expect_true(nrow(cdm$denominator |>
                     dplyr::collect()) == 1)
  expect_true(cdm$denominator |>
                dplyr::pull("subject_id") == 2)

  CDMConnector::cdm_disconnect(cdm)

})
