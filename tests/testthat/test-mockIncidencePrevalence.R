test_that("check working example with defaults", {
  skip_on_cran()
  db <- mockIncidencePrevalence(sampleSize = 10000, outPre = 0.5)
  cdmCheck <- inherits(db, "cdm_reference")
  expect_true(cdmCheck)

  expect_true(nrow(db$person %>%
    dplyr::collect()) >= 1)
  expect_true(nrow(db$observation_period %>%
    dplyr::collect()) >= 1)


  personDbNames <- c(
    "person_id", "gender_concept_id", "year_of_birth",
    "month_of_birth", "day_of_birth"
  )
  personNamesCheck <- all(personDbNames %in%
    names(db$person %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)))
  expect_true(personNamesCheck)

  obsPeriodNames <- c(
    "observation_period_id", "person_id",
    "observation_period_start_date", "observation_period_end_date"
  )
  obsPeriodNamesCheck <- all(obsPeriodNames %in%
    names(db$observation_period %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)))
  expect_true(obsPeriodNamesCheck)

  CDMConnector::cdm_disconnect(db)
})

test_that("check working example sample size and outcome prevalence option", {
  skip_on_cran()
  db <- mockIncidencePrevalence(sampleSize = 100,
                                   outPre = 0.2,
                                   earliestObservationStartDate = as.Date("2007-08-21"),
                                   latestObservationStartDate = as.Date("2007-08-21"),
                                   minDaysToObservationEnd = 1000)

  expect_true(nrow(db$person %>%
    dplyr::collect()) == 100)

  expect_true(nrow(db$outcome %>%
    dplyr::collect()) == 20)

  outcomeDbNames <- c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  )
  outcomeNamesCheck <- all(outcomeDbNames %in%
    names(db$outcome %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)))
  expect_true(outcomeNamesCheck)

  CDMConnector::cdm_disconnect(db)
})

test_that("multiple outcomes", {
  skip_on_cran()
  db <- mockIncidencePrevalence(
    sampleSize = 200,
    outPre = 0.2,
    maxOutcomes = 1,
    earliestObservationStartDate = as.Date("2007-08-21"),
    latestObservationStartDate = as.Date("2007-08-21"),
    minDaysToObservationEnd = 1000,
    maxDaysToObservationEnd = 1000
  )
  db2 <-
    mockIncidencePrevalence(
      sampleSize = 200,
      outPre = 0.2,
      maxOutcomes = 2,
      earliestObservationStartDate = as.Date("2007-08-21"),
      latestObservationStartDate = as.Date("2007-08-21"),
      minDaysToObservationEnd = 10000,
      maxDaysToObservationEnd = 10000
    )
  db3 <-
    mockIncidencePrevalence(
      sampleSize = 200,
      outPre = 0.2,
      maxOutcomes = 3,
      earliestObservationStartDate = as.Date("2007-08-21"),
      latestObservationStartDate = as.Date("2007-08-21"),
      minDaysToObservationEnd = 100000,
      maxDaysToObservationEnd = 100000
    )
  db4 <-
    mockIncidencePrevalence(
      sampleSize = 1,
      outPre = 1,
      maxOutcomes = 10,
      earliestObservationStartDate = as.Date("2007-08-21"),
      latestObservationStartDate = as.Date("2007-08-21"),
      minDaysToObservationEnd = 100000,
      maxDaysToObservationEnd = 100000
    )

  expect_true(nrow(db$outcome %>%
                     dplyr::collect()) == 40)

  expect_true(nrow(db2$outcome %>%
                     dplyr::collect()) > nrow(db$outcome %>%
                                                dplyr::collect()))

  expect_true(nrow(db3$outcome %>%
                     dplyr::collect()) > nrow(db2$outcome %>%
                                                dplyr::collect()))

  expect_true(
    nrow(
      db$outcome %>% dplyr::distinct(subject_id) %>% dplyr::collect()
    ) ==
      nrow(
        db2$outcome %>% dplyr::distinct(subject_id) %>% dplyr::collect()
      )
  )

  expect_true(
    nrow(
      db2$outcome %>% dplyr::distinct(subject_id) %>% dplyr::collect()
    ) ==
      nrow(
        db3$outcome %>% dplyr::distinct(subject_id) %>% dplyr::collect()
      )
  )

  # checking cohort_start_date of 2nd outcome comes after 1st outcome end date
  expect_true(
    db4$outcome %>%
      dplyr::filter(row_number() == 1) %>%
      dplyr::select(cohort_end_date) %>%
      dplyr::collect() <
      db4$outcome %>%
      dplyr::filter(row_number() == 2) %>%
      dplyr::select(cohort_start_date) %>%
      dplyr::collect()
  )



  outcomeDbNames <- c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  )
  outcomeNamesCheck <- all(outcomeDbNames %in%
                             names(db$outcome %>%
                                     utils::head(1) %>%
                                     dplyr::collect() %>%
                                     dplyr::rename_with(tolower)))
  expect_true(outcomeNamesCheck)

  CDMConnector::cdm_disconnect(db)
  CDMConnector::cdm_disconnect(db2)
  CDMConnector::cdm_disconnect(db3)
  CDMConnector::cdm_disconnect(db4)
})

test_that("check expected errors", {
  skip_on_cran()
  expect_error(
    mockIncidencePrevalence(personTable = "x")
  )
  expect_error(
    mockIncidencePrevalence(observationPeriodTable = "x")
  )
  expect_error(
    mockIncidencePrevalence(outcomeTable = "x")
  )
  expect_error(
    mockIncidencePrevalence(sampleSize = -1)
  )
  expect_error(
    mockIncidencePrevalence(
      sampleSize = 100,
      outPre = -0.2
    )
  )
  expect_error(
    mockIncidencePrevalence(
      earliestDateOfBirth = as.Date("2000-01-01"),
      latestDateOfBirth = as.Date("1999-01-01")
    )
  )
  expect_error(
    mockIncidencePrevalence(
      earliestObservationStartDate = as.Date("2000-01-01"),
      latestObservationStartDate = as.Date("1999-01-01")
    )
  )
  expect_error(
    mockIncidencePrevalence(
      minDaysToObservationEnd = 10,
      maxDaysToObservationEnd = 1
    )
  )
})

test_that("mockIncidencePrevalenceRef deprecated", {
expect_warning(
  db <- mockIncidencePrevalenceRef(sampleSize = 10)
)
})
