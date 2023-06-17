test_that("check working example with defaults", {
  db <- mockIncidencePrevalenceRef()

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

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check working example with outcome table", {
  skip_on_cran()
  outcomeTable <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05")
    )
  )

  db <- mockIncidencePrevalenceRef(outcomeTable = outcomeTable)

  expect_true(nrow(db$outcome %>%
    dplyr::collect()) == 1)

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

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("check working example sample size and outcome prevalence option", {
  skip_on_cran()
  db <- mockIncidencePrevalenceRef(sampleSize = 100, outPre = 0.2)

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

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
})

test_that("outcome varies by gender and age option", {
  skip_on_cran()
  db <- mockIncidencePrevalenceRef(
    sampleSize = 100,
    outPre = 0.2,
    genderBeta = -1,
    ageBeta = 1,
    intercept = -1
  )

  db2 <- mockIncidencePrevalenceRef(
    sampleSize = 100,
    outPre = 0.2,
    genderBeta = -1,
    ageBeta = 1
  )

  expect_true(nrow(db$person %>%
    dplyr::collect()) == 100)

  expect_true(nrow(db2$person %>%
    dplyr::collect()) == 100)

  expect_true(nrow(db$outcome %>%
    dplyr::collect()) != 20)

  expect_true(nrow(db2$outcome %>%
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

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
  DBI::dbDisconnect(attr(db2, "dbcon"), shutdown = TRUE)
})

test_that("multiple outcomes", {
  skip_on_cran()
  db <-
    mockIncidencePrevalenceRef(
      sampleSize = 200,
      outPre = 0.2,
      maxOutcomes = 1
    )
  db2 <-
    mockIncidencePrevalenceRef(
      sampleSize = 200,
      outPre = 0.2,
      maxOutcomes = 2
    )
  db3 <-
    mockIncidencePrevalenceRef(
      sampleSize = 200,
      outPre = 0.2,
      maxOutcomes = 3
    )
  db4 <-
    mockIncidencePrevalenceRef(
      sampleSize = 1,
      outPre = 1,
      maxOutcomes = 10
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

  DBI::dbDisconnect(attr(db, "dbcon"), shutdown = TRUE)
  DBI::dbDisconnect(attr(db2, "dbcon"), shutdown = TRUE)
  DBI::dbDisconnect(attr(db3, "dbcon"), shutdown = TRUE)
  DBI::dbDisconnect(attr(db4, "dbcon"), shutdown = TRUE)
})

test_that("check expected errors", {
  skip_on_cran()
  expect_error(
    mockIncidencePrevalenceRef(personTable = "x")
  )
  expect_error(
    mockIncidencePrevalenceRef(observationPeriodTable = "x")
  )
  expect_error(
    mockIncidencePrevalenceRef(outcomeTable = "x")
  )
  expect_error(
    mockIncidencePrevalenceRef(sampleSize = -1)
  )
  expect_error(
    mockIncidencePrevalenceRef(
      sampleSize = 100,
      outPre = -0.2
    )
  )
  expect_error(
    mockIncidencePrevalenceRef(
      earliestDateOfBirth = as.Date("2000-01-01"),
      latestDateOfBirth = as.Date("1999-01-01")
    )
  )
  expect_error(
    mockIncidencePrevalenceRef(
      earliestObservationStartDate = as.Date("2000-01-01"),
      latestObservationStartDate = as.Date("1999-01-01")
    )
  )
  expect_error(
    mockIncidencePrevalenceRef(
      minDaysToObservationEnd = 10,
      maxDaysToObservationEnd = 1
    )
  )
})
