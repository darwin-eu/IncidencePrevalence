test_that("full benchmark", {
  skip_on_cran()

  cdm <- mockIncidencePrevalence(
    sampleSize = 100,
    earliestObservationStartDate = as.Date("2010-01-01"),
    latestObservationStartDate = as.Date("2010-01-01"),
    minDaysToObservationEnd = 364,
    maxDaysToObservationEnd = 364,
    outPre = 0.1
  )

  timings <- benchmarkIncidencePrevalence(cdm)
  expect_true(is.data.frame(timings))
  expect_true("summarised_result" %in% class(timings))
  expect_equal(
    colnames(timings),
    colnames(omopgenerics::emptySummarisedResult())
  )

  timings_only_inc <- benchmarkIncidencePrevalence(cdm,
    analysisType = "only incidence"
  )
  expect_true(any(stringr::str_detect(timings_only_inc$group_level, "incidence")))
  expect_false(any(stringr::str_detect(timings_only_inc$group_level, "prevalence")))

  timings_only_prev <- benchmarkIncidencePrevalence(cdm,
    analysisType = "only prevalence"
  )
  expect_false(any(stringr::str_detect(timings_only_prev$group_level, "incidence")))
  expect_true(any(stringr::str_detect(timings_only_prev$group_level, "prevalence")))

  # expected errors
  expect_error(benchmarkIncidencePrevalence(
    cdm = "a"
  ))
  expect_error(benchmarkIncidencePrevalence(cdm,
    analysisType = "not an option"
  ))

  CDMConnector::cdmDisconnect(cdm)
})

test_that("check tables cleaned up", {
  skip_on_cran()
  cdm <- mockIncidencePrevalence(
    sampleSize = 100,
    earliestObservationStartDate = as.Date("2010-01-01"),
    latestObservationStartDate = as.Date("2010-01-01"),
    minDaysToObservationEnd = 364,
    maxDaysToObservationEnd = 364
  )

  start_tables <- CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
    schema = attr(attr(cdm, "cdm_source"), "write_schema")
  )

  timings <- benchmarkIncidencePrevalence(cdm)

  end_tables <- CDMConnector::listTables(attr(attr(cdm, "cdm_source"), "dbcon"),
    schema = attr(attr(cdm, "cdm_source"), "write_schema")
  )

  expect_equal(sort(start_tables), sort(end_tables))
  CDMConnector::cdmDisconnect(cdm)

  cdm <- mockIncidencePrevalence(
    sampleSize = 100,
    earliestObservationStartDate = as.Date("2010-01-01"),
    latestObservationStartDate = as.Date("2010-01-01"),
    minDaysToObservationEnd = 364,
    maxDaysToObservationEnd = 364
  )

  CDMConnector::cdmDisconnect(cdm)
})
