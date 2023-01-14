test_that("full benchmark", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 100,
                                    earliestObservationStartDate = as.Date("2010-01-01") ,
                                    latestObservationStartDate = as.Date("2010-01-01"),
                                    minDaysToObservationEnd = 364,
                                    maxDaysToObservationEnd = 364,
                                    outPre = 0.1 )

  timings<-benchmarkIncidencePrevalence(cdm,
                                        outputFolder = tempdir(),
                                        verbose = TRUE)
  expect_true(tibble::is_tibble(timings))
  expect_true("IncidencePrevalenceBenchmark.csv" %in% list.files(tempdir()))

  # expected errors
  expect_error(benchmarkIncidencePrevalence(cdm="a",
                               outputFolder = tempdir(),
                               verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                               outputFolder = "not a path",
                               verbose = TRUE))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
