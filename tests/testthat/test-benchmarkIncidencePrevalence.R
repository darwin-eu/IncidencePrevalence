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

test_that("benchmark multiple outcomes, prevalences and analysis types", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 100,
                                    earliestObservationStartDate = as.Date("2010-01-01") ,
                                    latestObservationStartDate = as.Date("2010-01-01"),
                                    minDaysToObservationEnd = 364,
                                    maxDaysToObservationEnd = 364,
                                    outPre = 0.1 )

  timings = benchmarkIncidencePrevalence(cdm,sample = NULL, nOutcomes = 2,
                                         prevOutcomes = c(10,24), analysisType = 5,
                                         outputFolder = tempdir(), verbose = TRUE)

  expect_true("yearly incidence - typical denominator, 2 outcome(s)" %in% timings$task)
  expect_true("monthly incidence - typical denominator, 2 outcome(s)" %in% timings$task)
  expect_false("yearly point prevalence - typical denominator, 2 outcome(s)" %in% timings$task)

  timings = benchmarkIncidencePrevalence(cdm,sample = NULL, nOutcomes = 1,
                                         prevOutcomes = c(15), analysisType = 2,
                                         outputFolder = tempdir(), verbose = TRUE)

  expect_false("yearly incidence - typical denominator, 2 outcome(s)" %in% timings$task)
  expect_true("monthly point prevalence - typical denominator, 1 outcome(s)" %in% timings$task)
  expect_true("yearly period prevalence - typical denominator, 1 outcome(s)" %in% timings$task)

  # expected errors
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2.2,
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = 25,
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(-10,30),
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(10,310),
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(10,30),
                                            analysisType = 0,
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

