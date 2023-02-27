test_that("full benchmark", {
  skip_on_cran()

  cdm <- mockIncidencePrevalenceRef(sampleSize = 100,
                                    earliestObservationStartDate = as.Date("2010-01-01") ,
                                    latestObservationStartDate = as.Date("2010-01-01"),
                                    minDaysToObservationEnd = 364,
                                    maxDaysToObservationEnd = 364,
                                    outPre = 0.1 )

  timings<-benchmarkIncidencePrevalence(cdm,
                                        outputFolder = tempdir(),
                                        fileName = "bench",
                                        verbose = TRUE)
  expect_true(tibble::is_tibble(timings))
  expect_true("bench.csv" %in% list.files(tempdir()))

  # using permanent tables
  timings_perm<-benchmarkIncidencePrevalence(cdm,
                                             tablePrefix = "stem",
                                        outputFolder = tempdir(),
                                        fileName = "bench_perm",
                                        verbose = TRUE)
  expect_true(tibble::is_tibble(timings_perm))
  expect_true("bench_perm.csv" %in% list.files(tempdir()))


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
  skip_on_cran()

  cdm <- mockIncidencePrevalenceRef(sampleSize = 100,
                                    earliestObservationStartDate = as.Date("2010-01-01") ,
                                    latestObservationStartDate = as.Date("2010-01-01"),
                                    minDaysToObservationEnd = 364,
                                    maxDaysToObservationEnd = 364,
                                    outPre = 0.1 )

  timings = benchmarkIncidencePrevalence(cdm,sample = NULL, nOutcomes = 2,
                                         prevOutcomes = c(0.1),
                                         analysisType = "only incidence",
                                         verbose = TRUE)

  expect_true("yearly incidence, 2 outcome(s)" %in% timings$task)
  expect_true("monthly incidence, 2 outcome(s)" %in% timings$task)
  expect_false("yearly point prevalence, 2 outcome(s)" %in% timings$task)

  timings = benchmarkIncidencePrevalence(cdm,sample = NULL, nOutcomes = 1,
                                         prevOutcomes = 0.15, analysisType = "all",
                                         verbose = TRUE)

  expect_false("yearly incidence, 2 outcome(s)" %in% timings$task)
  expect_true("monthly point prevalence, 1 outcome(s)" %in% timings$task)
  expect_true("yearly period prevalence, 1 outcome(s)" %in% timings$task)

  # expected errors
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2.2,
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 4,
                                            prevOutcomes = c(0.1,0.2),
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(-0.10,0.30),
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(0.10,310),
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(0.10,30),
                                            analysisType = 0,
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  expect_error(benchmarkIncidencePrevalence(cdm,
                                            nOutcomes = 2,
                                            prevOutcomes = c(0.10,30),
                                            analysisType = "prevalence",
                                            outputFolder = tempdir(),
                                            verbose = TRUE))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
