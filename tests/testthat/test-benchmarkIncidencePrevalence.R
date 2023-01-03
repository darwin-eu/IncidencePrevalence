test_that("full benchmark", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 100,
                                    earliestObservationStartDate = as.Date("2010-01-01") ,
                                    latestObservationStartDate = as.Date("2010-01-01"),
                                    minDaysToObservationEnd = 364,
                                    maxDaysToObservationEnd = 364,
                                    outPre = 0.1 )

  timings_simple_year<-benchmarkIncidencePrevalence(cdm,
                                                    outcomeTableBench ="outcome",
                                                    type = c("simple"),
                                                    estimationInterval = c("years"),
                                                    verbose = TRUE)
  expect_true(tibble::is_tibble(timings_simple_year))
  # given our 100 person cdm with a year of follow up, this should all run quickly
  expect_true(all(timings_simple_year$time_taken_secs < 5))

  timings_typical_year<-benchmarkIncidencePrevalence(cdm,
                                                     outcomeTableBench ="outcome",
                                                     type = c("typical"),
                                                     estimationInterval = c("years"))
  expect_true(tibble::is_tibble(timings_typical_year))
  expect_true(all(timings_typical_year$time_taken_secs < 30))

  timings_complex_year<-benchmarkIncidencePrevalence(cdm,
                                                     outcomeTableBench ="outcome",
                                                      type = c("complex"),
                                                      estimationInterval = c("years"))
  expect_true(tibble::is_tibble(timings_complex_year))
  expect_true(all(timings_complex_year$time_taken_secs < 40))

  timings_simple_week_month_quarter<-benchmarkIncidencePrevalence(cdm,
                                                                  outcomeTableBench ="outcome",
                                                     type = c("simple"),
                                                     estimationInterval = c("weeks",
                                                                            "months",
                                                                            "quarters"))
  expect_true(tibble::is_tibble(timings_simple_week_month_quarter))
  expect_true(all(timings_simple_week_month_quarter$time_taken_secs < 30))


  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

})
