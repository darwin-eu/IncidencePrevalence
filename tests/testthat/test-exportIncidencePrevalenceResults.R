test_that("writing results", {
  cdm <- mockIncidencePrevalenceRef()
  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)
  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  results <- gatherIncidencePrevalenceResults(cdm=cdm, resultList = list(prev))
  exportIncidencePrevalenceResults(
    result = results, zipName = "test",
    outputFolder = tempdir()
  )
  expect_true("test.zip" %in% list.files(tempdir()))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("writing results- expected errors", {
  cdm <- mockIncidencePrevalenceRef()
  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)
  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  results <- gatherIncidencePrevalenceResults(cdm=cdm,
                                              resultList = list(prev))
  # not a gathered result
  expect_error(exportIncidencePrevalenceResults(
    result = "a",
    zipName = "test_should_fail",
    outputFolder = tempdir()
  ))
  # zipName not length 1
  expect_error(exportIncidencePrevalenceResults(
    result = results,
    zipName = c("a", "test_should_fail"),
    outputFolder = tempdir()
  ))
  # outputFolder not a valid path
  expect_error(exportIncidencePrevalenceResults(
    result = results, zipName = "test_should_fail",
    outputFolder = "a"
  ))
  # outputFolder not an existing path
  expect_error(exportIncidencePrevalenceResults(
    result = results, zipName = "test_should_fail",
    outputFolder = file.path(tempdir(), "doesn_not_exist")
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
