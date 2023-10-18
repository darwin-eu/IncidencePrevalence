test_that("writing results", {
  cdm <- mockIncidencePrevalenceRef()
  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  inc <- estimateIncidence(
    cdm = cdm,
    interval = "overall",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  exportIncidencePrevalenceResults(
    resultList = list("incidence_estimate" = inc), zipName = "test",
    outputFolder = tempdir()
  )
  expect_true("test.zip" %in% list.files(tempdir()))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("writing results- expected errors", {
  cdm <- mockIncidencePrevalenceRef()
  cdm <- generateDenominatorCohortSet(cdm = cdm,name = "denominator")
  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  # not a  result
  expect_error(exportIncidencePrevalenceResults(
    result = "a",
    zipName = "test_should_fail",
    outputFolder = tempdir()
  ))
  expect_error(exportIncidencePrevalenceResults(
    result = list(prev, "a"),
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
