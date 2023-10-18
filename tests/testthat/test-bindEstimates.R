test_that("test input format errors", {
  expect_error(bindIncidenceEstimates())
  expect_error(bindPrevalenceEstimates())
  x <- 1
  expect_error(bindIncidenceEstimates(x))
  expect_error(bindPrevalenceEstimates(list(x)))
  class(x) <- "IncidenceResult"
  expect_error(bindIncidenceEstimates(x))
  class(x) <- "PrevalenceResult"
  expect_error(bindPrevalenceEstimates(x))
})

test_that("check with mock db", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 100)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
  )
  inc1 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  ) %>%
    dplyr::filter(dplyr::row_number() == 1)
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  ) %>%
    dplyr::filter(dplyr::row_number() == 1)
  expect_no_error(incCombined <- bindIncidenceEstimates(inc1, inc2))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)

  cdm <- mockIncidencePrevalenceRef(sampleSize = 100)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"),
      endDate = as.Date("2018-01-01")
    )
  )
  prev1 <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  prev2 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  expect_no_error(prevCombined <- bindPrevalenceEstimates(prev1, prev2))
  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("simple example", {
  x <- dplyr::tibble(incidence_result = 0.65, analysis_id = 1)
  attr(x, "settings") <- dplyr::tibble(analysis_id = 1, b = "a")
  attr(x, "attrition") <- dplyr::tibble(
    analysis_id = 1, denominator_cohort_name = "this"
  )
  class(x) <- c("IncidencePrevalenceResult", "IncidenceResult", class(x))
  y <- dplyr::tibble(incidence_result = c(0.45, 0.99, 0.1),
                     analysis_id = c(2, 3, 4))
  attr(y, "settings") <- dplyr::tibble(analysis_id = c(2, 3, 4),
                                       b = c("a", "b", "c"))
  attr(y, "attrition") <- dplyr::tibble(
    analysis_id = c(2, 3, 4),
    denominator_cohort_name = c("is", "a", "test")
  )
  class(y) <- c("IncidencePrevalenceResult", "IncidenceResult", class(y))
  expect_no_error(incCombined <- bindIncidenceEstimates(x, y))
  expect_error(incCombined <- bindPrevalenceEstimates(x, y))
  expect_true("IncidenceResult" %in% class(incCombined))
  expect_true("IncidencePrevalenceResult" %in% class(incCombined))
  expect_true("settings" %in% names(attributes(incCombined)))
  expect_true("attrition" %in% names(attributes(incCombined)))
  expectedInc <- dplyr::bind_rows(x, y, .id = "result_id")
  attr(expectedInc, "attrition") <- dplyr::bind_rows(
    attr(x, "attrition"), attr(y, "attrition"),
    .id = "result_id"
  )
  attr(expectedInc, "settings") <- dplyr::bind_rows(
    attr(x, "settings"), attr(y, "settings"),
    .id = "result_id"
  )
  expect_equal(incCombined, expectedInc)

  class(x) <- c("PrevalenceResult", class(x)[!(class(x) == "IncidenceResult")])
  class(y) <- c("PrevalenceResult", class(y)[!(class(y) == "IncidenceResult")])
  expect_no_error(prevCombined <- bindPrevalenceEstimates(x, y))
  expect_error(prevCombined <- bindIncidenceEstimates(x, y))
  expect_true("PrevalenceResult" %in% class(prevCombined))
  expect_true("IncidencePrevalenceResult" %in% class(prevCombined))
  expect_true("settings" %in% names(attributes(prevCombined)))
  expect_true("attrition" %in% names(attributes(prevCombined)))
  expectedPrev <- dplyr::bind_rows(x, y, .id = "result_id")
  attr(expectedPrev, "attrition") <- dplyr::bind_rows(
    attr(x, "attrition"), attr(y, "attrition"),
    .id = "result_id"
  )
  attr(expectedPrev, "settings") <- dplyr::bind_rows(
    attr(x, "settings"), attr(y, "settings"),
    .id = "result_id"
  )
  expect_equal(prevCombined, expectedPrev)
})
