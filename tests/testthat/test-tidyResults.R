test_that("basic example result", {
  testthat::skip_on_cran()

  cdm <- mockIncidencePrevalence()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )
  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  expect_no_error(tidy_inc <- asIncidenceResult(inc))
  expect_true(inherits(tidy_inc, "tidy_incidence"))
  # no incidence estimates
  expect_warning(asIncidenceResult(prev))
  # if result has attrition but no estimates
  expect_no_error(asIncidenceResult(inc |>
                                      dplyr::filter(result_id == 2)))
  # if result has estimates but no attrition
  expect_no_error(asIncidenceResult(inc |>
                                      dplyr::filter(result_id == 1)))

  expect_no_error(tidy_prev <- asPrevalenceResult(prev))
  expect_true(inherits(tidy_prev, "tidy_prevalence"))
  # no prevalence estimates
  expect_warning(asPrevalenceResult(inc))
  # if result has attrition but no estimates
  expect_no_error(asPrevalenceResult(prev |>
                                       dplyr::filter(result_id == 2)))
  # if result has estimates but no attrition
  expect_no_error(asPrevalenceResult(prev |>
                                       dplyr::filter(result_id == 1)))


  # include metadata
  expect_true("package_version" %in%
                names(asIncidenceResult(inc, metadata = TRUE)))
  expect_true(!"package_version" %in%
                names(asIncidenceResult(inc, metadata = FALSE)))
  expect_true("package_version" %in%
                names(asPrevalenceResult(prev, metadata = TRUE)))
  expect_true(!"package_version" %in%
                names(asPrevalenceResult(prev, metadata = FALSE)))

  })

test_that("multiple analyses", {
  testthat::skip_on_cran()

  cdm <- mockIncidencePrevalence(sampleSize = 100,
                                 outPre = 0.2)

  cdm <- generateDenominatorCohortSet(cdm = cdm,sex = c("Both", "Male", "Female"),
                                      name = "denominator",
                                      cohortDateRange = as.Date(
                                        c("1993-01-01", "1998-01-01")
                                      ))

  inc_orig <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months"
  )

  cdm$denominator <- cdm$denominator %>%
    dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995,
                                             "first", "second")) %>%
    dplyr::compute(name = "denominator",
                   temporary = FALSE)

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    strata = list(c("my_strata"))
  )
  res <- omopgenerics::bind(inc, prev)

  expect_no_error(asIncidenceResult(res))
  expect_no_error(asPrevalenceResult(res))

})
