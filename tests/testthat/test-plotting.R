test_that("basic plots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  cdm <- mockIncidencePrevalenceRef(sampleSize = 100)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
  )

  # incidence plot
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome", minCellCount = 0
  )
  plot <- plotIncidence(inc)
  expect_true(ggplot2::is.ggplot(plot))

  # with a different x axis
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome", interval = "overall", minCellCount = 0
  )
  prev <- estimatePrevalence(
    cdm = cdm, interval = "years",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotIncidence(inc, x = "denominator_age_group")
  expect_true(ggplot2::is.ggplot(plot))

  # prevalence plot
  plot <- plotPrevalence(prev, x = "denominator_age_group")
  expect_true(ggplot2::is.ggplot(plot))

  # with a different x axis
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    cohortDateRange = c(as.Date("2010-01-01"), as.Date("2010-06-01")),
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  prev <- estimatePrevalence(
    cdm = cdm, interval = "years",
    denominatorTable = "denominator",
    outcomeTable = "outcome", minCellCount = 0
  )
  plot <- plotPrevalence(prev,
                         x = "denominator_age_group"
  )
  expect_true(ggplot2::is.ggplot(plot))

  # empty plot if an empty result
  expect_true(ggplot2::is.ggplot(
    plotIncidence(inc |>
    dplyr::filter(result_id == 0))))
  expect_true(ggplot2::is.ggplot(
    plotPrevalence(prev |>
                    dplyr::filter(result_id == 0))))

  # other result type
  expect_true(ggplot2::is.ggplot(
    plotIncidence(prev)))
  expect_true(ggplot2::is.ggplot(
    plotPrevalence(inc)))


  # mixed result types
  expect_true(ggplot2::is.ggplot(plotPrevalence(omopgenerics::bind(inc,
                     prev))))
  expect_true(ggplot2::is.ggplot(plotIncidence(omopgenerics::bind(inc,
                                                                   prev))))

  # imported result
  result_path <- tempdir("result_to_plot")
  result <- omopgenerics::bind(inc, prev)
  unique(omopgenerics::settings(result) |> dplyr::pull("min_cell_count")) |> as.numeric()

  omopgenerics::exportSummarisedResult(result, path = result_path, minCellCount = 0)
  result_imported <-  omopgenerics::importSummarisedResult(result_path)

  # expect_identical(
  #   plotIncidence(result),
  #   plotIncidence(result_imported))
  # expect_identical(
  #   plotPrevalence(result),
  #   plotPrevalence(result_imported))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("plot facets", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome", minCellCount = 0
  )
  plot_orig <- plotIncidence(inc, facet = "denominator_age_group")
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot_sr <- plotIncidence(inc, facet = "denominator_age_group")

  expect_true(ggplot2::is.ggplot(plot_sr))



  # multiple facet grouping
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    ),
    sex = c("Male", "Female")
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  plot <- plotIncidence(inc,
                        facet = c(
                          "denominator_age_group",
                          "denominator_sex"
                        )
  )
  expect_true(ggplot2::is.ggplot(plot))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("plot colour", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")
  cdm <- mockIncidencePrevalenceRef(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  plot <- plotIncidence(inc,
                        colour = "denominator_age_group",
                        colour_name = "Age group"
  )
  expect_true(ggplot2::is.ggplot(plot))

  # multiple grouping
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator",
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    ),
    sex = c("Male", "Female")
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  plot <- plotIncidence(inc,
                        colour = c(
                          "denominator_age_group",
                          "denominator_sex"
                        )
  )

  expect_true(ggplot2::is.ggplot(plot))

  CDMConnector::cdm_disconnect(cdm)
})

test_that("plot options", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
    ageGroup = list(c(0, 30),
                    c(31, 100))
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  plotOptions <- list('hideConfidenceInterval' = TRUE,
                      'facetNcols' = 1)
  plot <- plotIncidence(inc,
                        colour = "denominator_age_group",
                        colour_name = "Age group",
                        options = plotOptions)
  expect_true(ggplot2::is.ggplot(plot))

  # prevalence
  prev <- estimatePrevalence(
    cdm = cdm, interval = "years",
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    minCellCount = 0
  )

  plot <- plotPrevalence(prev,
                         colour = c("denominator_age_group",
                                    "denominator_sex"),
                         options = plotOptions)

  expect_true(ggplot2::is.ggplot(plot))

  CDMConnector::cdm_disconnect(cdm)
})


