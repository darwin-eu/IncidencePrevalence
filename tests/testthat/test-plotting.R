test_that("basic plots", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  # comptibility with ggplot2 v3.6.0
  get_labs <- function(x) x$labels
  if ("get_labs" %in% getNamespaceExports("ggplot2")) {
    get_labs <- ggplot2::get_labs
  }

  cdm <- mockIncidencePrevalence(sampleSize = 100)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
  )

  # incidence plot
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotIncidence(inc)
  expect_true(ggplot2::is_ggplot(plot))
  expect_equal(plot$data$date_years, c(2008, 2009))
  expect_true(get_labs(plot)$x == "Date (years)")
  expect_true(get_labs(plot)$y == "Incidence (100,000 person-years)")
  expect_true(get_labs(plot)$ymin == "incidence_100000_pys_95CI_lower")
  expect_true(get_labs(plot)$ymax == "incidence_100000_pys_95CI_upper")
  expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$breaks, c(2008, 2009))

  plot <- plotIncidencePopulation(inc)
  expect_true(ggplot2::is_ggplot(plot))
  expect_equal(plot$data$date_years, c(2008, 2009))
  expect_true(get_labs(plot)$x == "Date (years)")
  expect_true(get_labs(plot)$y == "Denominator count")
  expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$breaks, c(2008, 2009))

  plot <- plotIncidencePopulation(inc, y = "outcome_count", x = "incidence_end_date")
  expect_true(ggplot2::is_ggplot(plot))
  expect_equal(plot$data$date_years, c(2008, 2009))
  expect_true(get_labs(plot)$x == "Date (years)")
  expect_true(get_labs(plot)$y == "Outcome count")
  expect_equal(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$breaks, c(2008, 2009))

  plot <- plotIncidencePopulation(inc, y = "person_days")
  expect_true(ggplot2::is_ggplot(plot))
  # grouping cols
  expect_true(length(availableIncidenceGrouping(inc)) == 0)
  expect_equal(
    availableIncidenceGrouping(inc, TRUE),
    c(
      "cdm_name", "outcome_cohort_name", "denominator_age_group",
      "denominator_sex", "denominator_target_cohort_name", "denominator_days_prior_observation"
    )
  )
  inc <- inc |>
    dplyr::mutate(cdm_name = "prova") |>
    omopgenerics::bind(inc)
  expect_true(availableIncidenceGrouping(inc) == "cdm_name")

  # with a different x axis
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "overall"
  )
  plot <- plotIncidence(inc, x = "denominator_age_group")
  expect_true(ggplot2::is_ggplot(plot))
  expect_equal(get_labs(plot)$x,"Denominator age group")
  plot <- plotIncidence(inc)
  expect_true(ggplot2::is_ggplot(plot))
  expect_equal(get_labs(plot)$x,"Incidence start date")

  prev <- estimatePrevalence(
    cdm = cdm,
    interval = "years",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  # prevalence plot
  plot <- plotPrevalence(prev,
                         x = "prevalence_start_date",
                         facet = "denominator_age_group"
  )
  expect_true(ggplot2::is_ggplot(plot))
  plot <- plotPrevalencePopulation(prev, x = "denominator_age_group")
  expect_true(ggplot2::is_ggplot(plot))
  plot <- plotPrevalencePopulation(prev, colour = "denominator_age_group")
  expect_equal(get_labs(plot)$x,"Date (years)")
  expect_true(ggplot2::is_ggplot(plot))
  # grouping cols
  expect_true(availablePrevalenceGrouping(prev) == "denominator_age_group")
  expect_equal(
    availablePrevalenceGrouping(prev, TRUE),
    c(
      "cdm_name", "outcome_cohort_name", "denominator_age_group",
      "denominator_sex", "denominator_target_cohort_name", "denominator_days_prior_observation"
    )
  )
  prev <- prev |>
    dplyr::mutate(cdm_name = "prova") |>
    omopgenerics::bind(prev)
  expect_equal(availablePrevalenceGrouping(prev), c("cdm_name", "denominator_age_group"))

  # with a different x axis
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2010-01-01"), as.Date("2010-06-01")),
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  prev <- estimatePrevalence(
    cdm = cdm, interval = "weeks",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotPrevalence(prev)
  expect_true(ggplot2::is_ggplot(plot))
  expect_equal(get_labs(plot)$x,"Prevalence start date")

  # empty plot if an empty result
  expect_warning(expect_true(ggplot2::is_ggplot(
    plotIncidence(inc |>
                    dplyr::filter(result_id == 0))
  )))
  expect_warning(expect_true(ggplot2::is_ggplot(
    plotPrevalence(prev |>
                     dplyr::filter(result_id == 0))
  )))

  # other result type
  expect_warning(expect_true(ggplot2::is_ggplot(
    plotIncidence(prev)
  )))
  expect_warning(expect_true(ggplot2::is_ggplot(
    plotPrevalence(inc)
  )))

  # mixed result types
  expect_true(ggplot2::is_ggplot(plotPrevalence(
    omopgenerics::bind(
      inc,
      prev
    ),
    x = "denominator_age_group"
  )))
  expect_true(ggplot2::is_ggplot(plotIncidence(
    omopgenerics::bind(
      inc,
      prev
    ),
    x = "denominator_age_group"
  )))

  # imported result
  result_path <- file.path(tempdir(), "result_to_plot")
  dir.create(result_path, showWarnings = FALSE)
  result <- omopgenerics::bind(inc, prev)
  omopgenerics::exportSummarisedResult(result,
                                       path = result_path,
                                       minCellCount = 5
  )

  result <- result |> omopgenerics::suppress(5)
  result_imported <- omopgenerics::importSummarisedResult(result_path)

  expect_identical(
    plotIncidence(result, x = "denominator_age_group"),
    plotIncidence(result_imported, x = "denominator_age_group")
  )
  expect_identical(
    plotPrevalence(result, x = "denominator_age_group"),
    plotPrevalence(result_imported, x = "denominator_age_group")
  )

  # tidy first also gives plot
  tidy_inc <- asIncidenceResult(inc)
  tidy_prev <- asPrevalenceResult(prev)

  expect_true(ggplot2::is_ggplot(plotIncidence(tidy_inc)))
  expect_true(ggplot2::is_ggplot(plotPrevalence(tidy_prev)))

  # errors if plotting functions are called with tidy outputs from other result type
  expect_error(plotIncidence(tidy_prev))
  expect_error(plotPrevalence(tidy_inc))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("plot facets", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")
  cdm <- mockIncidencePrevalence(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
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
  plot_sr <- plotIncidence(inc, facet = "denominator_age_group")

  expect_true(ggplot2::is_ggplot(plot_sr))



  # multiple facet grouping
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
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
  expect_true(ggplot2::is_ggplot(plot))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("plot colour", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")
  cdm <- mockIncidencePrevalence(sampleSize = 1000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
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
                        colour = "denominator_age_group"
  )
  expect_true(ggplot2::is_ggplot(plot))

  # multiple grouping
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
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

  expect_true(ggplot2::is_ggplot(plot))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("plot options", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")
  cdm <- mockIncidencePrevalence(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,
    name = "denominator",
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
                        ymin = NULL, ymax = NULL
  )
  expect_true(ggplot2::is_ggplot(plot))

  # prevalence
  prev <- estimatePrevalence(
    cdm = cdm, interval = "years",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )

  plot <- plotPrevalence(prev,
                         colour = c(
                           "denominator_age_group",
                           "denominator_sex"
                         ),
                         ymin = NULL, ymax = NULL
  )

  expect_true(ggplot2::is_ggplot(plot))

  plot <- plotPrevalence(prev,
                         colour = c(
                           "denominator_age_group",
                           "denominator_sex"
                         ),
                         line = FALSE
  )
  expect_true(ggplot2::is_ggplot(plot))

  plot <- plotPrevalence(prev,
                         colour = c(
                           "denominator_age_group",
                           "denominator_sex"
                         ),
                         ribbon = TRUE,
                         point = FALSE
  )
  expect_true(ggplot2::is_ggplot(plot))

  omopgenerics::cdmDisconnect(cdm)
})

test_that("y axis", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("scales")

  cdm <- mockIncidencePrevalence(sampleSize = 100)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
  )

  # incidence plot
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotIncidence(inc, x = "denominator_age_group", y = "denominator_count", facet = "incidence_start_date")
  expect_true(ggplot2::is_ggplot(plot))

  plot <- plotIncidence(inc, line = FALSE, ribbon = TRUE, y = "person_days")
  expect_true(ggplot2::is_ggplot(plot))

  # prevalence plot
  prev <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotPrevalence(prev, x = "denominator_age_group", y = "outcome_count", facet = "denominator_sex")
  expect_true(ggplot2::is_ggplot(plot))


  omopgenerics::cdmDisconnect(cdm)
})
