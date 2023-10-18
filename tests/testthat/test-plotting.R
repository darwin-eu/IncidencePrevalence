test_that("basic incidence plot", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotIncidence(inc)
  expect_true(ggplot2::is.ggplot(plot))

  # with a different x axis
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator", overwrite = TRUE,
    ageGroup = list(
      c(0, 30),
      c(31, 100)
    )
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome", interval = "overall"
  )
  plot <- plotIncidence(inc, x = "denominator_age_group")
  expect_true(ggplot2::is.ggplot(plot))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})

test_that("basic prevalence plot", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm, name = "denominator",
    cohortDateRange = c(as.Date("2008-01-01"), as.Date("2018-01-01"))
  )
  prev <- estimatePrevalence(
    cdm = cdm, interval = "years",
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  plot <- plotPrevalence(prev)
  expect_true(ggplot2::is.ggplot(plot))

  # with a different x axis
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator", overwrite = TRUE,
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

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


test_that("plot facets", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
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

  plot <- plotIncidence(inc, facet = "denominator_age_group")
  expect_true(ggplot2::is.ggplot(plot))

  # multiple facet grouping
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator", overwrite = TRUE,
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

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})


test_that("plot colour", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  cdm <- generateDenominatorCohortSet(
    cdm = cdm,name = "denominator", overwrite = TRUE,
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
    cdm = cdm,name = "denominator", overwrite = TRUE,
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

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
