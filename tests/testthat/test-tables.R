test_that("test tablePrevalence", {
  cdm <- mockIncidencePrevalenceRef()

  cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")

  prev_period <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    summarisedResult = TRUE,
  )

  default <- tablePrevalence(prev_period, prevalenceType = "period")
  expect_true("gt_tbl" %in% class(default))
  expect_true(all(colnames(default$`_data`) == c(
    'Database name', 'Outcome cohort name', 'Estimate name', 'Start date', 'End date', '[header]Denominator cohort name\n[header_level]Denominator cohort 1'
  )))
  expect_true(all(unique(default$`_data`$`Estimate name`) == c(
    "Denominator (N)", "Outcome (N)", "Prevalence [95% CI]"
  )))

  # denominator name false, outcome settings true
  expect_message(
    tib1 <- tablePrevalence(prev_period,
                          prevalenceType = "period",
                          type = "tibble",
                          denominatorName = FALSE,
                          outcomeSettings = TRUE)
  )
  expect_true(all(colnames(tib1) == c('Database name', 'Outcome cohort name', 'Estimate name', 'Estimate value', 'Start date', 'End date')))

  # no split strata
  fx <- tablePrevalence(
    prev_period,
    prevalenceType = "period",
    denominatorName = FALSE,
    analysisSettings = TRUE,
    outcomeName = FALSE,
    header = "cdm_name",
    splitStrata = FALSE,
    type = "flextable"
  )
  expect_true(all(colnames(fx$body$dataset) == c(
    'Strata name', 'Strata level', 'Estimate name', 'Start date', 'End date', 'Analysis type',
    'Analysis interval', 'Analysis complete database intervals', 'Analysis full contribution', 'Database name\nmock'
    )))

  # estimate in header
  fx2 <- tablePrevalence(
    prev_period,
    header = c("strata", "estimate"),
    splitStrata = TRUE,
    type = "flextable",
    prevalenceType = "period"
  )
  expect_true(all(colnames(fx2$body$dataset) == c(
    'Database name', 'Denominator cohort name', 'Outcome cohort name', 'Start date', 'End date', 'Denominator (N)', 'Outcome (N)', 'Prevalence [95% CI]'
  )))

  # point prevalence
  prev_period <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    summarisedResult = TRUE
  )
  gt2 <- tablePrevalence(prev_period, denominatorSettings = TRUE, prevalenceType = "point")
  expect_true(all(colnames(gt2$`_data`) == c(
    'Database name', 'Outcome cohort name', 'Estimate name', 'Start date', 'End date', 'Denominator age group',
    'Denominator sex', 'Denominator days prior observation', 'Denominator start date', 'Denominator end date',
    'Denominator target cohort name', '[header]Denominator cohort name\n[header_level]Denominator cohort 1'
  )))
})
