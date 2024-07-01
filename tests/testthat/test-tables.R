test_that("test tables", {
  # cdm <- mockIncidencePrevalenceRef()
  #
  # cdm <- generateDenominatorCohortSet(cdm = cdm, name = "denominator")
  #
  # prev_period <- estimatePeriodPrevalence(
  #   cdm = cdm,
  #   denominatorTable = "denominator",
  #   outcomeTable = "outcome",
  #   summarisedResult = TRUE
  # )
  #
  # gt1 <- tablePrevalence(prev_period, prevalenceType = "period",
  #                        header = c("group", "strata"),
  #                        denominatorSettings = FALSE)
  # expect_true("gt_tbl" %in% class(gt1))
  # expect_true(all(colnames(gt1$`_data`) == c(
  #   'Database name', 'Outcome cohort name', 'Estimate name', 'Prevalence start date', 'Prevalence end date', '[header]Denominator cohort name\n[header_level]Denominator cohort 1'
  # )))
  # expect_true(all(unique(gt1$`_data`$`Estimate name`) == c(
  #   "Denominator (N)", "Outcome (N)", "Prevalence [95% CI]"
  # )))
  #
  # # denominator name false, outcome settings true
  # expect_message(
  #   tib1 <- tablePrevalence(
  #     prev_period,
  #     prevalenceType = "period",
  #     type = "tibble",
  #     denominatorName = FALSE,
  #     outcomeSettings = TRUE,
  #     header = c("group", "strata"),
  #     denominatorSettings = FALSE
  #   )
  # )
  # expect_true(all(colnames(tib1) == c('Database name', 'Outcome cohort name', 'Estimate name', 'Estimate value', 'Prevalence start date', 'Prevalence end date')))
  #
  # # no split strata
  # fx <- tablePrevalence(
  #   prev_period,
  #   prevalenceType = "period",
  #   denominatorName = FALSE,
  #   analysisSettings = TRUE,
  #   outcomeName = FALSE,
  #   header = "cdm_name",
  #   splitStrata = FALSE,
  #   type = "tibble",
  #   denominatorSettings = FALSE
  # )
  # expect_true(all(colnames(fx$body$dataset) == c(
  #   'Strata name', 'Strata level', 'Estimate name', 'Prevalence start date', 'Prevalence end date', 'Analysis type',
  #   'Analysis interval', 'Analysis complete database intervals', 'Analysis full contribution', 'Database name\nmock'
  # )))
  #
  # # estimate in header
  # fx2 <- tablePrevalence(
  #   prev_period,
  #   header = c("strata", "estimate"),
  #   splitStrata = TRUE,
  #   type = "flextable",
  #   prevalenceType = "period",
  #   denominatorSettings = FALSE
  # )
  # expect_true("flextable" %in% class(fx2))
  # expect_true(all(colnames(fx2$body$dataset) == c(
  #   'Database name', 'Denominator cohort name', 'Outcome cohort name', 'Prevalence start date', 'Prevalence end date', 'Denominator (N)', 'Outcome (N)', 'Prevalence [95% CI]'
  # )))
  #
  # # point prevalence
  # prev_point <- estimatePointPrevalence(
  #   cdm = cdm,
  #   denominatorTable = "denominator",
  #   outcomeTable = "outcome",
  #   summarisedResult = TRUE
  # )
  # gt2 <- tablePrevalence(prev_point, denominatorSettings = TRUE, prevalenceType = "point")
  # expect_true(all(colnames(gt2$`_data`) == c(
  #   'Database name', 'Denominator cohort name', 'Prevalence start date', 'Prevalence end date',
  #   'Denominator age group', 'Denominator sex', 'Denominator days prior observation', 'Denominator start date',
  #   'Denominator end date', 'Denominator target cohort name',
  #   '[header_level]Outcome cohort name\n[header_level]Cohort 1\n[header_level]Denominator (N)',
  #   '[header_level]Outcome cohort name\n[header_level]Cohort 1\n[header_level]Outcome (N)',
  #   '[header_level]Outcome cohort name\n[header_level]Cohort 1\n[header_level]Prevalence [95% CI]'
  # )))
  #
  # # incidence
  # cdm$denominator <- cdm$denominator %>%
  #   dplyr::mutate(my_strata = dplyr::if_else(year(cohort_start_date) < 1995,
  #                                            "first", "second")) %>%
  #   dplyr::compute()
  # inc <- estimateIncidence(
  #   cdm = cdm,
  #   denominatorTable = "denominator",
  #   outcomeTable = "outcome",
  #   interval = "months",
  #   strata = list(c("my_strata")),
  #   summarisedResult = TRUE
  # )
  # tableInc <- tableIncidence(inc,
  #                            outcomeName = FALSE,
  #                            outcomeSettings = FALSE,
  #                            type = "flextable",
  #                            header = c("group", "strata"),
  #                            denominatorSettings = FALSE)
  # expect_true("flextable" %in% class(tableInc))
  # expect_true(all(colnames(tableInc$body$dataset) == c(
  #   'Database name', 'Estimate name', 'Incidence start date', 'Incidence end date',
  #   'Denominator cohort name\nDenominator cohort 1\nMy strata\nOverall',
  #   'Denominator cohort name\nDenominator cohort 1\nMy strata\nFirst'
  # )))
  #
  #
  # # no split strata
  # expect_no_error(tableIncidence(inc, outcomeName = FALSE,
  #                                outcomeSettings = TRUE, type = "flextable"))
  #
  # CDMConnector::cdm_disconnect(cdm)
})
