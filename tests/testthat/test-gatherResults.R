test_that("check gathering of restuls", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev1 <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  prev2 <- estimatePeriodPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  inc1 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0
  )
  inc2 <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 180
  )

  # with both types of results
  g1<-gatherResults(resultList=list(prev1, prev2, inc1, inc2))
  expect_true(is.list(g1))
  expect_true(length(g1)==4)
  expect_true(all(names(g1)==c("prevalence_estimates","prevalence_attrition",
                               "incidence_estimates","incidence_attrition")))
  # with only prevalence
  g2<-gatherResults(resultList=list(prev1, prev2))
  expect_true(is.list(g2))
  expect_true(length(g2)==2)
  expect_true(all(names(g2)==c("prevalence_estimates","prevalence_attrition")))
  # with only incidence
  g3<-gatherResults(resultList=list(inc1))
  expect_true(is.list(g3))
  expect_true(length(g3)==2)
  expect_true(all(names(g3)==c("incidence_estimates","incidence_attrition")))

  # with database name
  g4<-gatherResults(resultList=list(prev1, prev2, inc1, inc2),
                    databaseName = "test_database")
  expect_true(all(g4$prevalence_estimates$database_name=="test_database"))

  # adding outcome name
  g5<-gatherResults(resultList=list(prev1, prev2, inc1, inc2),
                    outcomeCohortId = 1,
                    outcomeCohortName = "test_cohort",
                    databaseName = "test_database")
  expect_true(all(g5$prevalence_estimates$outcome_cohort_name == "test_cohort"))
  expect_true(all(g5$incidence_estimates$outcome_cohort_name == "test_cohort"))
  })

test_that("expected errors", {
  cdm <- mockIncidencePrevalenceRef()

  cdm$denominator <- generateDenominatorCohortSet(cdm = cdm)

  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome"
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    outcomeWashout = 0
  )
expect_error(gatherResults(resultList=list(prev, "a")))

})
