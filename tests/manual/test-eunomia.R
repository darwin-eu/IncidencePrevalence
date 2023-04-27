test_that("eunomia test - some empty cohorts", {

  db <- DBI::dbConnect(duckdb::duckdb(),
                       dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "main",
    write_schema = "main"
  )
  cdm$dpop <- generateDenominatorCohortSet(cdm)


# set of 3 cohorts, the last of which should be empty
  asthma_cohort1 <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009))),
    primaryCriteriaLimit = "First"
  ))
  asthma_cohort2 <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009)),
                    Capr::age(Capr::gte(18))),
    primaryCriteriaLimit = "First"
  ))
  asthma_cohort3 <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009)),
                    Capr::age(Capr::gte(180))),
    primaryCriteriaLimit = "First"
  ))


  path <- file.path(tempdir(), "asthma_cohorts")
  dir.create(path)

  Capr::writeCohort(asthma_cohort1, file.path(path, "asthma_cohort1.json"))
  Capr::writeCohort(asthma_cohort2, file.path(path, "asthma_cohort2.json"))
  Capr::writeCohort(asthma_cohort3, file.path(path, "asthma_cohort3.json"))

  asthma_cohort_set <- CDMConnector::readCohortSet(path = path)

  cdm <- CDMConnector::generateCohortSet(
    cdm,
    asthma_cohort_set,
    name = "asthma",
    computeAttrition = TRUE,
    overwrite = TRUE
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years"
  )
  expect_true(nrow(inc) > 0)

  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years"
  )
  expect_true(nrow(prev) > 0)

  })

test_that("eunomia test - strata", {

  # Update  to your database details as appropriate here
  db <- DBI::dbConnect(duckdb::duckdb(),
                       dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "main",
    write_schema = "main"
  )

  asthma_cohort1 <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009))),
    primaryCriteriaLimit = "First"
  ))
  asthma_cohort2 <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009)),
                    Capr::age(Capr::gte(18))),
    primaryCriteriaLimit = "First"
  ))


  path1 <- file.path(tempdir(), "outcome")
  dir.create(path1)
  Capr::writeCohort(asthma_cohort1, file.path(path1, "asthma_cohort1.json"))

  path2 <- file.path(tempdir(), "strata")
  dir.create(path2)
  Capr::writeCohort(asthma_cohort2, file.path(path2, "asthma_cohort2.json"))



  outcome_cohort_set <- CDMConnector::readCohortSet(path = path1)
  cdm <- CDMConnector::generateCohortSet(
    cdm,
    outcome_cohort_set,
    name = "outcome",
    computeAttrition = TRUE,
    overwrite = TRUE
  )

  strata_cohort_set <- CDMConnector::readCohortSet(path = path2)
  cdm <- CDMConnector::generateCohortSet(
    cdm,
    strata_cohort_set,
    name = "strata",
    computeAttrition = TRUE,
    overwrite = TRUE
  )


  cdm$dpop <- generateDenominatorCohortSet(cdm,
                                           strataTable = "strata")


  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    strataTable = "strata",
    interval = "years"
  )
  expect_true(nrow(inc) > 0)

  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years"
  )
  expect_true(nrow(prev) > 0)

})

test_that("eunomia test - participants", {

  db <- DBI::dbConnect(duckdb::duckdb(),
                       dbdir = CDMConnector::eunomia_dir())
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = "main",
    write_schema = "main"
  )
  cdm$dpop <- generateDenominatorCohortSet(cdm)
  asthma_cohort <- Capr::cohort(entry = Capr::entry(
    Capr::condition(Capr::cs(Capr::descendants(317009))),
    primaryCriteriaLimit = "First"
  ))

  path <- file.path(tempdir(), "asthma_cohorts_participants")
  dir.create(path)

  Capr::writeCohort(asthma_cohort, file.path(path, "asthma_cohort.json"))

  asthma_cohort_set <- CDMConnector::readCohortSet(path = path)

  cdm <- CDMConnector::generateCohortSet(
    cdm,
    asthma_cohort_set,
    name = "asthma",
    computeAttrition = TRUE,
    overwrite = TRUE
  )

  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years",
    returnParticipants = TRUE,
    tablePrefix = "test_inc"
  )
  expect_true(nrow(inc) > 0)
  participants(inc, 1)


  prev <- estimatePointPrevalence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "asthma",
    interval = "years"
  )
  expect_true(nrow(prev) > 0)


})
