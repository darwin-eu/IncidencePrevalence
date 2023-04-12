test_that("eunomia test - some empty cohorts", {

  # Update  to your database details as appropriate here
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


