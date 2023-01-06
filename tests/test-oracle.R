test_that("oracle test", {
  skip("failing test.")

  con <- DBI::dbConnect(odbc::odbc(), "OracleODBC-19")


  cdm <- CDMConnector::cdm_from_con(
    con = con,
    cdm_schema = "CDMV5",
    cdm_tables = c(CDMConnector::tbl_group("default"), -visit_detail) # visit_detail is missing in Oracle test database
  )

  # tables <- DBI::dbListTables(con)
  # tables <- tolower(tables)
  # "dbplyr_001" %in% tables
  # stringr::str_subset(tables, "dbplyr")
  # tables_to_remove <- stringr::str_subset(tables, "dbplyr")
  # purrr::walk(tables_to_remove, ~DBI::dbRemoveTable(con,.))

  # debugonce(generateDenominatorCohortSet)
  cdm$denominator <- generateDenominatorCohortSet(
    cdm = cdm,
    startDate = as.Date("2007-01-01"),
    ageGroup = list(
      c(40, 150),
      c(40, 64)
    ),
    sex = c("Male", "Female"),
    daysPriorHistory = 365,
    sample = 1000000,
    verbose = TRUE
  )

#   cdm$outcome <- cdm$denominator %>% head(10000) %>% CDMConnector::computeQuery()
#
#   pont_prev <- estimatePointPrevalence(
#     cdm = cdm,
#     denominatorTable = "denominator",
#     outcomeTable = "outcome",
#     verbose = TRUE
#   )
#   period_prev <- estimatePeriodPrevalence(completeDatabaseIntervals =FALSE,
#                                           cdm = cdm,
#                                           denominatorTable = "denominator",
#                                           outcomeTable = "outcome",
#                                           verbose = TRUE
#   )
#   inc <- estimateIncidence(
#     cdm = cdm,
#     denominatorTable = "denominator",
#     outcomeTable = "outcome",
#     verbose = TRUE
#   )
#   inc2 <- estimateIncidence(
#     cdm = cdm,
#     denominatorTable = "denominator",
#     outcomeTable = "outcome",
#     outcomeWashout = 180,
#     verbose = TRUE
#   )
#
#   results <- gatherIncidencePrevalenceResults(
#     resultList = list(pont_prev, period_prev, inc, inc2),
#     outcomeCohortName = "test_sample",
#     outcomeCohortId = 1,
#     databaseName = "test_database"
#   )
#   expect_true(all(names(results) == c(
#     "prevalence_estimates",
#     "incidence_estimates"
#   )))
#
#   DBI::dbDisconnect(con)
# })
#
# test_that("benchmark dementia analysis", {
#   skip_if(Sys.getenv("DB_SERVER_cdmgold202007_dbi") == "")
#
#   db <- DBI::dbConnect(RPostgres::Postgres(),
#                        dbname = Sys.getenv("DB_SERVER_cdmgold202007_dbi"),
#                        port = Sys.getenv("DB_PORT"),
#                        host = Sys.getenv("DB_HOST"),
#                        user = Sys.getenv("DB_USER"),
#                        password = Sys.getenv("DB_PASSWORD")
#   )
#   cdm <- CDMConnector::cdm_from_con(
#     con = db,
#     cdm_schema = "public"
#   )
#   outcome_cohorts <- CDMConnector::readCohortSet(here::here("inst",
#                                                             "outcome_cohorts"))
#   # run dementia
#   dementia_cohort <- outcome_cohorts %>%
#     dplyr::filter(cohortName == "dementia")
#   cdm <- CDMConnector::generateCohortSet(cdm, dementia_cohort,
#                                          cohortTableName = "incprevbench_dementia",
#                                          overwrite = TRUE
#   )
#   timings_simple_dementia <- benchmarkIncidencePrevalence(cdm,
#                                                           outcomeTableBench = "incprevbench_dementia",
#                                                           type = "simple",
#                                                           estimationInterval = "years",
#                                                           verbose = TRUE
#   )
#   timings_simple_dementia$outcome <- "dementia"
#
#   timings_typical_dementia <- benchmarkIncidencePrevalence(cdm,
#                                                            outcomeTableBench = "incprevbench_dementia",
#                                                            type = "typical",
#                                                            estimationInterval = "years",
#                                                            verbose = TRUE
#   )
#
#
#   # run paracetamol
#   paracetamol_cohort <- outcome_cohorts %>%
#     dplyr::filter(cohortName == "paracetamol")
#   cdm <- CDMConnector::generateCohortSet(cdm, paracetamol_cohort,
#                                          cohortTableName = "incprevbench_paracetamol",
#                                          overwrite = TRUE
#   )
#   timings_simple_paracetamol <- benchmarkIncidencePrevalence(cdm,
#                                                              outcomeTableBench = "incprevbench_paracetamol",
#                                                              type = "simple",
#                                                              estimationInterval = "years",
#                                                              verbose = TRUE
#   )
#   timings_simple_paracetamol$outcome <- "dementia"
#   timings_simple_paracetamol$nrow_outcome<- cdm$incprevbench_dementia %>%
#     dplyr::count() %>% dplyr::pull()



  # expect this to take a long time to run!
  # timings_simple<-benchmarkIncidencePrevalence(cdm,
  #                                                   type = c("simple",
  #                                                            "typical",
  #                                                            "complex"),
  #                                                   estimationInterval = c("weeks",
  #                                                                          "months",
  #                                                                          "quarters",
  #                                                                          "years"),
  #                                                   verbose = TRUE)

  DBI::dbDisconnect(con)
})
