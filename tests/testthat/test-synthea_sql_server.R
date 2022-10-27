library(testthat)
library(dplyr, warn.conflicts = FALSE)

test_that("test methods against test server", {
  skip_if(Sys.getenv("TESTDB_USER") == "")

  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = Sys.getenv("TESTDB_DRIVER"),
                        Server   = Sys.getenv("TESTDB_SERVER"),
                        Database = Sys.getenv("TESTDB_NAME"),
                        UID      = Sys.getenv("TESTDB_USER"),
                        PWD      = Sys.getenv("TESTDB_PWD"),
                        Port     = Sys.getenv("TESTDB_PORT"))

  # Write cohort to db
  # sql <- SqlRender::readSql(system.file("sql/sql_server/", "cohortsTestDb.sql", package = "IncidencePrevalence"))
  # sql <- SqlRender::render(sql = sql, cdmDatabaseSchema = Sys.getenv("TESTDB_CDM_SCHEMA"), resultsDatabaseSchema = Sys.getenv("TESTDB_WRITE_SCHEMA"))
  # sql <- SqlRender::translate(sql = sql, targetDialect = Sys.getenv("TESTDB_DBMS"))
  # DBI::dbSendQuery(con, sql)

  cdm <- CDMConnector::cdm_from_con(con,
                                    cdm_schema = Sys.getenv("TESTDB_CDM_SCHEMA"),
                                    write_schema = Sys.getenv("TESTDB_WRITE_SCHEMA"),
                                    cohort_tables = c("cohort"))

  dpop <- collect_denominator_pops(cdm = cdm,
                                   sample = 100)
  cdm$denominator <- dpop$denominator_populations

  expect_true(all(c(
    "age_strata","min_age","max_age",
    "sex_strata",
    "study_start_date",
    "study_end_date",
    "study_days_prior_history",
    "cohort_definition_id"
  ) %in%
    names(dpop$denominator_settings)))

  dpop <- dpop$denominator_populations %>% dplyr::collect()

  expect_true(all(c(
    "cohort_definition_id",
    "subject_id",
    "cohort_start_date",
    "cohort_end_date"
  ) %in%
    names(dpop)))

  # variable names
  expect_true(length(names(dpop)) == 4)
  expect_true(all(c(
    "cohort_definition_id", "subject_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(dpop)))

  # types
  expect_true(class(dpop %>% dplyr::select(cohort_definition_id) %>%
                      dplyr::pull()) == "character")
  expect_true(class(dpop %>% dplyr::select(subject_id) %>%
                      dplyr::pull()) == "integer")
  expect_true(class(dpop %>% dplyr::select(cohort_start_date) %>%
                      dplyr::pull())== "Date")
  expect_true(class(dpop %>% dplyr::select(cohort_end_date) %>%
                      dplyr::pull())== "Date")

  ## Pop incidence
  inc <- collect_pop_incidence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "cohort",
    cohort_ids_outcomes = "1",
    outcome_washout_windows = 0,
    repetitive_events = FALSE,
    time_interval = c("months"),
    confidence_interval = "none",
    verbose = TRUE
  )

  expect_true(class(inc) == "list")
  expect_true(all(names(inc) %in%
                    c("incidence_estimates",
                      "analysis_settings",
                      "person_table",
                      "attrition" )))

  # check analysis settings tibble
  expect_true(all(c(
    "incidence_analysis_id",
    "cohort_id_outcome",
    "cohort_id_denominator_pop",
    "outcome_washout_window",
    "repetitive_events",
    "time_interval",
    "confidence_interval",
    "minimum_cell_count"
  ) %in%
    names(inc[["analysis_settings"]])))

  # check estimates tibble
  expect_true(all(c(
    "incidence_analysis_id",
    "n_persons",
    "person_days",
    "person_years",
    "n_events",
    "ir_100000_pys",
    "ir_100000_pys_low",
    "ir_100000_pys_high",
    "time", "start_time","end_time",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(inc[["incidence_estimates"]])))

  ## Pop prevalence
  prev <- collect_pop_prevalence(
    cdm = cdm,
    table_name_denominator = "denominator",
    cohort_ids_denominator_pops = "1",
    table_name_outcomes = "cohort",
    cohort_ids_outcomes = "1",
    confidence_interval = "binomial"
  )

  expect_true(class(prev) == "list")
  expect_true(all(names(prev) %in%
                    c("prevalence_estimates",
                      "analysis_settings",
                      "person_table",
                      "attrition" )))

  # check analysis settings tibble::tibble
  expect_true(all(c(
    "prevalence_analysis_id",
    "type",
    "point",
    "time_interval",
    "minimum_representative_proportion",
    "full_periods_required",
    "cohort_id_outcome",
    "cohort_id_denominator_pop",
    "confidence_interval",
    "minimum_cell_count"
  ) %in%
    names(prev[["analysis_settings"]])))

  # check estimates tibble
  expect_true(all(c(
    "prevalence_analysis_id",
    "time",
    "numerator", "denominator",
    "prev",
    "prev_low",
    "prev_high",
    "start_time", "end_time",
    "cohort_obscured",
    "result_obscured"
  ) %in%
    names(prev[["prevalence_estimates"]])))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
