test_that("mock db working examples", {
  library(DBI)
  library(dplyr)
  library(tibble)

  # duckdb mock database
  db <- duckdb::dbConnect(duckdb::duckdb(), ":memory:")
  person <- tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2010-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    ),
    cohort_end_date = c(
      as.Date("2010-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    )
  )
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "person", person,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "observation_period", observation_period,
      overwrite = TRUE
    )
  })
  DBI::dbWithTransaction(db, {
    DBI::dbWriteTable(db, "outcome", outcome,
      overwrite = TRUE
    )
  })
  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL
  )
  get_pop_prevalence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    cohort_id_denominator_pop = "1",
    period = "Point",
    time_interval = c("Months"),
    minimum_representative_proportion = 0.5,
    confidence_interval = "exact",
    verbose = FALSE
  )

  dpop <- collect_denominator_pops(
    db = db,
    cdm_database_schema = NULL,
    study_age_stratas = list(c(0, 100), c(0, 100))
  )
  get_pop_prevalence(db,
    results_schema_outcome = NULL,
    table_name_outcome = "outcome",
    cohort_id_outcome = "1",
    study_denominator_pop = dpop,
    cohort_id_denominator_pop = "1",
    period = "Month",
    time_interval = c("Months"),
    minimum_representative_proportion = 0.5,
    confidence_interval = "exact",
    verbose = FALSE
  )
})
