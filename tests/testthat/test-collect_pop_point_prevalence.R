test_that("mock db: checks on working example", {
  person <- tibble::tibble(
    person_id = "1",
    gender_concept_id = "8507",
    year_of_birth = 2000,
    month_of_birth = 01,
    day_of_birth = 01
  )
  observation_period <- tibble::tibble(
    observation_period_id = "1",
    person_id = "1",
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2012-06-01")
  )
  outcome <- tibble::tibble(
    cohort_definition_id = "1",
    subject_id = "1",
    cohort_start_date = c(
      as.Date("2008-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    ),
    cohort_end_date = c(
      as.Date("2008-02-05"),
      as.Date("2010-02-08"),
      as.Date("2010-02-20")
    )
  )

  cdm_ref <- generate_mock_incidence_prevalence_db(person = person,
                                                   observation_period = observation_period,
                                                   outcome = outcome)

  dpop <- collect_denominator_pops(cdm_ref = cdm_ref)
  dpop <- dpop$denominator_populations

  prev <- collect_pop_point_prevalence(
    cdm_ref = cdm_ref,
    table_name_outcomes = "outcome",
    cohort_ids_outcomes = "1",
    cohort_ids_denominator_pops = "1",
    study_denominator_pop = dpop
  )
  expect_true(nrow(prev[["prevalence_estimates"]]) >= 1)

  DBI::dbDisconnect(attr(cdm_ref, "dbcon"), shutdown = TRUE)
})

