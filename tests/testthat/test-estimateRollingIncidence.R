
test_that("estimateRollingIncidence fundamental manual check", {
  # Mock CDM with specific dates
  person <- dplyr::tibble(
    person_id = c(1, 2, 3),
    gender_concept_id = c(8507, 8532, 8507),
    year_of_birth = c(1980, 1985, 1990),
    month_of_birth = c(1, 1, 1),
    day_of_birth = c(1, 1, 1),
    race_concept_id = 0,
    ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1, 2, 3),
    person_id = c(1, 2, 3),
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2010-12-31"),
    period_type_concept_id = 0
  )
  
  # Denominator cohort entries (different dates)
  denominator <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1),
    subject_id = c(1, 2, 3),
    cohort_start_date = as.Date(c("2005-01-01", "2006-03-01", "2007-06-01")),
    cohort_end_date = as.Date(c("2010-12-31", "2010-12-31", "2010-12-31"))
  )
  
  # Outcome cohort (Person 1 has outcome at day 10, Person 2 at day 45, Person 3 no outcome)
  outcome <- dplyr::tibble(
    cohort_definition_id = c(1, 1),
    subject_id = c(1, 2),
    cohort_start_date = as.Date(c("2005-01-11", "2006-04-15")), # P1: +10 days, P2: +45 days
    cohort_end_date = as.Date(c("2005-01-11", "2006-04-15"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )
  cdm <- omopgenerics::insertTable(cdm, name = "denominator", table = denominator)
  cdm$denominator <- omopgenerics::newCohortTable(cdm$denominator)
  cdm <- omopgenerics::insertTable(cdm, name = "outcome", table = outcome)
  cdm$outcome <- omopgenerics::newCohortTable(cdm$outcome)
  
  res <- suppressMessages(estimateRollingIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    nIntervals = 2
  ))

  res_ir <- omopgenerics::settings(res) |> dplyr::inner_join(res, by = "result_id") |> dplyr::filter(result_type == "rolling_incidence")
    
  # Month 1 check (Days 0-29)
  # P1 outcome is at day 10 -> Should be counted here
  # P2 outcome is at day 45 -> Not counted here
  m1_outcomes <- res_ir |>
    dplyr::filter(additional_level == "1 &&& 0 &&& 29 &&& rolling_months") |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m1_outcomes, 1)

  m1_denoms <- res_ir |>
    dplyr::filter(additional_level == "1 &&& 0 &&& 29 &&& rolling_months") |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m1_denoms, 3)

  # Month 2 check (Days 30-59)
  # P2 outcome is at day 45 -> Should be counted here
  m2_outcomes <- res_ir |>
    dplyr::filter(additional_level == "2 &&& 30 &&& 59 &&& rolling_months") |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m2_outcomes, 1)
})

test_that("estimateRollingIncidence intervals and censoring", {
  person <- dplyr::tibble(
    person_id = c(1),
    gender_concept_id = c(8507),
    year_of_birth = c(1980), month_of_birth = c(1), day_of_birth = c(1),
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1), person_id = c(1),
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2010-12-31"), period_type_concept_id = 0
  )
  
  # Only 45 days in denominator
  denominator <- dplyr::tibble(
    cohort_definition_id = c(1), subject_id = c(1),
    cohort_start_date = as.Date(c("2005-01-01")),
    cohort_end_date = as.Date(c("2005-02-14")) # 44 days inclusive (45 total)
  )
  outcome <- dplyr::tibble(
    cohort_definition_id = c(1), subject_id = c(1),
    cohort_start_date = as.Date(c("2005-02-14")),
    cohort_end_date = as.Date(c("2005-02-14"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )
  cdm <- omopgenerics::insertTable(cdm, name = "denominator", table = denominator)
  cdm$denominator <- omopgenerics::newCohortTable(cdm$denominator)
  cdm <- omopgenerics::insertTable(cdm, name = "outcome", table = outcome)
  cdm$outcome <- omopgenerics::newCohortTable(cdm$outcome)

  res <- suppressMessages(estimateRollingIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = c("weeks", "months"),
    nIntervals = 3,
    repeatedEvents = TRUE
  ))

  res_ir <- omopgenerics::settings(res) |> dplyr::inner_join(res, by = "result_id") |> dplyr::filter(result_type == "rolling_incidence")

  # Weeks 
  # P1 has 45 days. Weeks are 7 days.
  # Week 1: 7 days
  # Week 6: 7 days (42 total)
  # Week 7: 3 days
  w1_pdays <- res_ir |>
    dplyr::filter(additional_level == "1 &&& 0 &&& 6 &&& rolling_weeks") |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(w1_pdays, 7)

  # Months
  # Month 1: 30 days
  # Month 2: 15 days
  m1_pdays <- res_ir |>
    dplyr::filter(additional_level == "1 &&& 0 &&& 29 &&& rolling_months") |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m1_pdays, 30)

  m2_pdays <- res_ir |>
    dplyr::filter(additional_level == "2 &&& 30 &&& 59 &&& rolling_months") |>
    dplyr::filter(estimate_name == "person_days") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m2_pdays, 15)

  # Month 3: Should not have this person in denominator
  m3_denoms <- res_ir |>
    dplyr::filter(additional_level == "3 &&& 60 &&& 89 &&& rolling_months") |>
    dplyr::filter(estimate_name == "denominator_count") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_length(m3_denoms, 0)
})

test_that("estimateRollingIncidence repeated events and washout", {
  person <- dplyr::tibble(
    person_id = c(1),
    gender_concept_id = c(8507),
    year_of_birth = c(1980), month_of_birth = c(1), day_of_birth = c(1),
    race_concept_id = 0, ethnicity_concept_id = 0
  )
  observation_period <- dplyr::tibble(
    observation_period_id = c(1), person_id = c(1),
    observation_period_start_date = as.Date("2000-01-01"),
    observation_period_end_date = as.Date("2010-12-31"), period_type_concept_id = 0
  )
  
  # 2 years in denominator
  denominator <- dplyr::tibble(
    cohort_definition_id = c(1), subject_id = c(1),
    cohort_start_date = as.Date(c("2005-01-01")),
    cohort_end_date = as.Date(c("2006-12-31")) 
  )
  
  outcome <- dplyr::tibble(
    cohort_definition_id = c(1, 1, 1), 
    subject_id = c(1, 1, 1),
    # Outcome 1: Day 31 (Month 2 in rolling months)
    # Outcome 2: Day 90 (Inside 180 day washout of Outcome 1)
    # Outcome 3: Day 273 (Outside 180 day washout of Outcome 2)
    cohort_start_date = as.Date(c("2005-02-01", "2005-04-01", "2005-10-01")),
    cohort_end_date = as.Date(c("2005-02-01", "2005-04-01", "2005-10-01"))
  )

  cdm <- omopgenerics::cdmFromTables(
    tables = list("person" = person, "observation_period" = observation_period),
    cdmName = "test"
  )
  cdm <- omopgenerics::insertTable(cdm, name = "denominator", table = denominator)
  cdm$denominator <- omopgenerics::newCohortTable(cdm$denominator)
  cdm <- omopgenerics::insertTable(cdm, name = "outcome", table = outcome)
  cdm$outcome <- omopgenerics::newCohortTable(cdm$outcome)

  res <- suppressMessages(estimateRollingIncidence(
    cdm = cdm,
    denominatorTable = "denominator",
    outcomeTable = "outcome",
    interval = "months",
    nIntervals = 12,
    outcomeWashout = 180,
    repeatedEvents = TRUE
  ))

  res_ir <- omopgenerics::settings(res) |> dplyr::inner_join(res, by = "result_id") |> dplyr::filter(result_type == "rolling_incidence")

  # Outcome 1: 2005-02-01 is 31 days after 2005-01-01.
  # Month 2 (Days 30-59). Outcome should be 1.
  m2_outcomes <- res_ir |>
    dplyr::filter(additional_level == "2 &&& 30 &&& 59 &&& rolling_months") |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m2_outcomes, 1)

  # Outcome 2: 2005-04-01. 
  # This is 90 days after 2005-01-01 -> Month 4 (Days 90-119).
  # However, 2005-04-01 is only 59 days after Outcome 1. Washout is 180.
  # So it should be excluded.
  m4_outcomes <- res_ir |>
    dplyr::filter(additional_level == "4 &&& 90 &&& 119 &&& rolling_months") |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull(estimate_value)
    
  if (length(m4_outcomes) > 0) {
    m4_outcomes <- as.numeric(m4_outcomes)
  } else {
    m4_outcomes <- 0
  }
  expect_equal(m4_outcomes, 0)
  
  # Outcome 3: 2005-10-01.
  # This is 273 days after 2005-01-01 -> Month 10 (Days 270-299).
  # 2005-10-01 is 183 days after Outcome 2 (2005-04-01). 183 > 180.
  # Should be counted!
  m10_outcomes <- res_ir |>
    dplyr::filter(additional_level == "10 &&& 270 &&& 299 &&& rolling_months") |>
    dplyr::filter(estimate_name == "outcome_count") |>
    dplyr::pull(estimate_value) |> as.numeric()
  expect_equal(m10_outcomes, 1)
})
