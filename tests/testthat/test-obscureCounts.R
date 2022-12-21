test_that("check for incidence", {
  inc <- tibble::tibble(
    incidence_analysis_id = c("1", "2", "3"),
    n_persons = c(4, 6, 200),
    n_events = c(3, 3, 100),
    person_days = 100,
    person_years = 1,
    incidence_100000_pys = c(1, 25, 50),
    incidence_100000_pys_95CI_lower = c(0.1, 20, 50),
    incidence_100000_pys_95CI_upper = c(10, 30, 50)
  )

  inc <- obscureCounts(inc, minCellCount = 5)

  expect_true(is.na(inc$n_persons[1]))
  expect_true(is.na(inc$person_days[1]))
  expect_true(is.na(inc$person_years[1]))
  expect_true(is.na(inc$n_events[1]))
  expect_true(is.na(inc$incidence_100000_pys[1]))
  expect_true(is.na(inc$incidence_100000_pys_95CI_lower[1]))
  expect_true(is.na(inc$incidence_100000_pys_95CI_upper[1]))

  expect_true(!is.na(inc$n_persons[2])) # should still report n_persons
  expect_true(!is.na(inc$person_days[2]))
  expect_true(!is.na(inc$person_years[2]))
  expect_true(is.na(inc$n_events[2]))
  expect_true(is.na(inc$incidence_100000_pys[2]))
  expect_true(is.na(inc$incidence_100000_pys_95CI_lower[2]))
  expect_true(is.na(inc$incidence_100000_pys_95CI_upper[2]))

  expect_true(!is.na(inc$n_persons[3]))
  expect_true(!is.na(inc$person_days[3]))
  expect_true(!is.na(inc$person_years[3]))
  expect_true(!is.na(inc$n_events[3]))
  expect_true(!is.na(inc$incidence_100000_pys[3]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_lower[3]))
  expect_true(!is.na(inc$incidence_100000_pys_95CI_upper[3]))
})

test_that("check for prevalence", {
  prev <- tibble::tibble(
    incidence_analysis_id = c("1", "2", "3"),
    n_population = c(4, 6, 100),
    n_cases = c(3, 4, 100),
    prevalence = c(0.05, 0.55, 0.55),
    prevalence_95CI_lower = c(0.1, 0.5, 0.5),
    prevalence_95CI_upper = c(0.15, 0.60, 0.60)
  )

  prev <- obscureCounts(prev)

  expect_true(is.na(prev$n_population[1]))
  expect_true(is.na(prev$n_cases[1]))
  expect_true(is.na(prev$prevalence[1]))
  expect_true(is.na(prev$prevalence_95CI_lower[1]))
  expect_true(is.na(prev$prevalence_95CI_upper[1]))

  expect_true(!is.na(prev$n_population[2]))
  expect_true(is.na(prev$n_cases[2]))
  expect_true(is.na(prev$prevalence[2]))
  expect_true(is.na(prev$prevalence_95CI_lower[2]))
  expect_true(is.na(prev$prevalence_95CI_upper[2]))

  expect_true(!is.na(prev$n_population[3]))
  expect_true(!is.na(prev$n_cases[3]))
  expect_true(!is.na(prev$prevalence[3]))
  expect_true(!is.na(prev$prevalence_95CI_lower[3]))
  expect_true(!is.na(prev$prevalence_95CI_upper[3]))
})
