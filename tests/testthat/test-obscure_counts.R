test_that("check for incidence", {
inc<-  tibble::tibble(
    incidence_analysis_id=c("1","2"),
    n_persons=c(4,200),
    n_events=c(3,100),
    person_days=100,
    person_years=1,
    ir_100000_pys=c(1,25),
    ir_100000_pys_low= c(0.1,20),
    ir_100000_pys_high= c(10,30)
  )

inc<-  obscure_counts(inc)

expect_true(is.na(inc$n_persons[1]))
expect_true(is.na(inc$person_days[1]))
expect_true(is.na(inc$person_years[1]))
expect_true(is.na(inc$n_events[1]))
expect_true(is.na(inc$ir_100000_pys[1]))
expect_true(is.na(inc$ir_100000_pys_low[1]))
expect_true(is.na(inc$ir_100000_pys_high[1]))

expect_true(!is.na(inc$n_persons[2]))
expect_true(!is.na(inc$person_days[2]))
expect_true(!is.na(inc$person_years[2]))
expect_true(!is.na(inc$n_events[2]))
expect_true(!is.na(inc$ir_100000_pys[2]))
expect_true(!is.na(inc$ir_100000_pys_low[2]))
expect_true(!is.na(inc$ir_100000_pys_high[2]))

})

test_that("check for prevalence", {
  prev<-  tibble::tibble(
    incidence_analysis_id=c("1","2"),
    denominator=c(4,100),
    numerator=c(3,100),
    prev=c(0.05, 0.55),
    prev_low= c(0.1,0.5),
    prev_high= c(0.15,0.60)
  )

  prev<-  obscure_counts(prev)

  expect_true(is.na(prev$denominator[1]))
  expect_true(is.na(prev$numerator[1]))
  expect_true(is.na(prev$prev[1]))
  expect_true(is.na(prev$prev_low[1]))
  expect_true(is.na(prev$prev_high[1]))

  expect_true(!is.na(prev$denominator[2]))
  expect_true(!is.na(prev$numerator[2]))
  expect_true(!is.na(prev$prev[2]))
  expect_true(!is.na(prev$prev_low[2]))
  expect_true(!is.na(prev$prev_high[2]))

})

