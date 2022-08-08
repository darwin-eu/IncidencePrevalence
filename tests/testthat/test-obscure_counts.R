test_that("check for incidence", {

  library(tibble)
  library(dplyr)

inc<-  tibble::tibble(
    incidence_analysis_id=c("1","2"),
    n_events=c(3,100),
    ir=c(1,25),
    ir_low= c(0.1,20),
    ir_high= c(10,30)
  )

inc<-  obscure_counts(inc)

expect_true(is.na(inc$n_events[1]))
expect_true(is.na(inc$ir[1]))
expect_true(is.na(inc$ir_low[1]))
expect_true(is.na(inc$ir_high[1]))

expect_true(!is.na(inc$n_events[2]))
expect_true(!is.na(inc$ir[2]))
expect_true(!is.na(inc$ir_low[2]))
expect_true(!is.na(inc$ir_high[2]))

})

test_that("check for prevalence", {

  library(tibble)
  library(dplyr)

  prev<-  tibble::tibble(
    incidence_analysis_id=c("1","2"),
    numerator=c(3,100),
    prev=c(0.05, 0.55),
    prev_low= c(0.1,0.5),
    prev_high= c(0.15,0.60)
  )

  prev<-  obscure_counts(prev)

  expect_true(is.na(prev$numerator[1]))
  expect_true(is.na(prev$prev[1]))
  expect_true(is.na(prev$prev_low[1]))
  expect_true(is.na(prev$prev_high[1]))

  expect_true(!is.na(prev$numerator[2]))
  expect_true(!is.na(prev$prev[2]))
  expect_true(!is.na(prev$prev_low[2]))
  expect_true(!is.na(prev$prev_high[2]))

})

