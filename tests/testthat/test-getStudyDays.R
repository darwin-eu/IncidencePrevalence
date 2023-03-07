test_that("mock db: check point and period output correctly", {
  start <- as.Date("2010-01-01")

  end <- as.Date("2012-01-01")

  interval <- "years"

  period <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = TRUE
  )

  point <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "start"
  )

  expect_true(all(period$time == c("2010", "2011")))

  expect_true(all(period$start_time == c(
    as.Date("2010-01-01"),
    as.Date("2011-01-01")
  )))

  expect_true(all(period$end_time == c(
    as.Date("2010-12-31"),
    as.Date("2011-12-31")
  )))

  expect_true(all(point$time == c("2010", "2011", "2012")))

  expect_true(all(point$start_time == c(
    as.Date("2010-01-01"),
    as.Date("2011-01-01"),
    as.Date("2012-01-01")
  )))

  expect_true(all(point$end_time == point$start_time))


  point_middle <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "middle"
  )

  expect_true(all(point_middle$start_time == c("2010-07-01", "2011-07-01")))

  point_end <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "end"
  )

  expect_true(all(point_end$start_time == c("2010-12-31", "2011-12-31")))

  interval <- "quarters"

  period <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = TRUE
  )

  point <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "start"
  )

  expect_true(all(period$time == c(
    "2010_Q1", "2010_Q2", "2010_Q3",
    "2010_Q4", "2011_Q1", "2011_Q2",
    "2011_Q3", "2011_Q4"
  )))

  expect_true(all(point$time == c(
    "2010_Q1", "2010_Q2", "2010_Q3",
    "2010_Q4", "2011_Q1", "2011_Q2",
    "2011_Q3", "2011_Q4", "2012_Q1"
  )))


  point_middle <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "middle"
  )

  expect_true(all(point_middle$start_time == c(
    "2010-02-15", "2010-05-15",
    "2010-08-15", "2010-11-15",
    "2011-02-15", "2011-05-15",
    "2011-08-15", "2011-11-15"
  )))

  point_end <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "end"
  )

  expect_true(all(point_end$start_time == c(
    "2010-03-31", "2010-06-30",
    "2010-09-30", "2010-12-31",
    "2011-03-31", "2011-06-30",
    "2011-09-30", "2011-12-31"
  )))

  start <- as.Date("2010-01-01")

  end <- as.Date("2010-03-02")

  interval <- "months"

  period <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = TRUE
  )

  point <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "start"
  )

  expect_true(all(period$time == c("2010_01", "2010_02")))

  expect_true(all(point$time == c("2010_01", "2010_02", "2010_03")))


  point_middle <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "middle"
  )

  expect_true(all(point_middle$start_time == c("2010-01-15", "2010-02-15")))

  point_end <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "end"
  )

  expect_true(all(point_end$start_time == c("2010-01-31", "2010-02-28")))

  interval <- "weeks"

  period <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = TRUE
  )

  point <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "start"
  )

  point_end <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    type = "point",
    timePoint = "end"
  )
  expect_true(all(period$time == c(
    "2010_01", "2010_02",
    "2010_03", "2010_04",
    "2010_05", "2010_06",
    "2010_07", "2010_08"
  )))

  expect_true(all(period$start_time == c(
    "2010-01-04", "2010-01-11",
    "2010-01-18", "2010-01-25",
    "2010-02-01", "2010-02-08",
    "2010-02-15", "2010-02-22"
  )))

  expect_true(all(point$time == c(
    "2009_53", "2010_01", "2010_02", "2010_03", "2010_04",
    "2010_05", "2010_06", "2010_07", "2010_08"
  )))

  expect_true(all(point$start_time == c(
    "2010-01-04", "2010-01-11",
    "2010-01-18", "2010-01-25",
    "2010-02-01", "2010-02-08",
    "2010-02-15", "2010-02-22",
    "2010-03-01"
  )))

  expect_true(all(point$end_time == point$start_time))

  expect_true(all(point_end$time == c(
    "2009_52", "2009_53", "2010_01", "2010_02",
    "2010_03", "2010_04", "2010_05", "2010_06", "2010_07"
  )))

  expect_true(all(point_end$start_time == c(
    "2010-01-03", "2010-01-10", "2010-01-17", "2010-01-24",
    "2010-01-31", "2010-02-07", "2010-02-14", "2010-02-21", "2010-02-28"
  )))
})



test_that("mock db: check period complete cohort or not", {
  start <- as.Date("2010-01-01")

  end <- as.Date("2012-01-01")

  interval <- "years"

  period <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = TRUE
  )

  period_incomplete <- getStudyDays(
    startDate = start,
    endDate = end,
    timeInterval = interval,
    completeDatabaseIntervals = FALSE
  )
  expect_true(all(period$time == c("2010", "2011")))

  expect_true(all(period_incomplete$time == c("2010", "2011", "2012")))
})
