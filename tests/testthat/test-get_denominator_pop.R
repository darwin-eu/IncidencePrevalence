test_that("output format", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
    package = "Eunomia"
  ), open = "rb"),
  exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\cdm.sqlite"))
  cdm_database_schema <- "main"

  result <- get_denominator_pop(
    db = db,
    cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Male", "Female"),
    days_prior_history = NULL,
    verbose = FALSE
  )

  # variable names
  expect_true(all(c(
    "person_id",
    "cohort_start_date", "cohort_end_date"
  ) %in%
    names(result)))
  DBI::dbDisconnect(db)
})




test_that("output format", {
  library(Eunomia)
  library(DBI)
  library(RSQLite)
  library(dplyr)
  untar(xzfile(system.file("sqlite", "cdm.tar.xz",
    package = "Eunomia"
  ), open = "rb"),
  exdir = tempdir()
  )
  db <- DBI::dbConnect(RSQLite::SQLite(), paste0(tempdir(), "\\cdm.sqlite"))
  cdm_database_schema <- "main"

  result <- get_denominator_pop(
    db = db,
    cdm_database_schema,
    start_date = NULL,
    end_date = NULL,
    min_age = NULL,
    max_age = NULL,
    sex = c("Male", "Female"),
    days_prior_history = NULL,
    verbose = FALSE
  )


  testthat::expect_true(!is.null(result$person_id) & sum(is.na(result$person_id)) == 0)
  testthat::expect_true(!is.null(result$cohort_start_date) & sum(is.na(result$cohort_start_date)) == 0)
  testthat::expect_true(!is.null(result$cohort_end_date) & sum(is.na(result$cohort_end_date)) == 0)

  DBI::dbDisconnect(db)
})
