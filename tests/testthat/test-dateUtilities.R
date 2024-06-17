test_that("test date functions with duckdb mock", {

  cdm <- mockIncidencePrevalenceRef(sampleSize = 5)

  # add days
  days_q <-  addDaysQuery(cdm,
                      variable = "observation_period_start_date",
                      number = c(1,2),
                      name_style = "a1_{number}",
                      type = "day")

  expect_equal(
    cdm$observation_period |>
    utils::head(5) |>
    dplyr::mutate(!!!days_q) |>
    dplyr::mutate(a1_1=as.Date(a1_1)) |>
    dplyr::pull("a1_1"),
   cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::days(1)) |>
      dplyr::mutate(a1_1=as.Date(a1_1)) |>
      dplyr::pull("a1_1"))

  # minus days
  days_q <-  minusDaysQuery(cdm,
                             variable = "observation_period_start_date",
                             number = -1,
                             name_style = "a1",
                             type = "day")

  expect_equal(
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::mutate(!!!days_q) |>
      dplyr::mutate(a1=as.Date(a1)) |>
      dplyr::pull("a1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1 = observation_period_start_date - lubridate::days(1)) |>
      dplyr::mutate(a1=as.Date(a1)) |>
      dplyr::pull("a1"))


  # add years
  years_q <-  addDaysQuery(cdm,
                          variable = "observation_period_start_date",
                          number = c(1,2),
                          name_style = "a1_{number}",
                          type = "year")

  expect_equal(
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::mutate(!!!years_q) |>
      dplyr::mutate(a1_1=as.Date(a1_1)) |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::years(1)) |>
      dplyr::mutate(a1_1=as.Date(a1_1)) |>
      dplyr::pull("a1_1"))


  # minus years
  years_q <-  minusDaysQuery(cdm,
                 variable = "observation_period_start_date",
                 number = -1,
                 name_style = "a1",
                 type = "year")

  expect_equal(
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::mutate(!!!years_q) |>
      dplyr::mutate(a1=as.Date(a1)) |>
      dplyr::pull("a1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1 = observation_period_start_date - lubridate::years(1)) |>
      dplyr::mutate(a1=as.Date(a1)) |>
      dplyr::pull("a1"))

  # using names instead of name style
  years_q <-  minusDaysQuery(cdm,
                             variable = "observation_period_start_date",
                             number = -c(1,2),
                             names = c("a1_1", "a1_2"),
                             type = "year")

  expect_equal(
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::mutate(!!!years_q) |>
      dplyr::mutate(a1_1=as.Date(a1_1)) |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date - lubridate::years(1)) |>
      dplyr::mutate(a1_1=as.Date(a1_1)) |>
      dplyr::pull("a1_1"))

 # input validation
expect_error(addDaysQuery(cdm,
               variable = "observation_period_start_date",
               number = c(1,2),
               name_style = "a1_{number}",
               type = "other"))

CDMConnector::cdm_disconnect(cdm)

  })

