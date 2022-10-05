test_that("sql_add_years", {

cdm <- generate_mock_incidence_prevalence_db()
dialect<-"postgresql"

# add 10
sql_duckdb<-sql_add_years(dialect=dialect,
             years_to_add=10,
             variable="observation_period_start_date")
obs<-cdm$observation_period %>%
  dplyr::mutate(observation_period_start_date1= as.Date(
    sql(sql_duckdb))) %>%
  dplyr::select("observation_period_start_date",
         "observation_period_start_date1")
expect_true(obs %>%
  dplyr::collect() %>%
  dplyr::mutate(
    observation_period_start_date2 =
      observation_period_start_date+years(10)) %>%
  dplyr::mutate(check=dplyr::if_else(
    observation_period_start_date1 ==
      observation_period_start_date2,
    TRUE, FALSE)) %>%
  dplyr::select("check") %>%
    dplyr::pull())

# add 0
sql_duckdb<-sql_add_years(dialect=dialect,
                         years_to_add=0,
                         variable="observation_period_start_date")
obs<-cdm$observation_period %>%
  dplyr::mutate(observation_period_start_date1= as.Date(
    sql(sql_duckdb))) %>%
  dplyr::select("observation_period_start_date",
                "observation_period_start_date1")
expect_true(obs %>%
              dplyr::collect() %>%
              dplyr::mutate(check=dplyr::if_else(
                observation_period_start_date ==
                  observation_period_start_date1,
                TRUE, FALSE)) %>%
              dplyr::select("check") %>%
              dplyr::pull())

# expect error if dialect is not supported

# expect error with decimal
expect_error(sql_add_years(dialect=dialect,
                         years_to_add=10.5,
                         variable="observation_period_start_date"))

})
