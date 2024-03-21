test_that("postgres test", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_POSTGRESQL_DBNAME") == "")

 db <- DBI::dbConnect(RPostgres::Postgres(),
            dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
            host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
            user = Sys.getenv("CDM5_POSTGRESQL_USER"),
            password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD"))
 cdm <- CDMConnector::cdm_from_con(
   con = db,
   cdm_schema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
   write_schema = c(schema =  Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA"),
                    prefix = "incp_")
 )


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
     dplyr::collect() |>
     dplyr::pull("a1_1"),
   cdm$observation_period |>
     utils::head(5) |>
     dplyr::collect() |>
     dplyr::mutate(a1_1 = observation_period_start_date + lubridate::days(1)) |>
     dplyr::mutate(a1_1 = as.Date(a1_1)) |>
     dplyr::collect() |>
     dplyr::pull("a1_1"))

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
     dplyr::collect() |>
     dplyr::pull("a1_1"),
   cdm$observation_period |>
     utils::head(5) |>
     dplyr::collect() |>
     dplyr::mutate(a1_1 = observation_period_start_date + lubridate::years(1)) |>
     dplyr::mutate(a1_1 = as.Date(a1_1)) |>
     dplyr::collect() |>
     dplyr::pull("a1_1"))


 expect_no_error(postgres_timings <- benchmarkIncidencePrevalence(cdm))
 expect_no_error(postgres_timings_participants <- benchmarkIncidencePrevalence(cdm))

 CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("redshift test", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_REDSHIFT_DBNAME") == "")

  db <- DBI::dbConnect(RPostgres::Redshift(),
                       dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
                       host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
                       port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
                       user     = Sys.getenv("CDM5_REDSHIFT_USER"),
                       password = Sys.getenv("CDM5_REDSHIFT_PASSWORD"))
  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
    write_schema = c(schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA"),
                     prefix = "incp_")
  )


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
      dplyr::collect() |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::days(1)) |>
      dplyr::mutate(a1_1 = as.Date(a1_1)) |>
      dplyr::collect() |>
      dplyr::pull("a1_1"))

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
      dplyr::collect() |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::years(1)) |>
      dplyr::mutate(a1_1 = as.Date(a1_1)) |>
      dplyr::collect() |>
      dplyr::pull("a1_1"))

  expect_no_error(redshift_timings <- benchmarkIncidencePrevalence(cdm))
  expect_no_error(redshift_timings_participants <- benchmarkIncidencePrevalence(cdm,
                                                                                returnParticipants = TRUE))

  CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("sql server test", {
  skip_on_cran()
  skip_if(Sys.getenv("CDM5_SQL_SERVER_SERVER") == "")

  db <- DBI::dbConnect(odbc::odbc(),
                       Driver   = Sys.getenv("SQL_SERVER_DRIVER"),
                       Server   = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
                       Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
                       UID      = Sys.getenv("CDM5_SQL_SERVER_USER"),
                       PWD      = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
                       TrustServerCertificate="yes",
                       Port     = Sys.getenv("CDM5_SQL_SERVER_PORT"))

  cdm <- CDMConnector::cdm_from_con(
    con = db,
    cdm_schema = strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]],
    write_schema = c(schema =  strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]],
                     prefix = "incp_")
  )

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
      dplyr::collect() |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::days(1)) |>
      dplyr::mutate(a1_1 = as.Date(a1_1)) |>
      dplyr::collect() |>
      dplyr::pull("a1_1"))

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
      dplyr::collect() |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::years(1)) |>
      dplyr::mutate(a1_1 = as.Date(a1_1)) |>
      dplyr::collect() |>
      dplyr::pull("a1_1"))

  expect_no_error(sql_server_timings <- benchmarkIncidencePrevalence(cdm))
  expect_no_error(sql_server_timings_participants <- benchmarkIncidencePrevalence(cdm,
                                                                                  returnParticipants = TRUE))

  CDMConnector::cdm_disconnect(cdm = cdm)

})

test_that("snowflake test", {
  skip_on_cran()
  skip_if(Sys.getenv("SNOWFLAKE_SERVER") == "")

  con <- DBI::dbConnect(odbc::odbc(),
                        SERVER = Sys.getenv("SNOWFLAKE_SERVER"),
                        UID = Sys.getenv("SNOWFLAKE_USER"),
                        PWD = Sys.getenv("SNOWFLAKE_PASSWORD"),
                        DATABASE = Sys.getenv("SNOWFLAKE_DATABASE"),
                        WAREHOUSE = Sys.getenv("SNOWFLAKE_WAREHOUSE"),
                        DRIVER = Sys.getenv("SNOWFLAKE_DRIVER"))

  cdm_schema <- strsplit(Sys.getenv("SNOWFLAKE_CDM_SCHEMA"), "\\.")[[1]]
  write_schema <- strsplit(Sys.getenv("SNOWFLAKE_SCRATCH_SCHEMA"), "\\.")[[1]]

  cdm <- CDMConnector::cdm_from_con(con = con,
                                    cdm_schema = cdm_schema,
                                    write_schema = write_schema,
                                    cdm_name = "snowflake")

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
      dplyr::collect() |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::days(1)) |>
      dplyr::mutate(a1_1 = as.Date(a1_1)) |>
      dplyr::collect() |>
      dplyr::pull("a1_1"))

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
      dplyr::collect() |>
      dplyr::pull("a1_1"),
    cdm$observation_period |>
      utils::head(5) |>
      dplyr::collect() |>
      dplyr::mutate(a1_1 = observation_period_start_date + lubridate::years(1)) |>
      dplyr::mutate(a1_1 = as.Date(a1_1)) |>
      dplyr::collect() |>
      dplyr::pull("a1_1"))


 expect_no_error(snowflake_timings <- benchmarkIncidencePrevalence(cdm))
 expect_no_error(snowflake_timings_participants <- benchmarkIncidencePrevalence(cdm,
                                                                                returnParticipants = TRUE))

  CDMConnector::cdm_disconnect(cdm = cdm)

})
