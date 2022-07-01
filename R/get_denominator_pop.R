
#' Identify a single denominator population
#'
#' @param db Database connection via DBI::dbConnect()
#' @param cdm_database_schema Name of the schema which contains the
#' omop cdm person and observation_period tables
#' @param start_date Date indicating the start of the study period. If NULL,
#'  the earliest observation_start_date in the observation_period table
#'  will be used.
#' @param end_date Date indicating the end of the study period. If NULL,
#'  the latest observation_end_date in the observation_period table
#'  will be used.
#' @param min_age Minimum age for the cohort
#' @param max_age Maximum age for the cohort.
#' @param sex Sex of the cohort
#' @param days_prior_history Days of prior history required to enter
#' the study cohort.
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#'
#' @return
#' @importFrom rlang .data
#' @export
#'
#' @examples
get_denominator_pop <- function(db,
                                cdm_database_schema,
                                start_date = NULL,
                                end_date = NULL,
                                min_age = NULL,
                                max_age = NULL,
                                sex = "Both",
                                days_prior_history = 0,
                                verbose = FALSE) {
  if (verbose == TRUE) {
    start <- Sys.time()
  }

  if (!is.null(start_date)) {
    if (is.na(start_date)) {
      start_date <- NULL
    }
  }
  if (!is.null(end_date)) {
    if (is.na(end_date)) {
      end_date <- NULL
    }
  }

  if (verbose == TRUE) {
    message("Progress: Checking inputs")
  }

  ## check for standard types of user error
  error_message <- checkmate::makeAssertCollection()
  db_inherits_check <- inherits(db, "DBIConnection")
  checkmate::assertTRUE(db_inherits_check,
    add = error_message
  )
  if (!isTRUE(db_inherits_check)) {
    error_message$push(
      "- db must be a database connection via DBI::dbConnect()"
    )
  }
  checkmate::assert_character(cdm_database_schema,
    add = error_message, null.ok = TRUE
  )
  checkmate::assert_date(start_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_date(end_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_numeric(min_age,
    add = error_message,
    null.ok = TRUE
  )
  if (length(min_age) >= 2) {
    error_message$push(
      "- min_age should be one value"
    )
  }
  checkmate::assert_numeric(max_age,
    add = error_message,
    null.ok = TRUE
  )
  if (length(max_age) >= 2) {
    error_message$push(
      "- max_age should be one value"
    )
  }
  checkmate::assert_character(sex,
    add = error_message
  )
  sex_check <- all(sex %in% c("Male", "Female", "Both"))
  if (!isTRUE(sex_check)) {
    error_message$push(
      "- sex must be one of Male, Female, or Both"
    )
  }
  if (length(sex) >= 2) {
    error_message$push(
      "- sex must be one of Male, Female, or Both"
    )
  }

  checkmate::assert_numeric(days_prior_history,
    add = error_message,
    null.ok = TRUE
  )
  days_check <- days_prior_history >= 0
  if (!isTRUE(days_check)) {
    error_message$push(
      "- days_prior_history cannot be negative"
    )
  }


  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  ## check person and observation_period tables exist
  # connect to relevant vocabulary tables
  # will return informative error if not found
  if (!is.null(cdm_database_schema)) {
    person_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
      "SELECT * FROM {cdm_database_schema}.person"
    )))
  } else {
    person_db <- tbl(db, "person")
  }

  if (!is.null(cdm_database_schema)) {
    observation_period_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
      "SELECT * FROM {cdm_database_schema}.observation_period"
    )))
  } else {
    observation_period_db <- tbl(db, "observation_period")
  }

  # make sure names are lowercase
  person_db <- dplyr::rename_with(person_db, tolower) %>%
    dplyr::compute()
  observation_period_db <- dplyr::rename_with(
    observation_period_db, tolower
  ) %>%
    dplyr::compute()


  # check variable names
  # person table
  person_db_names <- c(
    "person_id", "gender_concept_id", "year_of_birth",
    "month_of_birth", "day_of_birth"
  )
  person_db_names_check <- all(person_db_names %in%
    names(person_db %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)))
  checkmate::assertTRUE(person_db_names_check, add = error_message)

  # observation_period table
  obs_period_db_names <- c(
    "observation_period_id", "person_id",
    "observation_period_start_date", "observation_period_end_date"
  )
  obs_period_db_names_check <- all(obs_period_db_names %in%
    names(observation_period_db %>%
      utils::head(1) %>%
      dplyr::collect() %>%
      dplyr::rename_with(tolower)))
  checkmate::assertTRUE(obs_period_db_names_check, add = error_message)

  ## Identifying population of interest
  # Optional arguments to values
  if (is.null(start_date)) {
    start_date <- observation_period_db %>%
      dplyr::summarise(
        min(.data$observation_period_start_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(end_date)) {
    end_date <- observation_period_db %>%
      dplyr::summarise(
        max(.data$observation_period_end_date,
          na.rm = TRUE
        )
      ) %>%
      dplyr::collect() %>%
      dplyr::pull()
  }
  if (is.null(min_age)) {
    min_age <- 0
  }
  if (is.null(max_age)) {
    # arbitrarily set to 150
    max_age <- 150
  }

  # filtering on database side
  # drop anyone missing year_of_birth or gender_concept_id
  study_pop_db <- person_db %>%
    dplyr::left_join(observation_period_db,
      by = "person_id"
    ) %>%
    dplyr::filter(!is.na(.data$year_of_birth)) %>%
    dplyr::mutate(gender = ifelse(.data$gender_concept_id == "8507", "Male",
      ifelse(.data$gender_concept_id == "8532", "Female", NA)
    )) %>%
    dplyr::filter(!is.na(.data$gender)) %>%
    dplyr::compute()

  if (sex == "Male") {
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data$gender == "Male") %>%
      dplyr::compute()
  }
  if (sex == "Female") {
    study_pop_db <- study_pop_db %>%
      dplyr::filter(.data$gender == "Female") %>%
      dplyr::compute()
  }

  last_year <- lubridate::year(end_date) + 1
  earliest_year <- lubridate::year(start_date) - 1
  study_pop_db <- study_pop_db %>%
    # drop people too old even at study start
    dplyr::filter(.data$year_of_birth + max_age >= earliest_year) %>%
    # drop people too young even at study end
    dplyr::filter(.data$year_of_birth + min_age <= last_year) %>%
    # drop people with observation_period_star_date after study end
    dplyr::filter(.data$observation_period_start_date <= end_date) %>%
    # drop people with observation_period_end_date before study start
    dplyr::filter(.data$observation_period_end_date >= start_date) %>%
    dplyr::compute()

  ## bring in to memory and finalise population
  study_pop <- study_pop_db %>%
    dplyr::collect()

  if (nrow(study_pop) == 0) {
    message("-- No people found for denominator population")
    return(NULL)
  } else {

    # get date of birth
    study_pop <- study_pop %>%
      dplyr::mutate(dob = dplyr::if_else(
        is.na(.data$month_of_birth) |
          is.na(.data$day_of_birth),
        as.Date(
          paste(.data$year_of_birth,
            "06",
            "01",
            sep = "/"
          ),
          "%Y/%m/%d"
        ),
        as.Date(
          paste(.data$year_of_birth,
            .data$month_of_birth,
            .data$day_of_birth,
            sep = "/"
          ),
          "%Y/%m/%d"
        )
      ))

    study_pop <- study_pop %>%
      # Date at which they reach minimum and maximum age
      # (+1 to go to the end of the year)
      dplyr::mutate(
        date_min_age =
          lubridate::add_with_rollback(
            .data$dob,
            lubridate::years(min_age)
          )
      ) %>%
      dplyr::mutate(
        date_max_age =
          lubridate::add_with_rollback(
            .data$dob,
            lubridate::years((max_age + 1))
          )
      ) %>%
      # Date at which they reach
      # observation start date + prior_history requirement
      dplyr::mutate(
        date_with_prior_history =
          .data$observation_period_start_date +
            lubridate::days(days_prior_history)
      )

    # keep people only if
    # 1) they satisfy age criteria at some point in the study
    study_pop <- study_pop %>%
      dplyr::filter(.data$date_min_age <= end_date) %>%
      dplyr::filter(.data$date_max_age >= start_date)
    # 2) and they satisfy priory history criteria at some point in the study
    study_pop <- study_pop %>%
      dplyr::filter(.data$date_with_prior_history <= end_date) %>%
      dplyr::filter(.data$date_with_prior_history <=
        .data$observation_period_end_date)

    ## Get cohort start and end dates
    # Start date:
    # study start_date,
    # date_min_age,
    # date_with_prior_history
    # (whichever comes last)
    study_pop$cohort_start_date <- do.call(
      `pmax`,
      study_pop %>%
        dplyr::mutate(study_start_date = start_date) %>%
        dplyr::select(
          "study_start_date",
          "date_min_age",
          "date_with_prior_history"
        )
    )

    # End date:
    # study end date,
    # end of observation,
    # max.age
    # (whichever comes first)
    study_pop$cohort_end_date <- do.call(
      `pmin`,
      study_pop %>%
        dplyr::mutate(study_end_date = end_date) %>%
        dplyr::select(
          "study_end_date",
          "observation_period_end_date",
          "date_max_age"
        )
    )

    # Exclude people who are elegible after cohort_end_date
    study_pop <- study_pop %>%
      dplyr::filter(.data$cohort_start_date <=
        .data$cohort_end_date)

    # variables to keep
    study_pop <- study_pop %>%
      dplyr::select("person_id", "cohort_start_date", "cohort_end_date")


    if (verbose == TRUE) {
      duration <- abs(as.numeric(Sys.time() - start, units = "secs"))
      message(glue::glue(
        "Time taken: {floor(duration/60)} minutes and {duration %% 60 %/% 1} seconds"
      ))
    }

    if (nrow(study_pop) == 0) {
      message("-- No people found for denominator population")
      return(NULL)
    } else {
      return(study_pop)
    }
  }
}
