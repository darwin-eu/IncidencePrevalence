#' Identify a set of denominator populations
#'
#' @param db Database connection via DBI::dbConnect()
#' @param cdm_database_schema Name of the schema which contains the
#' omop cdm person and observation_period tables
#' @param study_start_date Date indicating the start of the study
#' period. If NULL,
#'  the earliest observation_start_date in the observation_period table
#'  will be used.
#' @param study_end_date Date indicating the end of the study
#' period. If NULL,
#'  the latest observation_end_date in the observation_period table
#'  will be used.
#' @param study_age_stratas List of age groups
#' @param study_sex_stratas Sex of the cohorts
#' @param study_days_prior_history Days of prior history required to enter
#' the study cohort.
#' @param verbose Either TRUE or FALSE.
#' If TRUE, progress will be reported.
#'
#' @return
#' @importFrom rlang .data
#' @export
#'
#' @examples
collect_denominator_pops <- function(db,
                                     cdm_database_schema,
                                     study_start_date = NULL,
                                     study_end_date = NULL,
                                     study_age_stratas = NULL,
                                     study_sex_stratas = "Both",
                                     study_days_prior_history = 0,
                                     verbose = FALSE) {

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
  checkmate::assert_date(study_start_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_date(study_end_date,
    add = error_message,
    null.ok = TRUE
  )
  # add check of age groups here
  checkmate::assert_vector(study_sex_stratas,
    add = error_message
  )
  sex_check <- all(study_sex_stratas %in% c("Male", "Female", "Both"))
  if (!isTRUE(sex_check)) {
    error_message$push(
      "- sex stratas must be one or more of: Male, Female, and Both"
    )
  }
  checkmate::assert_numeric(study_days_prior_history,
    add = error_message
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  # add broadest possible age group if NULL
  if (is.null(study_age_stratas)) {
    study_age_stratas <- list(c(0, 150))
  }
  # only allow multiple values of age and sex
  age_gr_df <- data.frame(do.call(rbind, study_age_stratas)) %>%
    dplyr::mutate(age_range = paste0(.data$X1, ";", .data$X2))
  pop_specs <- tidyr::expand_grid(
    age_range = age_gr_df$age_range,
    sex = study_sex_stratas,
    study_days_prior_history = study_days_prior_history,
    study_start_date = study_start_date,
    study_end_date = study_end_date
  ) %>%
    tidyr::separate(.data$age_range,
                    c("min_age", "max_age"),
                    remove = FALSE) %>%
    dplyr::mutate(min_age = as.numeric(.data$min_age)) %>%
    dplyr::mutate(max_age = as.numeric(.data$max_age))
  if (is.null(study_start_date)) {
    pop_specs$study_start_date <- as.Date(NA)
  }
  if (is.null(study_start_date)) {
    pop_specs$study_end_date <- as.Date(NA)
  }

  pop_specs <- pop_specs %>%
    dplyr::mutate(cohort_definition_id = as.character(dplyr::row_number()))

  # to list
  pop_specs <- split(
    pop_specs,
    pop_specs[, c("cohort_definition_id")]
  )
  # get each population
  study_populations <- lapply(pop_specs, function(x) {
    get_denominator_pop(
      db = db,
      cdm_database_schema = cdm_database_schema,
      start_date = x$study_start_date,
      end_date = x$study_end_date,
      min_age = x$min_age,
      max_age = x$max_age,
      sex = x$sex,
      days_prior_history = x$study_days_prior_history,
      verbose = TRUE
    ) %>%
      dplyr::mutate(
        study_start_date = x$study_start_date,
        study_end_date = x$study_end_date,
        age_strata = x$age_range,
        sex_strata = x$sex,
        required_days_prior_history = x$study_days_prior_history
      )
  })
  # to tibble and add specification for each cohort
  study_populations <- dplyr::bind_rows(study_populations,
    .id = "cohort_definition_id"
  )


  return(study_populations)
}
