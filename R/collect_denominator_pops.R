collect_denominator_pops <- function(db,
                                     results_schema_outcome,
                                     table_name_outcome,
                                     cohort_id_outcome,
                                     study_start_date=NULL,
                                     study_end_date=NULL,
                                     time="Months",
                                     age_groups=NULL,
                                     sex=NULL,
                                     days_prior_history=0,
                                     repetitive_events=FALSE,
                                     prior_event_lookback=NULL,
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
  checkmate::assert_character(results_schema_outcome,
    add = error_message
  )
    checkmate::assert_character(table_name_outcome,
    add = error_message
  )
        checkmate::assert_character(cohort_id_outcome,
    add = error_message
  )
  checkmate::assert_date(study_start_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_date(study_end_date,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_vector(time,
    add = error_message
  )
  time_check <- all(time %in% c("Months", "Years"))
  if (!isTRUE(time_check)) {
    error_message$push(
      "- time must be one or both of Months and Years"
    )
  }
  checkmate::assert_numeric(age_groups,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_vector(sex,
    add = error_message
  )
  sex_check <- all(sex %in% c("Male", "Female"))
  if (!isTRUE(sex_check)) {
    error_message$push(
      "- sex must be one or both of Male and Female "
    )
  }
  checkmate::assert_numeric(days_prior_history,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_logical(repetitive_events,
    add = error_message
  )
  checkmate::assert_numeric(prior_event_lookback,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)



}
