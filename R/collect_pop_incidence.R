collect_pop_incidence <- function(db,
                                  results_schema_outcomes,
                                  cohort_ids_outcomes,
                                  study_denominator_pop,
                                  cohort_ids_denominator_pops,
                                  time_intervals = c("Months", "Years"),
                                  prior_event_lookbacks,
                                  repetitive_events,
                                  confidence_intervals="exact",
                                  verbose = FALSE){


  # help to avoid formatting errors
  if (is.numeric(cohort_ids_outcomes)) {
    cohort_ids_outcomes <- as.character(cohort_ids_outcomes)
  }
  if (is.numeric(cohort_ids_denominator_pops)) {
    cohort_ids_denominator_pops <- as.character(cohort_ids_denominator_pops)
  }
  if (is.character(time_intervals)) {
    time_intervals <- stringr::str_to_sentence(time_intervals)
  }
  if (is.character(confidence_intervals)) {
    confidence_intervals <- stringr::str_to_lower(confidence_intervals)
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
  checkmate::assert_character(results_schema_outcomes,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_character(cohort_ids_outcomes,
    add = error_message,
    null.ok = TRUE
  )

  checkmate::assert_tibble(study_denominator_pop,
    add = error_message
  )
  checkmate::assertTRUE(all(study_denominator_pop$cohort_start_date <=
    study_denominator_pop$cohort_end_date))
  checkmate::assertTRUE(nrow(study_denominator_pop) > 0,
    add = error_message
  )
  checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_definition_id) &
    sum(is.na(study_denominator_pop$cohort_definition_id)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$person_id) &
    sum(is.na(study_denominator_pop$person_id)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_start_date) &
    sum(is.na(study_denominator_pop$cohort_start_date)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$cohort_end_date) &
    sum(is.na(study_denominator_pop$cohort_end_date)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$age_strata) &
    sum(is.na(study_denominator_pop$age_strata)) == 0)
  checkmate::assertTRUE(!is.null(study_denominator_pop$sex_strata) &
    sum(is.na(study_denominator_pop$sex_strata)) == 0)
  checkmate::assertTRUE(
    !is.null(study_denominator_pop$required_days_prior_history) &
    sum(is.na(study_denominator_pop$required_days_prior_history)) == 0)
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "person_id",
    "cohort_start_date", "cohort_end_date",
    "age_strata", "sex_strata", "required_days_prior_history"
  ) %in%
    names(study_denominator_pop)))

  checkmate::assert_character(cohort_ids_denominator_pops,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_choice(time_intervals,
    choices = c("Months", "Years"),
    add = error_message
  )
  checkmate::assert_numeric(prior_event_lookbacks,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_logical(repetitive_events,
    add = error_message
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  checkmate::assert_choice(confidence_intervals,
    choices = c("exact"),
    add = error_message,
    null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)







  study_specs<- tidyr::expand_grid(
    cohort_id_outcome = cohort_ids_outcomes,
    cohort_id_denominator_pop=cohort_ids_denominator_pops,
    time_interval = time_intervals,
    prior_event_lookback = prior_event_lookbacks,
    repetitive_events=repetitive_events,
    confidence_interval=confidence_intervals,
    verbose=verbose
  )

    if (is.null(prior_event_lookbacks)) {
    study_specs$prior_event_lookback <- NA
  }

    study_specs <- study_specs %>%
    dplyr::mutate(cohort_definition_id = as.character(dplyr::row_number()))

  study_specs <- split(
    study_specs,
    study_specs[, c("cohort_definition_id")]
  )

# get irs
irs<-lapply(study_specs, function(x) {
get_pop_incidence(db=db,
                  results_schema_outcome=results_schema_outcomes,
                  table_name_outcome=outcomecohortTableStem,
                  cohort_id_outcome=x$cohort_id_outcome,
                  study_denominator_pop=study_denominator_pop,
                  cohort_id_denominator_pop=x$cohort_id_denominator_pop,
                  time_interval=x$time_interval,
                  prior_event_lookback = x$prior_event_lookback,
                  repetitive_events=x$repetitive_events,
                  confidence_interval = x$confidence_interval,
                  verbose = x$verbose) %>%
      dplyr::mutate(
                  cohort_id_outcome=x$cohort_id_outcome,
                  cohort_id_denominator_pop=x$cohort_id_denominator_pop,
                  time_interval=x$time_interval,
                  prior_event_lookback = x$prior_event_lookback,
                  repetitive_events=x$repetitive_events,
                  confidence_interval = x$confidence_interval
      )
  })
  # to tibble and add specification for each cohort
  irs <- dplyr::bind_rows(irs,
    .id = "cohort_definition_id"
  )

return(irs)

}
