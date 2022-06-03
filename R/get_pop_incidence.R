get_pop_incidence <- function(db,
                                    results_schema_outcome,
                                    table_name_outcome,
                                    cohort_id_outcome = NULL,
                                    study_denominator_pop,
                                    cohort_id_denominator_pop = NULL,
                                    time_interval = c("Months"),
                                    prior_event_lookback = NULL,
                                    repetitive_events = FALSE,
                                    confidence_intervals = "exact",
                                    verbose = FALSE) {

  # help to avoid errors
  if (is.numeric(cohort_id_outcome)) {
    cohort_id_outcome <- as.character(cohort_id_outcome)
  }
  if (is.numeric(cohort_id_denominator_pop)) {
    cohort_id_denominator_pop <- as.character(cohort_id_denominator_pop)
  }
  if (is.character(time_interval)) {
    time_interval <- stringr::str_to_sentence(time_interval)
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
  checkmate::assert_character(results_schema_outcome,
    add = error_message
  )
  checkmate::assert_character(cohort_id_outcome,
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
  checkmate::assertTRUE(!is.null(study_denominator_pop$required_days_prior_history) &
    sum(is.na(study_denominator_pop$required_days_prior_history)) == 0)
  checkmate::assertTRUE(all(c(
    "cohort_definition_id",
    "person_id",
    "cohort_start_date", "cohort_end_date",
    "age_strata", "sex_strata", "required_days_prior_history"
  ) %in%
    names(study_denominator_pop)))

  checkmate::assert_character(cohort_id_denominator_pop,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_choice(time_interval,
    choices = c("Months", "Years"),
    add = error_message
  )
  checkmate::assert_numeric(prior_event_lookback,
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


  ## Analysis code
  # bring in study popupulation
  study_pop <- study_denominator_pop
  if (!is.null(cohort_id_denominator_pop)) {
    study_pop <- study_pop %>%
      dplyr::filter(.data$cohort_definition_id ==
        cohort_id_denominator_pop)
  }

  # check population n is above zero
  # return error if not
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(nrow(study_pop) > 0,
    add = error_message
  )
  if (!nrow(study_pop) > 0) {
    error_message$push(
      glue::glue("- Zero rows in study_denominator_pop with
      cohort_id_denominator_pop={cohort_id_denominator_pop}")
    )
  }
  checkmate::reportAssertions(collection = error_message)



  # link to outcome cohort
  outcome_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {results_schema_outcome}.{table_name_outcome}"
  )))
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0,
    add = error_message
  )
  if (!nrow(study_pop) > 0) {
    error_message$push(
      glue::glue("- Zero rows in {results_schema_outcome}.{table_name_outcome}")
    )
  }
  checkmate::reportAssertions(collection = error_message)

  if (!is.null(cohort_id_outcome)) {
    outcome_db <- outcome_db %>%
      dplyr::filter(.data$cohort_definition_id == cohort_id_outcome) %>%
      dplyr::compute()
  }
  error_message <- checkmate::makeAssertCollection()
  checkmate::assertTRUE(outcome_db %>% dplyr::tally() %>% dplyr::pull() > 0,
    add = error_message
  )
  if (!nrow(study_pop) > 0) {
    error_message$push(
      glue::glue("- Zero rows in {results_schema_outcome}.{table_name_outcome}
                 for cohort_id_outcome={cohort_id_outcome}")
    )
  }
  checkmate::reportAssertions(collection = error_message)

  # bring outcome into memory
  # nb more than one outcome per person is allowed
  outcome <- outcome_db %>%
    dplyr::rename("outcome_date" = "cohort_start_date") %>%
    dplyr::rename("cohort_outcome_id" = "cohort_definition_id") %>%
    dplyr::rename("person_id" = "subject_id") %>%
    dplyr::select("person_id", "outcome_date") %>%
    dplyr::collect()


  # start date
  start_date <- min(study_pop$cohort_start_date)
  # end date to the last day of last available full period
  if (time_interval == "Years") {
    end_date <- lubridate::floor_date(max(study_pop$cohort_end_date),
      unit = "years"
    ) - lubridate::days(1)
  }
  if (time_interval == "Months") {
    end_date <- lubridate::floor_date(max(study_pop$cohort_end_date),
      unit = "months"
    ) - lubridate::days(1)
  }

  # will give error if no full months/ years
  error_message <- checkmate::makeAssertCollection()

  if (time_interval == "Years") {
    n_time <- lubridate::interval(
      lubridate::ymd(start_date),
      lubridate::ymd(end_date)
    ) %/%
      lubridate::years(1)

    checkmate::assertTRUE(n_time > 0,
      add = error_message
    )
    if (!n_time > 0) {
      error_message$push(
        glue::glue("- Less than one full year between earliest study_start_date
                   and last study_end_date in study_denominator_pop")
      )
    }
  }

  if (time_interval == "Months") {
    n_time <- lubridate::interval(
      lubridate::ymd(start_date),
      lubridate::ymd(end_date)
    ) %/%
      months(1)

    checkmate::assertTRUE(n_time > 0,
      add = error_message
    )
    if (!n_time > 0) {
      error_message$push(
        glue::glue("- Less than one full month between earliest study_start_date
                   and last study_end_date in study_denominator_pop")
      )
    }
  }

  checkmate::reportAssertions(collection = error_message)

  # fetch incidence rates
  # looping through each time interval
  ir <- list()
  # for (i in seq_along(1:(n_time + 1))) {
   for (i in seq_along(1:11)) {
    if (time_interval == "Years") {
      working_t_start <- start_date + lubridate::years(i - 1)
      working_t_end <- start_date + lubridate::years(i) - lubridate::days(1)
      working_t_days <- as.numeric(difftime(working_t_end + lubridate::days(1),
        working_t_start,
        units = "days"
      ))
    }

    if (time_interval == "Months") {
      working_t_start <- start_date + months(i - 1)
      working_t_end <- start_date + months(i) - lubridate::days(1)
      working_t_days <- as.numeric(difftime(working_t_end +
        lubridate::days(1),
      working_t_start,
      units = "days"
      ))
    }

    # drop people with end_date prior to working_t_start
    # drop people with start_date after working_t_end
    working_pop <- study_pop %>%
      dplyr::filter(.data$cohort_end_date >= working_t_start) %>%
      dplyr::filter(.data$cohort_start_date <= working_t_end)

    # individuals start date for this period
    # which could be start of the period or later
    working_pop <- working_pop %>%
      dplyr::mutate(t_start_date = dplyr::if_else(.data$cohort_start_date <=
        working_t_start, working_t_start,
      .data$cohort_start_date
      ))

    # individuals end date for this period
    # end of the period or earlier
    working_pop <- working_pop %>%
      dplyr::mutate(t_end_date = dplyr::if_else(.data$cohort_end_date >= working_t_end,
        working_t_end,
        .data$cohort_end_date
      ))


    # Exclusions based on events prior to current start date
    outcome_prior <- outcome %>%
      dplyr::inner_join(working_pop %>%
        dplyr::select("person_id", "t_start_date"),
      by = "person_id"
      ) %>%
      dplyr::mutate(diff_days = as.numeric(difftime(.data$t_start_date,
        .data$outcome_date,
        units = "days"
      ))) %>%
      dplyr::filter(.data$diff_days > 0)

    # keep most recent
    outcome_prior <- outcome_prior %>%
      dplyr::group_by(.data$person_id, .data$t_start_date) %>%
      dplyr::summarise(
        diff_days = min(.data$diff_days),
        .groups = "drop"
      )

    if (is.null(prior_event_lookback)) {
      # If prior_event_lookback is null,
      # we exclude people with an event at any point before their index date
      working_pop <- working_pop %>%
        dplyr::anti_join(outcome_prior,
          by = c("person_id", "t_start_date")
        )
    }

    if (is.numeric(prior_event_lookback)) {
      # If a number of days is specified,
      # first get outcomes that occurred in this window of time
      outcome_prior_lookback <- outcome_prior %>%
        dplyr::filter(.data$diff_days < prior_event_lookback)

      # calculate at which individuals satisfied prior lookback requirement
      working_pop <- working_pop %>%
        dplyr::left_join(outcome_prior_lookback,
          by = c("person_id", "t_start_date")
        )
      # update t_start_date to when individuals satisfied prior lookback
      # as in by this new date, they are now eligible
      working_pop <- working_pop %>%
        dplyr::mutate(t_start_date = dplyr::if_else(
          is.na(.data$diff_days),
          .data$t_start_date,
          .data$t_start_date +
            lubridate::days(prior_event_lookback - .data$diff_days)
        ))
      # But, we need to now exclude people who reach the required lookback period after the end date
      working_pop <- working_pop %>%
        dplyr::filter(.data$t_start_date <= .data$t_end_date)

      working_pop <- working_pop %>%
        dplyr::select(-"diff_days")
    }

    # Add outcome information
    # First event during the interval of time under study
    # Repetitive events option not included at the moment
    outcome_t <- outcome %>%
      dplyr::inner_join(working_pop %>%
        dplyr::select(
          "person_id", "cohort_start_date",
          "t_start_date", "cohort_end_date",
          "t_end_date"
        ),
      by = "person_id"
      ) %>%
      dplyr::mutate(
        diff_days =
          as.numeric(difftime(.data$outcome_date,
            .data$t_start_date,
            units = "days"
          ))
      ) %>%
      dplyr::filter(.data$outcome_date >= .data$t_start_date) %>%
      dplyr::filter(.data$outcome_date <= .data$t_end_date)
    # keep earliest
    if(nrow(outcome_t)>0){
    outcome_t <- outcome_t %>%
      dplyr::group_by(.data$person_id, .data$t_start_date) %>%
      dplyr::summarise(outcome_date = min(.data$outcome_date), .groups = "drop")
    }

    outcome_t <- outcome_t %>%
      dplyr::select("person_id","t_start_date", "outcome_date" )

    working_pop <- working_pop %>%
      dplyr::left_join(outcome_t %>%
        dplyr::select("person_id", "outcome_date"),
      by = "person_id"
      )

    # number of days contributed in working time
    # no outcome: t_start_date to t_end_date
    # outcome: t_start_date to t_end_date
    working_pop <- working_pop %>%
      dplyr::mutate(
        working_days =
          dplyr::if_else(is.na(.data$outcome_date),
            as.numeric(difftime(.data$t_end_date,
              .data$t_start_date,
              units = "days"
            )),
            as.numeric(difftime(.data$outcome_date,
              .data$t_start_date,
              units = "days"
            ))
          )
      )


    # if they enter the same day they exit, they contribute 0.5d
    working_pop <- working_pop %>%
      dplyr::mutate(working_days = dplyr::case_when(
        (.data$working_days == 0) ~ 0.5,
        .data$working_days > 0 ~ .data$working_days
      ))

    ir[[paste0(i)]] <- working_pop %>%
      dplyr::summarise(
        n_persons = length(unique(working_pop$person_id)),
        person_days = sum(.data$working_days),
        person_months = (.data$person_days / 30.44),
        person_years = (.data$person_days / 365.25),
        n_events = sum(!is.na(.data$outcome_date))
      ) %>%
      dplyr::mutate(ir = (.data$n_events / .data$person_months) * 100000) %>%
      dplyr::mutate(calendar_month = ifelse(time_interval == "Months",
        lubridate::month(working_t_start),
        NA
      )) %>%
      dplyr::mutate(calendar_year = lubridate::year(working_t_start))
  }

  ir <- dplyr::bind_rows(ir)

  if (confidence_intervals == "None") {
    ir <- ir %>%
      dplyr::mutate(ir_low = NA) %>%
      dplyr::mutate(ir_high = NA)
  }

  if (confidence_intervals == "exact") {
    ci <- epiR::epi.conf(as.matrix(ir %>%
      dplyr::select("n_events", "person_months")),
    ctype = "inc.rate",
    method = confidence_intervals,
    N = 100000,
    design = 1,
    conf.level = 0.95
    ) * 100000
    ci <- ci %>%
      dplyr::select("lower", "upper") %>%
      dplyr::rename("ir_low" = "lower") %>%
      dplyr::rename("ir_high" = "upper")

    ir <- dplyr::bind_cols(ir, ci) %>%
      dplyr::relocate(.data$ir_low, .before = .data$calendar_month) %>%
      dplyr::relocate(.data$ir_high, .after = .data$ir_low)
  }

  # add design related variables
  ir <- ir %>%
    dplyr::mutate(
      required_days_prior_history =
        unique(study_pop$required_days_prior_history)
    ) %>%
    dplyr::mutate(age_strata = unique(study_pop$age_strata)) %>%
    dplyr::mutate(sex_strata = unique(study_pop$sex_strata)) %>%
    dplyr::mutate(prior_event_lookback = prior_event_lookback) %>%
    dplyr::mutate(repetitive_events = repetitive_events) %>%
    dplyr::mutate(time_interval = time_interval) %>%
    dplyr::mutate(confidence_intervals = confidence_intervals)

  return(ir)
}



# get_ir_for_period <- function()
