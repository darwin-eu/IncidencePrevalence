calculate_pop_incidence <- function(db,
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
  if (!is.null(cohort_id_denominator_pop)) {
    study_pop <- study_pops %>%
      filter(cohort_definition_id == cohort_id_denominator_pop)
  }

  # link to outcome cohort
  outcome_db <- tbl(db, sql(glue::glue(
    "SELECT * FROM {results_schema_outcome}.{table_name_outcome}"
  )))

  if (!is.null(cohort_id_outcome)) {
    outcome_db <- outcome_db %>%
      filter(cohort_definition_id == cohort_id_outcome) %>%
      compute()
  }

  # nb more than one outcome per person is allowed
  outcome <- outcome_db %>%
    rename("outcome_date" = "cohort_start_date") %>%
    rename("cohort_outcome_id" = "cohort_definition_id") %>%
    rename("person_id" = "subject_id") %>%
    select("person_id", "outcome_date") %>%
    collect()

  # Time intervals------
  start_date <- min(study_pop$study_start_date)
  end_date <- max(study_pop$study_end_date)
  if (time_interval == "Years") {
    n.time <- lubridate::interval(ymd(start_date), ymd(end_date)) %/% years(1)
  }
  if (time_interval == "Months") {
    n.time <- lubridate::interval(ymd(start_date), ymd(end_date)) %/% months(1)
  }

  # fetch incidence rates
  # looping through each time interval
  IR <- list()
  for (i in 1:(n.time + 1)) {
    if (time_interval == "Years") {
      working.t.start <- start_date + years(i - 1) # date, first day of the year
      # date, last day of the year or observation period
      # to make sure the user doesn't specify an end date after the last date of available data?
      if ((i == max(n.time) + 1) & (start_date + years(i) - days(1) > end_date)) {
        working.t.end <- end_date
      } else {
        working.t.end <- start_date + years(i) - days(1)
      }
      working.t.days <- as.numeric(difftime(working.t.end + days(1), working.t.start, units = "days"))
    }

    if (time_interval == "Months") {
      working.t.start <- start_date + months(i - 1) # date, first day of the month
      if ((i == max(n.time) + 1) & (start_date + months(i) - days(1) > end_date)) {
        working.t.end <- end_date
      } else {
        working.t.end <- start_date + months(i) - days(1)
      }
      working.t.days <- as.numeric(difftime(working.t.end + days(1), working.t.start, units = "days"))
    }

    # drop people who were censored prior to working.t.start (start of the month/year...)
    # & drop people who were not present in the database before working.t.end
    working.pop <- study_pop %>%
      filter(cohort_end_date >= working.t.start) %>%
      filter(cohort_start_date <= working.t.end)

    # individuals start date for this period
    # could be start of the period or later
    working.pop <- working.pop %>%
      mutate(t_start_date = if_else(cohort_start_date <= working.t.start,
                                   working.t.start,
                                   cohort_start_date))

    # individuals end date for this period
    # end of the period or earlier
    working.pop <- working.pop %>%
      mutate(t_end_date = if_else(cohort_end_date >= working.t.end,
                                 working.t.end,
                                 cohort_end_date))


    # Exclusions based on events prior to current start date
    outcome_prior <- outcome %>%
      inner_join(working.pop %>% select(person_id, t_start_date),
                 by="person_id") %>%
      mutate(diff_days = as.numeric(difftime(t_start_date, outcome_date,
        units = "days"
      ))) %>%
      filter(diff_days > 0)

    if (is.null(prior_event_lookback)) {
      # If prior_event_lookback is null,
      # we exclude people with an event at any point before their index date
      working.pop <- working.pop %>%
        anti_join(outcome_prior,
                  by=c("person_id", "t_start_date"))
    }

    if (is.numeric(prior_event_lookback)) {
      # If a number of days is specified, check outcomes that occurred in this window of time
      outcome_prior_lookback <- outcome_prior %>%
        group_by(person_id) %>%
        arrange(diff_days) %>%
        mutate(seq = row_number()) %>%
        filter(seq == 1) %>% # most recent prior event relative to t_start_date
        filter(diff_days < prior_event_lookback) %>%
        select(-seq)

      # Date at which individuals satisfied prior lookback requirement
      working.pop<- working.pop %>%
        left_join(outcome_prior_lookback %>%
                    select(person_id, t_start_date, diff_days),
                  by=c("person_id", "t_start_date"))
      # update t_start_date
      working.pop<-working.pop %>%
        mutate(t_start_date=if_else(
          is.na(diff_days), t_start_date,
          t_start_date+days(prior_event_lookback - diff_days) ))
      # Exclude people who reach the lookback period after the end date
      working.pop <- working.pop %>%
        filter(t_start_date <= t_end_date)

      working.pop<-working.pop %>%
        select(-"diff_days")
    }

    # Add outcome information
    # First event during the interval of time under study
    # Repetitive events option not included at the moment
    outcome.t <- outcome %>%
      inner_join(working.pop %>% select(person_id, cohort_start_date,
                                       t_start_date, cohort_end_date,
                                       t_end_date),
                 by="person_id") %>%
      mutate(diff_days = as.numeric(difftime(t_start_date, outcome_date,
        units = "days"
      ))) %>%
      filter(outcome_date >= t_start_date) %>%
      filter(outcome_date <= t_end_date) %>%
      group_by(person_id) %>%
      arrange(desc(diff_days)) %>%
      mutate(seq = row_number()) %>%
      filter(seq == 1) %>%
      select(-"seq")

    working.pop <- working.pop %>%
      left_join(outcome.t %>% select(person_id, outcome_date),
                by="person_id")


    # number of days contributed in working.time
    working.pop <- working.pop %>%
      mutate(working.days = ifelse(is.na(outcome_date),
        as.numeric(difftime(t_end_date, t_start_date, units = "days")),
        ifelse((t_start_date <= outcome_date) & (t_end_date >= outcome_date),
          as.numeric(difftime(outcome_date, t_start_date, units = "days")),
          as.numeric(difftime(t_end_date, t_start_date, units = "days"))
        )
      )) %>%
      # if they enter the same day they exit, they contribute 0.25d
      mutate(working.days = case_when(
        (working.days == 0) ~ 0.25,
        working.days > 0 ~ as.numeric(working.days)
      )) # don't know why numbers <14 have decimals (.00)?

    IR[[paste0(i)]] <- working.pop %>%
      summarise(
        n_persons = length(unique(working.pop$person_id)),
        person_days = sum(working.days),
        person_months = (person_days / 30.44),
        person_years = (person_days / 365.25),
        n_events = sum(!is.na(outcome_date))
      ) %>%
      mutate(ir = (n_events / person_months) * 100000) %>%
      mutate(calendar_month = ifelse(time_interval == "Months",
                                     month(working.t.start),
                                     NA)) %>%
      mutate(calendar_year = year(working.t.start))
  }

  IR <- bind_rows(IR)

  if (confidence_intervals == "None") {
    IR<-IR %>%
      mutate(ir_low=NA) %>%
      mutate(ir_high=NA)
  }

  if (confidence_intervals == "exact") {
    ci <- epi.conf(as.matrix(IR %>% select(n_events, person_months)),
      ctype = "inc.rate",
      method = confidence_intervals,
      N = 100000,
      design = 1,
      conf.level = 0.95
    ) * 100000
    ci <- ci %>%
      select(lower, upper) %>%
      rename("ir_low" = "lower") %>%
      rename("ir_high" = "upper")

    IR <- bind_cols(IR, ci) %>%
      relocate(ir_low, .before = calendar_month) %>%
      relocate(ir_high, .after = ir_low)
  }

  # add design related variables
  IR <- IR %>%
      mutate(required_days_prior_history = unique(study_pop$required_days_prior_history)) %>%
      mutate(age_strata = unique(study_pop$age_strata)) %>%
      mutate(sex_strata = unique(study_pop$sex_strata)) %>%
      mutate(prior_event_lookback = prior_event_lookback)%>%
      mutate(repetitive_events = repetitive_events)%>%
      mutate(time_interval = time_interval)

  return(IR)
}
