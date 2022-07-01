#' Get population prevalence estimates
#'
#' @param db
#' @param results_schema_outcome
#' @param table_name_outcome
#' @param cohort_id_outcome
#' @param study_denominator_pop
#' @param cohort_id_denominator_pop
#' @param period
#' @param time_interval
#' @param prior_event_lookback
#' @param minimum_representative_proportion
#' @param confidence_interval
#' @param verbose
#'
#' @return
#' @export
#'
#' @examples
get_pop_prevalence <- function(db,
                               results_schema_outcome,
                               table_name_outcome,
                               cohort_id_outcome = NULL,
                               study_denominator_pop,
                               cohort_id_denominator_pop = NULL,
                               period = 1,
                               time_interval = c("Months"),
                               prior_event_lookback = NULL,
                               minimum_representative_proportion = 0.5,
                               confidence_interval = "exact",
                               verbose = FALSE) {

  if (!is.null(prior_event_lookback)) {
    if (is.na(prior_event_lookback)) {
      prior_event_lookback <- NULL
    }
  }

  # help to avoid formatting errors
  if (is.numeric(cohort_id_outcome)) {
    cohort_id_outcome <- as.character(cohort_id_outcome)
  }
  if (is.numeric(cohort_id_denominator_pop)) {
    cohort_id_denominator_pop <- as.character(cohort_id_denominator_pop)
  }
  if (is.character(time_interval)) {
    time_interval <- stringr::str_to_sentence(time_interval)
  }
  if (is.character(confidence_interval)) {
    confidence_interval <- stringr::str_to_lower(confidence_interval)
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
                              add = error_message,
    null.ok = TRUE
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
  checkmate::assert_logical(verbose,
                            add = error_message
  )
  checkmate::assert_choice(confidence_interval,
                           choices = c("exact", "none"),
                           add = error_message,
                           null.ok = TRUE
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)

  if(verbose==TRUE){
    message("Inputs checked and all initial assertions passed")
  }

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

  if(verbose==TRUE){
    message("Check passed: one or more people in denominator")
  }

  # link to outcome cohort
    if(!is.null(results_schema_outcome)){
  outcome_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {results_schema_outcome}.{table_name_outcome}"
  )))
  } else {
     outcome_db <- tbl(db, "outcome")
  }
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

  if(verbose==TRUE){
    message("Check passed: one or more outcomes identified")
  }

  # bring outcomes into memory
  if(verbose==TRUE){
    message("Bringing outcomes into memory")
  }
  outcome <- outcome_db %>%
    dplyr::rename("person_id" = "subject_id") %>%
    dplyr::rename("outcome_start_date" = "cohort_start_date") %>%
    dplyr::rename("outcome_end_date" = "cohort_end_date") %>%
    dplyr::select("person_id", "outcome_start_date", "outcome_end_date") %>%
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

  if (!is.null(prior_event_lookback)){
    outcome_exclude <- outcome %>%
      dplyr::select("person_id","outcome_start_date") %>%
      dplyr::left_join(outcome %>%
                         dplyr::select("person_id","outcome_end_date") %>%
                         dplyr::rename("prior_outcome_end_date" = "outcome_end_date"),
                       by = "person_id") %>%
      dplyr::filter(.data$outcome_start_date > .data$prior_outcome_end_date) %>%
      dplyr::mutate(diff_days = as.numeric(difftime(.data$outcome_start_date,
                                                    .data$prior_outcome_end_date,
                                                    units = "days"))) %>%
      dplyr::filter(.data$diff_days < prior_event_lookback) %>%
      dplyr::select("person_id","outcome_start_date")
    if (nrow(outcome_exclude)>0){
      outcome_visible <- outcome %>%
        dplyr::anti_join(outcome_exclude,
                         by = c("person_id","outcome_start_date"))
    }
  } else {
    outcome_visible <- outcome %>%
      dplyr::group_by(.data$person_id) %>%
      dplyr::filter(.data$outcome_start_date == min(.data$outcome_start_date))
  }

  # fetch prevalence
  # looping through each time interval
  prev <- list()
  for (i in seq_along(1:(n_time + 1))) {
    if (time_interval == "Years") {
      working_t_start <- start_date + lubridate::years(i - 1)
      working_t_end <- working_t_start + lubridate::days(period)
    }
    if (time_interval == "Months") {
      working_t_start <- start_date + months(i - 1)
      working_t_end <- working_t_start + lubridate::days(period)
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
      dplyr::mutate(t_end_date =
                      dplyr::if_else(.data$cohort_end_date >= working_t_end,
                                     working_t_end,
                                     .data$cohort_end_date
                      ))

    working_pop <- working_pop %>%
      dplyr::select("person_id", "t_start_date", "t_end_date")

    # update t_start_date based on previous outcome
    if (is.null(prior_event_lookback)){

      working_pop <- working_pop %>%
        dplyr::anti_join(outcome %>%
                           dplyr::select("person_id","outcome_start_date") %>%
                           dplyr::left_join(working_pop %>%
                                              dplyr::select("person_id","t_start_date"),
                                            by = "person_id") %>%
                           dplyr::filter(.data$outcome_start_date < .data$t_start_date) %>%
                           dplyr::select("person_id"),
                         by = "person_id")

    }else{


      outcome_prior <- outcome %>%
        dplyr::rename("prior_outcome_end_date" = "outcome_end_date") %>%
        dplyr::select("person_id", "prior_outcome_end_date") %>%
        dplyr::inner_join(working_pop %>%
                            dplyr::select("person_id", "t_start_date"),
                          by = "person_id"
        ) %>%
        dplyr::mutate(diff_days = as.numeric(difftime(.data$t_start_date,
                                                      .data$prior_outcome_end_date,
                                                      units = "days"
        ))) %>%
        dplyr::filter(.data$diff_days > 0)

      if (nrow(outcome_prior) >= 1) {
        outcome_prior <- outcome_prior %>%
          dplyr::group_by(.data$person_id, .data$t_start_date) %>%
          dplyr::summarise(
            diff_days = min(.data$diff_days),
            .groups = "drop"
          )
        outcome_prior <- outcome_prior %>%
          dplyr::filter(.data$diff_days <= prior_event_lookback)

        # calculate at which individuals satisfied prior lookback requirement
        working_pop <- working_pop %>%
          dplyr::left_join(outcome_prior,
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
          )) %>%
          dplyr::select(-"diff_days")
      }
    }

    working_pop <- working_pop %>%
      dplyr::inner_join(working_pop %>%
                          dplyr::mutate(diff_days = as.numeric(difftime(.data$t_end_date,
                                                                        .data$t_start_date,
                                                                        units = "days")) + 1) %>%
                          dplyr::group_by(.data$person_id) %>%
                          dplyr::summarise(contribution = sum(.data$diff_days)/period) %>%
                          dplyr::filter(.data$contribution >= minimum_representative_proportion) %>%
                          select("person_id"),
                        by = "person_id")

    individuals <- working_pop %>%
      distinct() %>%
      nrow()

    prevalent <- outcome_visible %>%
      dplyr::left_join(working_pop,
                       by = "person_id") %>%
      dplyr::filter(.data$outcome_start_date <= .data$t_end_date) %>%
      dplyr::filter(.data$outcome_end_date >= .data$t_start_date) %>%
      nrow()

    prev[[paste0(i)]] <- dplyr::tibble(prevalent = prevalent,
                                     individuals = individuals,
                                     prevalence = prevalent/individuals,
                                     calendar_month = ifelse(time_interval == "Months",
                                                             lubridate::month(working_t_start),
                                                             NA),
                                     calendar_year = lubridate::year(working_t_start))
  }

  ir <- dplyr::bind_rows(ir)

  if (confidence_interval == "none") {
    ir <- ir %>%
      dplyr::mutate(ir_low = NA) %>%
      dplyr::mutate(ir_high = NA)
  }

  if (confidence_interval == "exact") {
    ci <- tibble(prev_low = qchisq(0.05/2, df=2*(prev$prevalent-1))/2/prev$individuals,
                 prev_high = qchisq(1-0.05/2, df=2*prev$prevalent)/2/prev$individuals)

    prev <- dplyr::bind_cols(prev, ci) %>%
      dplyr::relocate(.data$prev_low, .before = .data$calendar_month) %>%
      dplyr::relocate(.data$prev_high, .after = .data$prev_low)
  }

  # add study design related variables
  prev <- prev %>%
    dplyr::mutate(required_days_prior_history = unique(study_pop$required_days_prior_history)) %>%
    dplyr::mutate(age_strata = unique(study_pop$age_strata)) %>%
    dplyr::mutate(sex_strata = unique(study_pop$sex_strata)) %>%
    dplyr::mutate(prior_event_lookback = prior_event_lookback) %>%
    dplyr::mutate(period = period) %>%
    dplyr::mutate(time_interval = time_interval) %>%
    dplyr::mutate(confidence_interval = confidence_interval)

  return(prev)
}
