
#' Identify a denominator population
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
#' @export
#'
#' @examples
get_denominator_pop <- function(db,
                                cdm_database_schema,
                                start_date = NULL,
                                end_date = NULL,
                                min_age = NULL, #change to 0?
                                max_age = NULL, #change to 150?
                                sex = "Both",
                                days_prior_history = 0,
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
  checkmate::assert_character(cdm_database_schema,
    add = error_message
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
  checkmate::assert_numeric(max_age,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_character(sex,
    add = error_message
  )
  sex_check <- all(sex %in% c("Male", "Female", "Both"))
  if (!isTRUE(sex_check)) {
    error_message$push(
      "- sex must be one of ´Male´, ´Female´, or , 'Both' "
    )
  }
  checkmate::assert_numeric(days_prior_history,
    add = error_message,
    null.ok = TRUE
  )
  checkmate::assert_logical(verbose,
    add = error_message
  )
  # report initial assertions
  checkmate::reportAssertions(collection = error_message)


  ## check person and observation_period tables exist
  # connect to relevant vocabulary tables
  # will return informative error if not found
  person_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {cdm_database_schema}.person"
  )))
  observation_period_db <- dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {cdm_database_schema}.observation_period"
  )))

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
  for (i in seq_along(person_db_names)) {
    variable_check <- exists(person_db_names[i], where = person_db %>%
      utils::head(1) %>%
      dplyr::collect())
    if (!isTRUE(variable_check)) {
      checkmate::assertTRUE(variable_check, add = error_message)
      error_message$push(glue::glue(
        "- Variable {person_db_names[i]} not found in person table"
      ))
    }
  }
  # observation_period table
  observation_period_db_names <- c(
    "observation_period_id", "person_id",
    "observation_period_start_date", "observation_period_end_date"
  )
  for (i in seq_along(observation_period_db_names)) {
    variable_check <- exists(observation_period_db_names[i],
      where = observation_period_db %>%
        utils::head(1) %>%
        dplyr::collect()
    )
    if (!isTRUE(variable_check)) {
      checkmate::assertTRUE(variable_check, add = error_message)
      error_message$push(
        glue::glue(
          "- Variable {observation_period_db_names[i]}
          not found in observation_period table"
        )
      )
    }
  }

  # check data
  person_n_check <- (person_db %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull()) >= 100
  if (!isTRUE(person_n_check)) {
    error_message$push(
      "- Less than 100 rows in person table "
    )
  }

  person_year_of_birth_check <- nrow(person_db %>%
    dplyr::filter(is.na("year_of_birth")) %>%
    dplyr::collect()) == 0
  if (!isTRUE(person_year_of_birth_check)) {
    error_message$push(
      "- People in person table missing year of birth"
    )
  }



  observation_period_n_check <- (observation_period_db %>%
    dplyr::select("person_id") %>%
    dplyr::distinct() %>%
    dplyr::tally() %>%
    dplyr::collect() %>%
    dplyr::pull()) >= 100
  if (!isTRUE(person_n_check)) {
    error_message$push(
      "- Less than 100 rows in person table "
    )
  }





  ##
  # if (is.null(start_date)) {
  #   start_date <- observation_period_db %>%
  #     dplyr::select(observation_period_start_date) %>%
  #     dplyr::distinct() %>%
  #     dplyr::collect() %>%
  #     dplyr::summarise(min(observation_period_start_date)) %>%
  #     dplyr::pull()
  # }
  #
  # if (is.null(end_date)) {
  #   end_date <- observation_period_db %>%
  #     dplyr::select(observation_period_end_date) %>%
  #     dplyr::distinct() %>%
  #     dplyr::collect() %>%
  #     dplyr::summarise(max(observation_period_end_date)) %>%
  #     dplyr::pull()
  # }
  
  person_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                cdm_database_schema,
                                ".person")))
  observation_period_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                            cdm_database_schema,
                                            ".observation_period")))
  study.pop <- person_db %>%
    left_join(observation_period_db)%>%
    mutate(dob= to_date(paste(year_of_birth, month_of_birth, day_of_birth,sep="-"),'YYYY-MM-DD')) %>%
    collect()
  
  study.pop <-study.pop %>%
    dplyr::mutate(gender= ifelse(gender_concept_id==8507, "Male",
                                 ifelse(gender_concept_id==8532, "Female", NA ))) %>%
    dplyr::mutate(gender= factor(gender,
                                 levels = c("Male", "Female")))
  
  
  #Optional arguments
  if(is.null(start_date)){
    start_date <- study.pop %>% 
      select(observation_period_start_date)%>% pull()%>%min()
  }
  if(is.null(end_date)){
    end_date <- study.pop %>% 
      select(observation_period_end_date)%>% pull()%>%max()
  }
  if(is.null(min_age)){
    min_age <- 0 
  }
  if(is.null(max_age)){
    max_age <- 150 
  }
  if(sex=="Male"){
    study.pop<- study.pop %>% filter(gender=="Male")
  } 
  if(sex=="Female"){
    study.pop<- study.pop %>% filter(gender=="Female")
  }
  
  
  # two time varying elements
  # Age
  # Observation start date + prior_history requirement
  study.pop <- study.pop %>%
    mutate(date_min_age=dob+days(min_age*365)) %>% #Days doesn't allow decimal objects (365.25)
    mutate(date_max_age=dob+ days((max_age+1)*365)) %>%
    mutate(date_with_prior_history=observation_period_start_date+days(days_prior_history))
  
  # keep people only if
  # 1) they satisfy age criteria at some point in the study
  # date_min_age is before study end
  study.pop<- study.pop %>%
    filter(date_min_age<=end_date)
  # date_max_age is after study study start
  study.pop<- study.pop %>%
    filter(date_max_age>=start_date)
  
  # 2) they satisfy priory history criteria at some point in the study
  # date_with_prior_history is on or after study start
  study.pop<- study.pop %>%
    filter(date_with_prior_history<end_date)
  # date_with_prior_history is before observation_period_end_date 
  study.pop<- study.pop %>%
    filter(date_with_prior_history<=observation_period_end_date)
  
  # observation end date is after start date
  # study.pop<- study.pop %>%
  #  filter(observation_period_start_date<end.date)  
  
  #Start date-----
  #start of study, start of observation, date_min_age, date_with_prior_history (whichever comes last)
  study.pop$cohort_start_date <-apply(study.pop[,c("observation_period_start_date",
                                                   "date_min_age",
                                                   "date_with_prior_history"
  )],1,max, na.rm=T) #remove na.rm
  study.pop <-study.pop %>%
    mutate(cohort_start_date=as.Date(cohort_start_date))%>%
    mutate(cohort_start_date= if_else(cohort_start_date < start_date, start_date, cohort_start_date))
  
  
  #End date-----
  #end of study, end of observation, max.age(whichever comes first)
  study.pop$cohort_end_date <-apply(study.pop[,c("observation_period_end_date",
                                                 "date_max_age")],1,min, na.rm=T)#remove na.rm
  study.pop <-study.pop %>%
    mutate(cohort_end_date=as.Date(cohort_end_date))%>%
    mutate(cohort_end_date= if_else(cohort_end_date > end_date, end_date, cohort_end_date))
  
  #Exclude people who are elegible after cohort_end_date
  study.pop <- study.pop %>%
    filter(cohort_start_date<= cohort_end_date)%>%
    select(person_id,gender,cohort_start_date,cohort_end_date)
  
  
  return(study.pop)
}
