collect_denominator_pops <- function(db,
                         cdm_database_schema,
                         study_start_date=NULL,
                         study_end_date=NULL,
                         age_groups=NULL,
                         sex="Both",
                         days_prior_history=0,
                         verbose = FALSE){

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
  checkmate::assert_vector(sex,
    add = error_message
  )
  sex_check <- all(sex %in% c("Male", "Female", "Both"))
  if (!isTRUE(sex_check)) {
    error_message$push(
      "- sex must be one or more of: Male, Female, and Both"
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

# add broadest possible age group if NULL
  if (is.null(age_groups)) {
    age_groups <- list(c(0,150))
  }
# only allow multiple values of age and sex
age_gr_df<-data.frame(do.call(rbind,age_groups)) %>%
  mutate(age_range=paste0(X1,";",X2))
pop_specs<-expand_grid(age_range = age_gr_df$age_range,
                               sex = sex)  %>%
  separate(age_range, c("min_age", "max_age")) %>%
  mutate(min_age=as.numeric(min_age)) %>%
  mutate(max_age=as.numeric(max_age))%>%
  mutate(cohort_definition_id=as.character(1:nrow(.)))
# to list
pop_specs <-  split(pop_specs,
                 pop_specs[,c('cohort_definition_id')])
# get each population
study_populations<-lapply(pop_specs, function(x)
  get_denominator_pop(
       db=db,
       cdm_database_schema=cdm_database_schema,
       start_date = study_start_date,
                    end_date = study_end_date,
                    min_age = x$min_age,
                    max_age = x$max_age,
                    sex = x$sex,
                    days_prior_history = days_prior_history,
                    verbose = TRUE))
# to tibble and add specification for each cohort
study_populations<-bind_rows(study_populations, .id = "cohort_definition_id") %>%
  left_join(bind_rows(pop_specs),
            by=c("cohort_definition_id"))


return(study_populations)
}
