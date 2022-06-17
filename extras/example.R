# packages -----
library(SqlRender)
library(DatabaseConnector)
library(CohortGenerator)
library(CirceR)
library(FeatureExtraction)
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(RSQLite)
library(rmarkdown)
library(tableone)
library(scales)
library(forcats)
library(epiR)
library(RPostgreSQL)
library(readxl)
library(lubridate)
library(readxl)
library(dtplyr)
devtools::load_all()


# database ----
#usethis::edit_r_environ()

downloadJdbcDrivers("postgresql", here()) # if you already have this you can omit and change pathToDriver below
connectionDetails <- createConnectionDetails(dbms = "postgresql",
                                             server =Sys.getenv("SERVER_TEST"),
                                             user = Sys.getenv("DB_USER_TEST"),
                                             password = Sys.getenv("DB_PASSWORD_TEST"),
                                             port = Sys.getenv("DB_PORT_TEST") ,
                                             pathToDriver = here())
db <-DBI::dbConnect(RPostgres::Postgres(),
                    dbname = Sys.getenv("SERVER_DBI_TEST"),
                    port = Sys.getenv("DB_PORT_TEST"),
                    host = Sys.getenv("DB_HOST_TEST"),
                    user = Sys.getenv("DB_USER_TEST"),
                    password = Sys.getenv("DB_PASSWORD_TEST"))


targetDialect <-"postgresql"
cdm_database_schema<-"omop21t2_test"
vocabulary_database_schema<-"omop21t2_test"
results_database_schema<-"results21t2_test"

outcomecohortTableStem<-"EB_OmopPopEpi"

# build results cohorts -----
cohortJsonFiles <- list.files(here("extras", "outcome_cohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
working.json<-here("extras", "outcome_cohorts",
                      cohortJsonFiles[i])
cohortJson <- readChar(working.json, file.info(working.json)$size)
cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
sql <- buildCohortQuery(cohortExpression,
                   options = CirceR::createGenerateOptions(generateStats = TRUE))

cohortDefinitionSet[[i]]<-tibble(atlasId = i,
      cohortId = i,
      cohortName = str_replace(cohortJsonFiles[i],".json",""),
      json=cohortJson,
      sql=sql,
      logicDescription = NA,
      generateStats=FALSE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = outcomecohortTableStem)

CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

CohortGenerator::dropCohortStatsTables(
    connectionDetails = connectionDetails,
    cohortDatabaseSchema = results_database_schema,
    cohortTableNames = cohortTableNames,
    connection = NULL)

# rows for each outcome
# nb, depending on outcome definition, one person may
# contribute multiple rows
dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {results_database_schema}.{outcomecohortTableStem}"
  ))) %>%
  group_by(cohort_definition_id) %>%
  tally()%>%
  arrange(cohort_definition_id)

# n of people per cohort
dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {results_database_schema}.{outcomecohortTableStem}"
  ))) %>%
  select(cohort_definition_id, subject_id) %>%
  distinct() %>%
  group_by(cohort_definition_id) %>%
  tally()%>%
  arrange(cohort_definition_id)

# maximum rows per person per outcome
# i.e. max number of times someone has an outcome
dplyr::tbl(db, dplyr::sql(glue::glue(
    "SELECT * FROM {results_database_schema}.{outcomecohortTableStem}"
  ))) %>%
  group_by(cohort_definition_id, subject_id) %>%
  tally() %>%
  summarise(max(n, na.rm=TRUE)) %>%
  arrange(cohort_definition_id)


# dementia ----
denominator_pop_dementia<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2014-01-01"),
                         study_end_date=as.Date("2016-12-31"),
                         study_age_stratas = list(c(50,80),
                                                  c(80,110)),
                         study_sex_stratas = c("Male", "Female", "Both"),
                         study_days_prior_history =c(365,1826),
                         verbose = TRUE)
ir_dementia_1y<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=1,
                        study_denominator_pop=denominator_pop_dementia,
                        cohort_id_denominator_pop=1,
                        time_interval=c("Months"),
                        prior_event_lookback=365,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=TRUE)
ir_dementia_all_hist<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=1,
                        study_denominator_pop=denominator_pop_dementia,
                        cohort_id_denominator_pop=1,
                        time_interval=c("Months"),
                        prior_event_lookback=NULL,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=FALSE)


bind_rows(ir_dementia_1y %>% mutate(type="1y"),
ir_dementia_all_hist %>% mutate(type="All")) %>%
    mutate(year_months=paste0(calendar_year, "-", calendar_month)) %>%
  ggplot(aes(group=type, colour=type))+
  geom_point(aes(year_months, ir),
              position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=year_months, ymin=ir_low, ymax=ir_high),
                 position=position_dodge(width=0.5))

all(ir_dementia_1y %>%
  select(ir) %>%
  pull() ==
ir_dementia_all_hist %>%
  select(ir) %>%
  pull())

# by design of the dementia outcome cohort (first event, end date end of observation),
# we should get the same results
# regardless of repetitive events TRUE or FALSE
get_pop_incidence(db=db,
                  results_schema_outcome="results21t2_test",
                  table_name_outcome=outcomecohortTableStem,
                  cohort_id_outcome=1,
                  study_denominator_pop=denominator_pop_dementia,
                  cohort_id_denominator_pop=1,
                  time_interval=c("Years"),
                  repetitive_events=FALSE) %>%
  select(ir) %>%
  pull() ==
get_pop_incidence(db=db,
                  results_schema_outcome="results21t2_test",
                  table_name_outcome=outcomecohortTableStem,
                  cohort_id_outcome=1,
                  study_denominator_pop=denominator_pop_dementia,
                  cohort_id_denominator_pop=1,
                  time_interval=c("Years"),
                  repetitive_events=TRUE) %>%
  select(ir) %>%
  pull()


get_pop_incidence(db=db,
                  results_schema_outcome="results21t2_test",
                  table_name_outcome=outcomecohortTableStem,
                  cohort_id_outcome=1,
                  study_denominator_pop=denominator_pop_dementia,
                  cohort_id_denominator_pop=1,
                  time_interval=c("Months"),
                  repetitive_events=FALSE) %>%
  filter(calendar_year=="2014") %>%
  summarise(person_days=sum(person_days),
            person_months=sum(person_months),
            person_years=sum(person_years),
            n_events=sum(n_events))

get_pop_incidence(db=db,
                  results_schema_outcome="results21t2_test",
                  table_name_outcome=outcomecohortTableStem,
                  cohort_id_outcome=1,
                  study_denominator_pop=denominator_pop_dementia,
                  cohort_id_denominator_pop=1,
                  time_interval=c("Years"),
                  repetitive_events=FALSE) %>%
  filter(calendar_year=="2014") %>%
  summarise(person_days=sum(person_days),
            person_months=sum(person_months),
            person_years=sum(person_years),
            n_events=sum(n_events))


collect_pop_incidence(db,
                      results_schema_outcomes="results21t2_test",
                      cohort_ids_outcomes=c(1,2,3),
                      study_denominator_pop=denominator_pop_dementia,
                      cohort_ids_denominator_pops=c(1,2,3),
                      time_intervals = "Years",
                      prior_event_lookbacks=NULL,
                      repetitive_events=FALSE,
                      confidence_intervals="exact",
                      verbose = FALSE)%>%
  group_by(cohort_definition_id)%>%
  filter(calendar_year=="2014") %>%
  summarise(person_days=sum(person_days),
            person_months=sum(person_months),
            person_years=sum(person_years),
            n_events=sum(n_events))

# influenza -------
denominator_pop_influenza<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2014-01-01"),
                         study_end_date=as.Date("2025-06-30"),
                         study_age_stratas = NULL,
                         study_sex_stratas = "Both",
                         study_days_prior_history =365,
                         verbose = TRUE)

ir_influenza_0<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=3,
                        study_denominator_pop=denominator_pop_influenza,
                        cohort_id_denominator_pop="1",
                        time_interval=c("Months"),
                        prior_event_lookback=0,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=TRUE)
ir_influenza_60<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=3,
                        study_denominator_pop=denominator_pop_influenza,
                        cohort_id_denominator_pop="1",
                        time_interval=c("Months"),
                        prior_event_lookback=60,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=TRUE)
ir_influenza_all_hist<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=3,
                        study_denominator_pop=denominator_pop_influenza,
                        cohort_id_denominator_pop=NULL,
                        time_interval=c("Months"),
                        prior_event_lookback=NULL,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=TRUE)


bind_rows(ir_influenza_0 %>% mutate(type="0"),
          ir_influenza_60 %>% mutate(type="60"),
          ir_influenza_all_hist %>% mutate(type="All")) %>%
    mutate(year_months=paste0(calendar_year, "-", calendar_month)) %>%
  ggplot(aes(group=type, colour=type))+
  geom_point(aes(year_months, ir),
              position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=year_months, ymin=ir_low, ymax=ir_high),
                 position=position_dodge(width=0.5))
















study_pops<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2012-01-01"),
                         study_end_date=as.Date("2014-12-31"),
                         study_age_stratas = list(c(10,15), c(16,20), c(10,20)),
                         study_sex_stratas = c("Male", "Female", "Both"),
                         study_days_prior_history =c(0,365),
                         verbose = TRUE)
tictoc::toc()

study_pops %>%
  group_by(cohort_definition_id,
           age_strata, sex_strata,
           required_days_prior_history) %>%
  tally() %>%
  mutate(cohort_definition_id=as.numeric(cohort_definition_id)) %>%
  arrange(cohort_definition_id)



ir<-calculate_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome="cohorts",
                                    cohort_id_outcome=1,
                                    study_denominator_pop=study_pops,
                                    cohort_id_denominator_pop="17",
                                    time_interval=c("Months"),
                                    prior_event_lookback=NULL,
                                    repetitive_events=FALSE,
                                    confidence_intervals="exact",
                                    verbose=FALSE)

ir2<-calculate_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome="cohorts",
                                    cohort_id_outcome=1,
                                    study_denominator_pop=study_pops,
                                    cohort_id_denominator_pop="17",
                                    time_interval=c("Years"),
                                    prior_event_lookback=NULL,
                                    repetitive_events=FALSE,
                                    confidence_intervals="exact",
                                    verbose=FALSE)

# covid -------
denominator_pop_covid<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2020-03-01"),
                         study_end_date=as.Date("2021-04-01"),
                         study_age_stratas = NULL,
                         study_sex_stratas = "Both",
                         study_days_prior_history =365,
                         verbose = TRUE)

ir_covid<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=4,
                        study_denominator_pop=denominator_pop_covid,
                        cohort_id_denominator_pop="1",
                        time_interval=c("Months"),
                        prior_event_lookback=21,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=TRUE)
ir_covid_repetitive<-get_pop_incidence(db=db,
                        results_schema_outcome="results21t2_test",
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=4,
                        study_denominator_pop=denominator_pop_covid,
                        cohort_id_denominator_pop="1",
                        time_interval=c("Months"),
                        prior_event_lookback=21,
                        repetitive_events=TRUE,
                        confidence_interval="exact",
                        verbose=TRUE)

plot_data<-bind_rows(ir_covid %>% mutate(type="Without repetitive events"),
          ir_covid_repetitive %>% mutate(type="With repetitive events")) %>%
    mutate(year_months=paste0(calendar_year, "-", calendar_month))
lev<-unique(plot_data$year_months)

plot_data %>%
  mutate(year_months=factor(year_months,
                               levels=lev)) %>%
  ggplot(aes(group=type, colour=type))+
  geom_point(aes(year_months, ir),
              position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=year_months, ymin=ir_low, ymax=ir_high),
                 position=position_dodge(width=0.5))


# how does this compare to what we would expect (in general)
# based on the outcome table start dates
a<-outcome_db %>%
  filter(cohort_id_outcome=="4") %>%
  select(cohort_start_date) %>%
  collect()

a %>%
  ggplot() +
  geom_histogram(aes(cohort_start_date), binwidth = 3)+
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
             date_labels = "%B")





