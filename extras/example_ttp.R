# packages -----
library(SqlRender)
library(DatabaseConnector)
library(CohortGenerator)
library(here)
library(RPostgreSQL)
library(DBI)
library(dbplyr)
library(dplyr)


# library(CirceR)
# library(FeatureExtraction)
# library(lubridate)
# library(stringr)
# library(ggplot2)
# library(tidyr)
# library(kableExtra)
# library(RSQLite)
# library(rmarkdown)
# library(tableone)
# library(scales)
# library(forcats)
# library(epiR)
# library(readxl)
# library(lubridate)
# library(readxl)
# library(dtplyr)
devtools::load_all()


# database ----
#usethis::edit_r_environ()

server<-Sys.getenv("SERVER_FEB22")
user<-Sys.getenv("DB_USER_FEB22")
password<- Sys.getenv("DB_PASSWORD_FEB22")
port<-Sys.getenv("DB_PORT_FEB22")
host<-Sys.getenv("DB_HOST_FEB22")
connectionDetails <-DatabaseConnector::downloadJdbcDrivers("postgresql", here::here())
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "postgresql",
                                                                server =server,
                                                                user = user,
                                                                password = password,
                                                                port = port ,
                                                                pathToDriver = here::here())
db <-DBI::dbConnect(RPostgres::Postgres(),
                    dbname = Sys.getenv("SERVER_DBI_FEB22"),
                    port = port,
                    host = host,
                    user = user,
                    password = password)


targetDialect <-"postgresql"
cdm_database_schema<-"omop21t2_cmbd"
vocabulary_database_schema<-"omop21t2_cmbd"
results_database_schema<-"results21t2_cmbd"

outcomecohortTableStem<-"EB_OmopPopEpi_itp"

# build results cohorts -----
cohortJsonFiles <- list.files(here("extras", "outcome_cohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,"immune_thrombocytopenia")]

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


# ttp ----
itp_outcome_id<-cohortDefinitionSet %>%
  filter(cohortName=="immune_thrombocytopenia") %>%
  select(cohortId) %>%
  pull()


collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2017-01-01"),
                         study_end_date=as.Date("2018-01-01"),
                         study_age_stratas = list(c(0,5),
                                                  c(6,17)),
                         study_sex_stratas = c("Male", "Female"),
                         verbose = TRUE)

denominator_pop_itp<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2017-01-01"),
                         study_end_date=as.Date("2018-01-02"),
                         study_age_stratas = list(c(0,5),
                                                  c(6,17),
                                                  c(18,34),
                                                  c(35,54),
                                                  c(55,64),
                                                  c(65,74),
                                                  c(75,84),
                                                  c(85,114)),
                         study_sex_stratas = c("Male", "Female"),
                         verbose = TRUE)
itp<-get_pop_incidence(db=db,
                        results_schema_outcome=results_database_schema,
                        table_name_outcome=outcomecohortTableStem,
                        cohort_id_outcome=itp_outcome_id,
                        study_denominator_pop=denominator_pop_itp,
                        cohort_id_denominator_pop=1,
                        time_interval=c("Years"),
                        outcome_washout_window=365,
                        repetitive_events=FALSE,
                        confidence_interval="exact",
                        verbose=TRUE)
itp %>%
  filter(calendar_year=="2017")

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
                      outcome_washout_windows=NULL,
                      repetitive_events=FALSE,
                      confidence_intervals="exact",
                      verbose = FALSE)%>%
  group_by(cohort_definition_id)%>%
  filter(calendar_year=="2014") %>%
  summarise(person_days=sum(person_days),
            person_months=sum(person_months),
            person_years=sum(person_years),
            n_events=sum(n_events))
