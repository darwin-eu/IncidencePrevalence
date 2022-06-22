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

outcomecohortTableStem<-"EB_OmopPopEpi_covid"

# build results cohorts -----
cohortJsonFiles <- list.files(here("extras", "outcome_cohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,"covid")]
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,"reinfection", negate = TRUE)]
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


# covid ----
covid_outcome_id<-cohortDefinitionSet %>%
  filter(cohortName=="sars_cov_2_pcr_positive_test") %>%
  select(cohortId) %>%
  pull()

denominator_pop<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2020-02-01"),
                         study_end_date=as.Date("2021-05-01"),
                         study_age_stratas = list(c(0,17),
                                                  c(18,39),
                                                  c(40,59),
                                                  c(60,69),
                                                  c(70,79),
                                                  c(80,150)),
                         study_sex_stratas = c("Male", "Female"),
                         verbose = TRUE)
ir <- collect_pop_incidence(db,
                        results_schema_outcome=results_database_schema,
                                  cohort_ids_outcomes=covid_outcome_id,
                                  study_denominator_pop=denominator_pop,
                                  cohort_ids_denominator_pops=unique(denominator_pop$cohort_definition_id),
                                  time_intervals = c("Months"),
                                  prior_event_lookbacks = NULL,
                                  repetitive_events = FALSE,
                                  verbose = FALSE)


plot_data<-ir %>%
    mutate(year_months=paste0(calendar_year, "-", calendar_month))
lev<-unique(plot_data$year_months)

plot_data<- plot_data %>%
    mutate(year_months=factor(year_months,
                               levels=lev))

plot_data<- plot_data %>%
  mutate(age_strata=factor(age_strata,
                               levels=c("0;17",
                                        "18;39",
                                        "40;59",
                                        "60;69",
                                        "70;79",
                                        "80;150") ))

plot_data %>%
  ggplot(aes(group=sex_strata,
             colour=sex_strata))+
  facet_grid(age_strata ~ .) +
  geom_point(aes(year_months, ir),
              position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=year_months, ymin=ir_low, ymax=ir_high),
                width=0,
                 position=position_dodge(width=0.5))+
  scale_y_log10()

