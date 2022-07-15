# packages -----
library(SqlRender)
library(DatabaseConnector)
library(CohortGenerator)
library(here)
library(RPostgreSQL)
library(DBI)
library(dbplyr)
library(dplyr)
library(CirceR)
library(stringr)


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


# plot functions ------

theme_darwin_facet <- function(reference_size = 15) {
    ggplot2::theme(panel.border =ggplot2::element_rect(color = "black")) +
    # facets
    ggplot2::theme(strip.text = ggplot2::element_text(size = 14, face = "bold")) +
    ggplot2::theme(strip.text.y.left = ggplot2::element_text(angle = 0)) +
    ggplot2::theme(strip.text.y.right = ggplot2::element_text(angle = 0)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "#f7f7f7")) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = reference_size)) +
    ggplot2::theme(panel.spacing.x = unit(0.25, "lines")) +
    ggplot2::theme(panel.spacing.y = unit(0.25, "lines"))
}

theme_darwin <- function(reference_size = 15) {

  # colors
  color.border <- "#ffffff"
  color.background <- "#ffffff"
  color.grid.major <- "#d9d9d9"
  color.axis.text <- "#252525"
  color.axis.title <- "#252525"

  # theme
  ggplot2::theme_bw(base_size = reference_size) +
    # background
    ggplot2::theme(panel.background = ggplot2::element_rect(fill = color.background, color = color.background)) +
    ggplot2::theme(plot.background = ggplot2::element_rect(fill = color.background, color = color.background)) +
    ggplot2::theme(panel.border = ggplot2::element_rect(color = color.border)) +
    # grid
    ggplot2::theme(panel.grid.major = ggplot2::element_line(color = color.grid.major, size = .25)) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank()) +
    # axis
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = reference_size, color = color.axis.text)) +
    ggplot2::theme(axis.text.y = ggplot2::element_text(size = reference_size, color = color.axis.text)) +
    ggplot2::theme(axis.title.x = ggplot2::element_text(
      size = reference_size, color = color.axis.title,
      vjust = 0
    )) +
    ggplot2::theme(axis.title.y = ggplot2::element_text(
      size = reference_size, color = color.axis.title,
      vjust = 1.25
    )) +
    ggplot2::theme(axis.ticks = ggplot2::element_blank()) +
    # legend
    ggplot2::theme(legend.background = ggplot2::element_rect(fill = color.background)) +
    ggplot2::theme(legend.text = element_text(size = reference_size, color = color.axis.title)) +
    ggplot2::theme(legend.title = element_blank()) +
    ggplot2::theme(legend.position = "top") +
    # margins
    ggplot2::theme(plot.margin = grid::unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

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
cohortJsonFiles <- list.files(here("inst", "outcome_cohorts"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,"covid")]
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,"reinfection", negate = TRUE)]
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
working.json<-here("inst", "outcome_cohorts",
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


# covid hosp ----
covid_outcome_id<-cohortDefinitionSet %>%
  filter(cohortName=="covid_hospitalisation") %>%
  select(cohortId) %>%
  pull()

denominator_pop<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2020-03-01"),
                         study_end_date=as.Date("2021-03-01"),
                         study_age_stratas = list(c(0,39),
                                                  c(40,64),
                                                  c(65,79),
                                                  c(80,150)),
                         study_sex_stratas = c("Male", "Female"),
                         verbose = TRUE)
ir <- collect_pop_incidence(db,
                        results_schema_outcome=results_database_schema,
                                  cohort_ids_outcomes=covid_outcome_id,
                                  study_denominator_pop=denominator_pop,
                                  cohort_ids_denominator_pops=unique(denominator_pop$cohort_definition_id),
                                  time_intervals = c("Months"),
                                  outcome_washout_windows = NULL,
                                  repetitive_events = FALSE,
                                  verbose = FALSE)


plot_data<-ir %>%
    mutate(year_months=paste0(calendar_year, "-", calendar_month))
lev<-unique(plot_data$year_months)

plot_data<- plot_data %>%
    mutate(year_months=factor(year_months,
                               levels=lev))

plot_data<- plot_data %>%
  mutate(age_plot=ifelse(age_strata=="0;39", "<40",
                  ifelse(age_strata=="40;64", "40 to 64",
                  ifelse(age_strata=="65;79", "65 to 79",
                  ifelse(age_strata=="80;150", ">=80",
                         NA))))) %>%
  mutate(age_plot=factor(age_plot,
                               levels=c("<40",
                                        "40 to 64",
                                        "65 to 79",
                                        ">=80")))

plot_data %>%
  ggplot(aes(group=sex_strata,
             colour=sex_strata))+
  facet_grid(age_plot ~ .) +
  geom_point(aes(year_months, ir),
             size=2,
              position=position_dodge(width=0.5))+
  geom_errorbar(aes(x=year_months, ymin=ir_low, ymax=ir_high),
                width=0,
                 position=position_dodge(width=0.5))+
  theme_darwin()+
  theme_darwin_facet()+
  ylab("Incidence rate (per 100,000 PYs)")+
  xlab("")
ggsave("covid_hospitalisation_ir.png",
       width=12, height=7)






# covid diag or test ----
covid_outcome_id<-cohortDefinitionSet %>%
  filter(cohortName=="covid_diagnosis_or_test_positive") %>%
  select(cohortId) %>%
  pull()

denominator_pop<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=as.Date("2020-02-01"),
                         study_end_date=as.Date("2021-05-01"),
                         verbose = TRUE)
ir <- collect_pop_incidence(db,
                        results_schema_outcome=results_database_schema,
                                  cohort_ids_outcomes=covid_outcome_id,
                                  study_denominator_pop=denominator_pop,
                                  cohort_ids_denominator_pops=unique(denominator_pop$cohort_definition_id),
                                  time_intervals = c("Months"),
                                  outcome_washout_windows = NULL,
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



