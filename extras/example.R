# packages
library(SqlRender)
library(DatabaseConnector)
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
devtools::load_all()

db <-DBI::dbConnect(RPostgres::Postgres(),
                    dbname = Sys.getenv("SERVER_DBI_TEST"),
                    port = Sys.getenv("DB_PORT_TEST"),
                    host = Sys.getenv("DB_HOST_TEST"),
                    user = Sys.getenv("DB_USER_TEST"),
                    password = Sys.getenv("DB_PASSWORD_TEST"))
cdm_database_schema<-"omop21t2_test"


# db <-DBI::dbConnect(RPostgres::Postgres(),
#                     dbname = Sys.getenv("SERVER_DBI_FEB22"),
#                     port = Sys.getenv("DB_PORT_FEB22"),
#                     host = Sys.getenv("DB_HOST_FEB22"),
#                     user = Sys.getenv("DB_USER_FEB22"),
#                     password = Sys.getenv("DB_PASSWORD_FEB22"))
# cdm_database_schema<-"omop21t2_cmbd"

tictoc::tic()
study_pop1<-get_denominator_pop(db,
                    cdm_database_schema,
                    start_date = as.Date("2012-01-01"),
                    end_date = as.Date("2018-01-01"),
                    min_age = 10,
                    max_age = 15,
                    sex = c("Male"),
                    days_prior_history = 365,
                    verbose = TRUE)
tictoc::toc()

study_pop2<-get_denominator_pop(db,
                    cdm_database_schema,
                    verbose = TRUE)
tictoc::tic()
study_pops<-collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=NULL,
                         study_end_date=NULL,
                         age_groups=list(c(10,15), c(16,20), c(10,20)),
                         sex=c("Male", "Female", "Both"),
                         days_prior_history=365,
                         verbose = TRUE)
tictoc::toc()

study_pops %>%
  group_by(cohort_definition_id, min_age, max_age, sex) %>%
  tally()


# do we get the same people from both?
table(get_denominator_pop(db,
                    cdm_database_schema,
                    min_age = 10,
                    max_age = 15,
                    sex = c("Male"),
                    days_prior_history = 365) %>%
  select(person_id) %>%
  pull() ==
collect_denominator_pops(db,
                         cdm_database_schema,
                         study_start_date=NULL,
                         study_end_date=NULL,
                         age_groups=list(c(10,15)),
                         sex="Male",
                         days_prior_history=365) %>%
  select(person_id) %>%
  pull())



