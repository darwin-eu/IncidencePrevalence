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


db <-DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                    dbname = Sys.getenv("SERVER_DBI_TEST"),
                    port = Sys.getenv("DB_PORT_TEST"),
                    host = Sys.getenv("DB_HOST_TEST"),
                    user = Sys.getenv("DB_USER_TEST"),
                    password = Sys.getenv("DB_PASSWORD_TEST"))
cdm_database_schema<-"omop21t2_test"


get_denominator_pop(db,
                    cdm_database_schema,
                    start_date = NULL,
                    end_date = NULL,
                    min_age = NULL,
                    max_age = NULL,
                    sex = c("Male", "Female"),
                    days_prior_history = NULL,
                    verbose = TRUE)
