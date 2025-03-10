bench <- list()
# postgres ----
db <- DBI::dbConnect(RPostgres::Postgres(),
  dbname = Sys.getenv("CDM5_POSTGRESQL_DBNAME"),
  host = Sys.getenv("CDM5_POSTGRESQL_HOST"),
  user = Sys.getenv("CDM5_POSTGRESQL_USER"),
  password = Sys.getenv("CDM5_POSTGRESQL_PASSWORD")
)
cdm <- CDMConnector::cdmFromCon(
  con = db, cdmName = "ohdsi_postgres",
  cdmSchema = Sys.getenv("CDM5_POSTGRESQL_CDM_SCHEMA"),
  writeSchema = c(schema = Sys.getenv("CDM5_POSTGRESQL_SCRATCH_SCHEMA")),
  writePrefix = "incp_"
)
bench[["ohdsi_postgres"]] <- benchmarkIncidencePrevalence(cdm)
omopgenerics::cdmDisconnect(cdm)

# redshift ----
db <- DBI::dbConnect(RPostgres::Redshift(),
  dbname   = Sys.getenv("CDM5_REDSHIFT_DBNAME"),
  host     = Sys.getenv("CDM5_REDSHIFT_HOST"),
  port     = Sys.getenv("CDM5_REDSHIFT_PORT"),
  user     = Sys.getenv("CDM5_REDSHIFT_USER"),
  password = Sys.getenv("CDM5_REDSHIFT_PASSWORD")
)
cdm <- CDMConnector::cdmFromCon(
  con = db, cdmName = "ohdsi_redshift",
  cdmSchema = Sys.getenv("CDM5_REDSHIFT_CDM_SCHEMA"),
  writeSchema = c(schema = Sys.getenv("CDM5_REDSHIFT_SCRATCH_SCHEMA")),
  writePrefix = "incp_", cdmVersion = "5.3"
)
bench[["ohdsi_redshift"]] <- benchmarkIncidencePrevalence(cdm)
omopgenerics::cdmDisconnect(cdm)

# sql server ----
db <- DBI::dbConnect(odbc::odbc(),
  Driver = Sys.getenv("SQL_SERVER_DRIVER"),
  Server = Sys.getenv("CDM5_SQL_SERVER_SERVER"),
  Database = Sys.getenv("CDM5_SQL_SERVER_CDM_DATABASE"),
  UID = Sys.getenv("CDM5_SQL_SERVER_USER"),
  PWD = Sys.getenv("CDM5_SQL_SERVER_PASSWORD"),
  TrustServerCertificate = "yes",
  Port = Sys.getenv("CDM5_SQL_SERVER_PORT")
)
a <- strsplit(Sys.getenv("CDM5_SQL_SERVER_CDM_SCHEMA"), "\\.")[[1]]
b <- strsplit(Sys.getenv("CDM5_SQL_SERVER_SCRATCH_SCHEMA"), "\\.")[[1]]
cdm <- CDMConnector::cdmFromCon(
  con = db, cdmName = "ohdsi_sql_Server",
  cdmSchema = c(catalog = a[1], schema = a[2]),
  writeSchema = c(catalog = b[1], schema = b[2])
)
bench[["ohdsi_sql_server"]] <- benchmarkIncidencePrevalence(cdm)
omopgenerics::cdmDisconnect(cdm)

# snowflake ----
con <- DBI::dbConnect(odbc::odbc(),
  SERVER = Sys.getenv("SNOWFLAKE_SERVER"),
  UID = Sys.getenv("SNOWFLAKE_USER"),
  PWD = Sys.getenv("SNOWFLAKE_PASSWORD"),
  DATABASE = Sys.getenv("SNOWFLAKE_DATABASE"),
  WAREHOUSE = Sys.getenv("SNOWFLAKE_WAREHOUSE"),
  DRIVER = Sys.getenv("SNOWFLAKE_DRIVER")
)

cdm <- CDMConnector::cdmFromCon(
  con = con, cdmName = "ohdsi_snowflake",
  cdmSchema = strsplit(Sys.getenv("SNOWFLAKE_CDM_SCHEMA"), "\\.")[[1]],
  writeSchema = strsplit(Sys.getenv("SNOWFLAKE_SCRATCH_SCHEMA"), "\\.")[[1]],
  writePrefix = "incp_"
)
bench[["ohdsi_snowflake"]] <- benchmarkIncidencePrevalence(cdm)
omopgenerics::cdmDisconnect(cdm)

# spark ----
con <- DBI::dbConnect(
  odbc::databricks(),
  httpPath = Sys.getenv("DARWIN_DATABRICKS_HTTPPATH"),
  workspace = Sys.getenv("DARWIN_DATABRICKS_HOST"),
  useNativeQuery = FALSE,
  uid = Sys.getenv("DARWIN_DATABRICKS_USER"),
  pwd = Sys.getenv("DARWIN_DATABRICKS_TOKEN")
)
cdmSchema <- Sys.getenv("DARWIN_DATABRICKS_CDM_SCHEMA")
writeSchema <- Sys.getenv("DARWIN_DATABRICKS_SCRATCH_SCHEMA")
cdm <- CDMConnector::cdmFromCon(con, cdmSchema, writeSchema,
                                cdmName = "darwin_databricks_spark")
bench[["darwin_databricks_spark"]] <- benchmarkIncidencePrevalence(cdm)
omopgenerics::cdmDisconnect(cdm)

# add existing results ----
# cprd_gold <- omopgenerics::importSummarisedResult(
#   here::here("data-raw", "results_CPRD GOLD_2025_03_03.csv"))
# IncidencePrevalenceBenchmarkResults <- bind(IncidencePrevalenceBenchmarkResults, cprd_gold)
# save -----
IncidencePrevalenceBenchmarkResults <- bind(bench)
usethis::use_data(IncidencePrevalenceBenchmarkResults,
  internal = FALSE,
  overwrite = TRUE
)
