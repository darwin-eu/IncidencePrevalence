test_that("postgres test", {

skip_if(Sys.getenv("DB_SERVER_cdmgold202007_dbi") == "")

db <- DBI::dbConnect(RPostgres::Postgres(),
                  dbname = Sys.getenv("DB_SERVER_cdmgold202007_dbi"),
                  port = Sys.getenv("DB_PORT") ,
                  host = Sys.getenv("DB_HOST") ,
                  user = Sys.getenv("DB_USER"),
                  password = Sys.getenv("DB_PASSWORD"))
cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = "public",
                                    write_schema = "results")
cdm$denominator <- generateDenominatorCohortSet(cdm = cdm,
                                                sample = 1000)
cdm$outcome <- cdm$denominator %>% head(100)

prev <- estimatePrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  verbose = TRUE
)
inc <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  verbose = TRUE
)



})
