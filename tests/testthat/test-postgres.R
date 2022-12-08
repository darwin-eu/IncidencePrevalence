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
                                                daysPriorHistory = c(0,180),
                                                sample = 10000,
                                                verbose = TRUE)
cdm$outcome <- cdm$denominator %>% head(100)

pont_prev <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  verbose = TRUE
)
period_prev <- estimatePointPrevalence(
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
inc2 <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator",
  outcomeTable = "outcome",
  outcomeWashout = 180,
  verbose = TRUE
)

results<-gatherResults(resultList=list(pont_prev, period_prev, inc, inc2),
                       outcomeCohortName="test_sample",
                       outcomeCohortId = 1,
                       databaseName="test_database")
expect_true(all(names(results)==c("prevalence_estimates",
                                  "incidence_estimates")))

})
