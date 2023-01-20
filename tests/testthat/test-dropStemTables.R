test_that("multiplication works", {
  cdm <- mockIncidencePrevalenceRef(sampleSize = 10000)
  attr(cdm, "write_schema") <- "main"

  cdm$dpop <- generateDenominatorCohortSet(
    cdm = cdm,
    ageGroup = list(
      c(0, 18),
      c(19, 60)
    ),
    daysPriorHistory = c(0, 1, 2),
    tablePrefix =  "result"
  )
  inc <- estimateIncidence(
    cdm = cdm,
    denominatorTable = "dpop",
    outcomeTable = "outcome",
    interval = "overall",
    tablePrefix =  "result",
    returnParticipants = TRUE
  )

  # we have the stem tables in our write schema
  expect_true(any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
      schema = attr(cdm, "write_schema")
    ),
    "result"
  )))

  dropStemTables(cdm, computePermanentStem = "result")
  # and now we don´t
  expect_false(any(stringr::str_detect(
    CDMConnector::listTables(attr(cdm, "dbcon"),
      schema = attr(cdm, "write_schema")
    ),
    "result"
  )))

  expect_error(dropStemTables("cdm", computePermanentStem = "result"))
  expect_error(dropStemTables(cdm, computePermanentStem = 1))
  expect_error(dropStemTables(cdm,
    computePermanentStem = c("result", "result1")
  ))

  DBI::dbDisconnect(attr(cdm, "dbcon"), shutdown = TRUE)
})
