
sqlAddYears <- function(dialect, yearsToAdd, variable) {
  errorMessage <- checkmate::makeAssertCollection()
  yearCheck <- (yearsToAdd %% 1 == 0)
  checkmate::assertTRUE(yearCheck,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  renderedTranslatedSql <- SqlRender::translate(
    SqlRender::render("DATEADD(year, @yearsToAdd, @variable)",
      yearsToAdd = yearsToAdd,
      variable = variable
    ),
    targetDialect = dialect
  )
  return(renderedTranslatedSql)
}

sqlAddDays <- function(dialect, daysToAdd, variable) {
  errorMessage <- checkmate::makeAssertCollection()
  daysCheck <- (daysToAdd %% 1 == 0)
  checkmate::assertTRUE(daysCheck,
    add = errorMessage
  )
  checkmate::reportAssertions(collection = errorMessage)

  renderedTranslatedSql <- SqlRender::translate(
    SqlRender::render("DATEADD(day, @daysToAdd, @variable)",
      daysToAdd = daysToAdd,
      variable = variable
    ),
    targetDialect = dialect
  )
  return(renderedTranslatedSql)
}

extractQuery <- function(query, description = "") {
  sql <- dbplyr::sql_render(query)
  sql <- gsub("\"", "", sql)
  sql <- rbind(
    paste0("< SQL ", description, ">"),
    sql, " "
  )
  return(sql)
}
