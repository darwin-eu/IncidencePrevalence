
sql_add_years<-function(dialect, years_to_add, variable){

  error_message <- checkmate::makeAssertCollection()
  year_check<-(years_to_add%%1==0)
  checkmate::assertTRUE(year_check,
                         add = error_message
  )
  checkmate::reportAssertions(collection = error_message)

  rendered_translated_sql <- SqlRender::translate(
    SqlRender::render("DATEADD(year, @years_to_add, @variable)",
           years_to_add=years_to_add,
           variable=variable),
    targetDialect = dialect)
  return(rendered_translated_sql)
}

sql_add_days<-function(dialect, days_to_add, variable){

  error_message <- checkmate::makeAssertCollection()
  days_check<-(days_to_add%%1==0)
  checkmate::assertTRUE(days_check,
                        add = error_message
  )
  checkmate::reportAssertions(collection = error_message)

  rendered_translated_sql <- SqlRender::translate(
    SqlRender::render("DATEADD(day, @days_to_add, @variable)",
                      days_to_add=days_to_add,
                      variable=variable),
    targetDialect = dialect)
  return(rendered_translated_sql)
}
