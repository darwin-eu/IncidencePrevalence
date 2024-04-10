
# utility function to support metaprogramming with date
addDaysQuery <- function(cdm,
                         variable, # name of the variable to use
                         number,  # number of days or years, use a negative number to subtract da
                         type, # must be year or day
                         name_style){ # note, absolute values of numbers will be used

  if(!type %in% c("day", "year")){
    cli::cli_abort("type must be day or year")
  }

  if(type == "day"){
    if(omopgenerics::cdmSourceType(cdm) == c("duckdb")){
      q <-  glue::glue("{variable} + days({number})")
    }  else {
      q <- glue::glue("add_days({variable} , as.integer({(number)}))")
    }}

  if(type == "year"){
    if(omopgenerics::cdmSourceType(cdm) == c("duckdb")){
      q <-  glue::glue("{variable} + years({number})")
    } else {
      q <- glue::glue("add_years({variable} , as.integer({(number)}))")
    }}

  q %>%
    rlang::parse_exprs() %>%
    rlang::set_names(glue::glue(name_style))
}

minusDaysQuery <- function(cdm,
                         variable, # name of the variable to use
                         number,  # number of days or years, use a negative number to subtract da
                         type, # must be year or day
                         name_style = NULL, # note, absolute values of numbers will be used
                         names = NULL){  # alternative to name_style, set of names

  if(!type %in% c("day", "year")){
    cli::cli_abort("type must be day or year")
  }

  if(type == "day"){
    if(omopgenerics::cdmSourceType(cdm) == c("duckdb")){
      q <-  glue::glue("{variable} - days({number})")
    }  else {
      q <- glue::glue("add_days({variable} , as.integer({(number)}))")
    }}

  if(type == "year"){
    if(omopgenerics::cdmSourceType(cdm) == c("duckdb")){
      q <-  glue::glue("{variable} - years({number})")
    } else {
      q <- glue::glue("add_years({variable} , as.integer({(number)}))")
    }}

  q<-q %>%
    rlang::parse_exprs()

  if(!is.null(name_style)){
    q<-q %>%
      rlang::set_names(glue::glue(name_style))
  }

  if(!is.null(names)){
    q<-q %>%
      rlang::set_names(names)
  }

  q

}






